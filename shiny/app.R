library(shiny)
library(tidyverse)
library(gt)
library(shinythemes)
library(rdrop2)
# token <- drop_auth()
token <- readRDS("tokenfile.rds")
token$refresh()
# drop_auth(cache = FALSE, rdstoken = "tokenfile.rds")

source("helpers.R")

# User Interface
ui <- shinyUI(
  fluidPage(
    theme = shinytheme("yeti"),
    includeCSS("custom.css"),
    tags$script('
      Shiny.addCustomMessageHandler("updateProgress", function(message) {
        $("#progressContainer").html(message);
      });
    '),
    titlePanel("Sensitivity values for missing items in AoU frailty index"),
    fluidRow(
      column(1,
             p(em("Progress")),
             style = "padding-bottom:0"
      ),
      column(11,
             div(id = "progressContainer", style = "width: 55%;"),
             style = "padding:0;height:10px;"
      )
    ),
    fluidRow(
      column(
        7,
        h3(textOutput("currentCharacteristic")),
        plotOutput("characteristicPlot"),
        fluidRow(
          column(10,
                 uiOutput("dynamicInputs")
          ),
          column(2,
                 fluidRow(br()),
                 fluidRow(
                   uiOutput("backButton")
                 ),
                 fluidRow(br()),
                 fluidRow(
                   uiOutput("submitButton")
                 ),
                 fluidRow(br()),
                 fluidRow(
                   uiOutput("submitAllButton")
                 )
          )
          # fluidRow(
          #     column(
          #         3,
          #         uiOutput("backButton")
          #     ),
          #     column(
          #         3,
          #         uiOutput("submitButton")
          #     ),
          #     column(
          #         3,
          #         uiOutput("submitAllButton")
          #         # conditionalPanel(condition = "output.submitAllVisible == true",
          #         #                  actionButton("submitAll", "Submit"))
          #     )          
        )
      ),
      column(
        5,
        tabsetPanel(
          tabPanel(title = "Table",
                   gt_output("characteristicTable")
          ),
          tabPanel(title = "Help!",
                   markdown("
                          #### What is the goal of this exercise?
                          
                          The goal is to determine plausible values for missing data in the All of Us frailty index.
                          AoU participants are missing data on the deficits include in the frailty index because they 
                          chose to skip a given question, or because they didn't fill out an entire survey.
                          We think that those whose deficits are missing are different from those whose data we have, 
                          even after accounting for demographic factors like age, gender, and race/ethnicity.
                          By asking experts for realistc ranges for those missing data, we can conduct a sensitivity 
                          analysis to see how far off our naive results might be.
                          
                          #### How do I read the figure?
                          
                          The four panels in the figure represent four 'types' of AoU participants (clockwise from top left):
                          
                          - those who filled out all the surveys, including the question on the deficit of interest
                          - those missing some surveys but who did fill out the question on the deficit of interest
                          - those missing some surveys without data on the deficit of interest
                          - those who filled out the surveys but chose to skip the question on the deficit of interest
                          
                          In green are observed means/proportions for that 'type' of person, which of course
                          can only be computed for those with the item observed. In blue are predictions from
                          a model fit on those with complete surveys only, which reflect what we'd expect from a sample
                          with the same distribution of demographic characteristics as we see in that 'type'. The difference
                          between the green and blue points among those with missing survey data but observed data on that deficit
                          reflects *unmeasured* differences among those who filled out all the surveys and those who didn't.
                          
                          #### What do I do?
                          
                          Your job is to move the sliders in the left panel to encompass a realistic range for the average 
                          value of the deficit, as well as a 'best guess' somewhere in that range.
                          The red points on the figure will move to show how your choices compare to the observed 
                          and model-predicted data.
                          Differences between your range, in red, and the model prediction, in blue, represent the *unmeasured*
                          differences in that deficit that you think might be present among those who didn't answer questions about 
                          that deficit and those who did.
                          Once you go through all 33 deficits and press 'Submit', your data will be uploaded and 
                          used in a future sensitivity analysis.
                          ")
          )
        )
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  
  # Load data
  healthData <- load_health_data()
  
  # Create a reactive value for the current characteristic index
  currentCharacteristicIndex <- reactiveVal(1)
  
  # Send the current characteristic to the UI
  output$currentCharacteristic <- reactive({ get_label(healthData$characteristics[currentCharacteristicIndex()]) })
  
  output$submitAllButton <- renderUI({
    if (currentCharacteristicIndex() == 33) {
      actionButton("submitAll", "Submit")
    }
  })
  
  output$backButton <- renderUI({
    if (currentCharacteristicIndex() > 1) {
      actionButton("back", "Back")    }
  })
  
  output$submitButton <- renderUI({
    if (currentCharacteristicIndex() < 33) {
      actionButton("submit", "Next")    }
  })
  
  # Function to update the progress bar
  update_progress_bar <- function(session, progress) {
    # Calculate the progress percentage
    progress_percent <- round(progress / 33 * 100, 0)
    
    # Create the progress bar
    progress_bar <- sprintf('
    <div class="progress" style="height: 30px;">
      <div class="progress-bar progress-bar-info" role="progressbar" aria-valuenow="%s" aria-valuemin="0" aria-valuemax="100" style="width: %s%%;">
       %s%% </div>
    </div>
  ', progress_percent, progress_percent, progress_percent)
    
    # Update the progress bar in the UI
    session$sendCustomMessage(type = "updateProgress", message = progress_bar)
  }
  
  observe({
    update_progress_bar(session, currentCharacteristicIndex())
  })
  
  # Reactive: Filter the data based on the selected characteristic
  filteredData <- reactive({
    filter_data_by_characteristic(healthData, currentCharacteristicIndex())
  })
  
  output$dynamicInputs <- renderUI({
    
    nextSvy <- filteredData()$pred |> 
      filter(skipped == "Skipped item", missing_group == "missing survey(s)")
    nextQ <- filteredData()$pred |> 
      filter(skipped == "Skipped item", missing_group == "complete")
    
    svy <- list(
      fluidRow(
        h4("Your estimated range for participants missing entire surveys:")
      ),
      column(6,
             sliderInput(
               inputId = "minmaxValueSvy",
               label = "Range:",
               min = 0,
               max = 1,
               value = c(max(nextSvy$conf.low - .05, 0), nextSvy$conf.high + 0.05),
               step = .001
             )
      ),
      column(6,
             sliderInput(
               inputId = "avgValueSvy",
               label = "Best guess:",
               min = 0,
               max = 1,
               value = nextSvy$estimate,
               step = .001
             )
      )
      # fluidRow(
      #     column(
      #         3,
      #         numericInput("minValueSvy", "Minimum:", 0)
      #     ),
      #     column(
      #         3,
      #         numericInput("avgValueSvy", "Best guess:", 0)
      #     ),
      #     column(
      #         3,
      #         numericInput("maxValueSvy", "Maximum:", 0)
      #     )
      # )
    )
    q <- list(
      fluidRow(
        h4("Your estimated range for the participants skipping this question:")
      ),
      fluidRow(
        column(6,
               sliderInput(
                 inputId = "minmaxValueQ",
                 label = "Range:",
                 min = 0,
                 max = 1,
                 value = c(max(nextQ$conf.low - .05, 0), nextQ$conf.high + 0.05),
                 step = .001
               )
        ),
        column(6,
               sliderInput(
                 inputId = "avgValueQ",
                 label = "Best guess:",
                 min = 0,
                 max = 1,
                 value = nextQ$estimate,
                 step = .001
               )
        )
        # column(
        #     3,
        #     numericInput("minValueQ", "Minimum:", 0)
        # ),
        # column(
        #     3,
        #     numericInput("avgValueQ", "Best guess:", 0)
        # ),
        # column(
        #     3,
        #     numericInput("maxValueQ", "Maximum:", 0)
        # )
      ))
    # if (filteredData()$dat$survey.category[1] == "The Basics")  return(q)
    
    list(svy, q)
    
  })
  
  # Create a reactive data frame for storing input values
  inputValues <- reactiveVal(data.frame(characteristicIndex = numeric(), characteristic = character(),
                                        minValueSvy = numeric(), avgValueSvy = numeric(), maxValueSvy = numeric(),
                                        minValueQ   = numeric(), avgValueQ   = numeric(), maxValueQ   = numeric()))
  
  # Render the plot
  output$characteristicPlot <- renderPlot({
    create_characteristic_plot(filteredData(), input$minmaxValueSvy[1], input$avgValueSvy, input$minmaxValueSvy[2], 
                               input$minmaxValueQ[1], input$avgValueQ, input$minmaxValueQ[2])
  })
  
  # Render the table
  output$characteristicTable <- render_gt({
    create_characteristic_table(filteredData())
  })
  
  # Handle the submit button
  observeEvent(input$submit, {
    # Save the input values for the current characteristic
    save_input_values(filteredData(), input$minmaxValueSvy[1], input$avgValueSvy, input$minmaxValueSvy[2], 
                      input$minmaxValueQ[1], input$avgValueQ, input$minmaxValueQ[2], inputValues)
    
    
    
    nextData <- filter_data_by_characteristic(healthData, currentCharacteristicIndex() + 1)
    
    nextSvy <- nextData$pred |> 
      filter(skipped == "Skipped item", missing_group == "missing survey(s)")
    nextQ <- nextData$pred |> 
      filter(skipped == "Skipped item", missing_group == "complete")

    # Reset the numeric inputs to 0
    updateNumericInput(session, "minmaxValueSvy", value = c(max(nextSvy$conf.low - .05, 0), nextSvy$conf.high + 0.05))
    updateNumericInput(session, "avgValueSvy", value = nextSvy$estimate)
    # updateNumericInput(session, "maxValueSvy", value = 0)
    updateNumericInput(session, "minmaxValueQ", value = c(max(nextQ$conf.low - .05, 0), nextQ$conf.high + 0.05))
    updateNumericInput(session, "avgValueQ", value = nextQ$estimate)
    # updateNumericInput(session, "maxValueQ", value = 0)
    
    # Increment the currentCharacteristicIndex
    if (currentCharacteristicIndex() < 33) {
      currentCharacteristicIndex(currentCharacteristicIndex() + 1)
    }
    
  })
  
  # Handle the back button
  observeEvent(input$back, {
    
    # Decrement the currentCharacteristicIndex
    if (currentCharacteristicIndex() > 1) {
      currentCharacteristicIndex(currentCharacteristicIndex() - 1)
      
      # Get the previously stored values for the characteristic
      prev_vals <- inputValues()[inputValues()$characteristicIndex == currentCharacteristicIndex(),]
      prev_min_svy <- prev_vals$minValueSvy
      prev_avg_svy <- prev_vals$avgValueSvy
      prev_max_svy <- prev_vals$maxValueSvy
      prev_min_q <- prev_vals$minValueQ
      prev_avg_q <- prev_vals$avgValueQ
      prev_max_q <- prev_vals$maxValueQ
      
      # Update the input values
      updateNumericInput(session, "minmaxValueSvy", value = c(prev_min_svy, prev_max_svy))
      updateNumericInput(session, "avgValueSvy", value = prev_avg_svy)
      # updateNumericInput(session, "maxValueSvy", value = 0)
      updateNumericInput(session, "minmaxValueQ", value = c(prev_min_q, prev_max_q))
      updateNumericInput(session, "avgValueQ", value = prev_avg_q)
      # updateNumericInput(session, "maxValueQ", value = 0)
      
      # updateNumericInput(session, "minValueSvy", value = prev_min_svy)
      # updateNumericInput(session, "avgValueSvy", value = prev_avg_svy)
      # updateNumericInput(session, "maxValueSvy", value = prev_max_svy)
      # updateNumericInput(session, "minValueQ", value = prev_min_q)
      # updateNumericInput(session, "avgValueQ", value = prev_avg_q)
      # updateNumericInput(session, "maxValueQ", value = prev_max_q)
    }
  })
  
  # Handle the submit all button
  observeEvent(input$submitAll, {
    # Save the input values for the last characteristic
    # save_input_values(filteredData(), input$minValueSvy, input$avgValueSvy, input$maxValueSvy, 
    #                   input$minValueQ, input$avgValueQ, input$maxValueQ, inputValues)
    save_input_values(filteredData(), input$minmaxValueSvy[1], input$avgValueSvy, input$minmaxValueSvy[2], 
                      input$minmaxValueQ[1], input$avgValueQ, input$minmaxValueQ[2], inputValues)
    # Reset the currentCharacteristicIndex to 1
    currentCharacteristicIndex(1)
    
    # Reset the numeric inputs to 0
    updateNumericInput(session, "minmaxValueSvy", value = c(0, .5))
    updateNumericInput(session, "avgValueSvy", value = 0.25)
    # updateNumericInput(session, "maxValueSvy", value = 0)
    updateNumericInput(session, "minmaxValueQ", value = c(0, .5))
    updateNumericInput(session, "avgValueQ", value = 0.25)
    # updateNumericInput(session, "maxValueQ", value = 0)
    # updateNumericInput(session, "minValueSvy", value = 0)
    # updateNumericInput(session, "avgValueSvy", value = 0)
    # updateNumericInput(session, "maxValueSvy", value = 0)
    # updateNumericInput(session, "minValueQ", value = 0)
    # updateNumericInput(session, "avgValueQ", value = 0)
    # updateNumericInput(session, "maxValueQ", value = 0)
    
    # Submit all inputs to Dropbox
    submit_all_inputs_to_dropbox(inputValues())
  })
}

shinyApp(ui, server)