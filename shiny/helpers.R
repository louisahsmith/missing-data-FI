# create tables from parts
recreate_table <- function(data, boxhead, tab) {
  tab$`_data` <- data
  tab$`_boxhead` <- boxhead
  tab
}

# use coalesce with selecting functions
new_coalesce <- function(.cols, ...) {
  df <- across({{ .cols }}, .fns = identity)
  classes <- sapply(df, class)
  if (length(unique(classes)) > 1) df <- mutate(df, across(everything(), as.character))
  df <- mutate(df, .res = coalesce(!!!syms(names(df))))
  pull(df, .res)
}

# Load and preprocess your health data
load_health_data <- function() {
  # Load your data here
  comparison_table_data <- read_rds("comparison_table_data.rds")
  comparison_table_boxhead <- read_rds("comparison_table_boxhead.rds")
  comparison_table <- read_rds("comparison_table.rds") %>%
    tab_options(table.font.size = px(12),
                data_row.padding = px(0),
                data_row.padding.horizontal = px(5)) 
  all_tabs <- map2(comparison_table_data, comparison_table_boxhead, recreate_table, tab = comparison_table)
  
  prop_cis <- read_rds("prop_cis.rds")
  pred_deficits <- read_rds("pred_deficits.rds")
  # %>% 
  #   ungroup() %>%
  #   mutate(skipped = new_coalesce(ends_with("_skip")),
  #          deficit = factor(deficit, labels = unique(prop_cis$variable))) %>% 
  #   select(-ends_with("_skip"))  |> 
  #   filter(skipped != "Missing entire survey")
  
  # return a list with 'characteristics' and 'data'
  list(characteristics = names(all_tabs), 
       data = list(all_tabs = all_tabs, 
                   prop_cis = prop_cis,
                   pred_deficits = pred_deficits))
}

# Filter the data based on the selected characteristic
filter_data_by_characteristic <- function(healthData, characteristicIndex) {
  characteristic <- names(healthData$data$all_tabs)[characteristicIndex]
  # Filter your data based on the selected characteristic
  filteredData <- list(tab = healthData$data$all_tabs[[characteristic]], 
                       dat = filter(healthData$data$prop_cis, deficit == characteristic),
                       pred = filter(healthData$data$pred_deficits, deficit == characteristic),
                       characteristic = characteristic,
                       characteristicIndex = characteristicIndex)
  
  # group_descriptions <- data.frame(missing_group = c("complete", "missing difficulties", "missing comorbidities", 
  #                                                    "missing difficulties/comorbidities"),
  #                                  description = c("Completed all surveys", "Missing The Basics Disability", 
  #                                                  "Missing Personal Medical History",
  #                                                  "Missing The Basics Disability & Personal Medical History"))
  
  filteredData$pred <- filteredData$pred %>%
    # left_join(group_descriptions, by = "missing_group") |> 
    mutate(description = ifelse(missing_group == "complete", "Completed all surveys", "Missing survey(s)"),
      new_description = paste(description, tolower(skipped), sep = ", "))
  
  filteredData$dat <- filteredData$dat %>%
    # left_join(group_descriptions, by = "missing_group") %>%
    left_join(labels(), by = join_by(deficit == characteristic)) %>% 
    mutate(description = ifelse(missing_group == "complete", "Completed all surveys", "Missing survey(s)"),
           new_description = paste0(description, ", observed item"),
           skipped = "Observed item"
           )
  # ,
  #          description = fct_expand(description, "Skipped item", paste0("Missing ", survey.category)))
  
  filteredData
}

# Create the characteristic plot
create_characteristic_plot <- function(filteredData, minValueSvy, avgValueSvy, maxValueSvy, minValueQ, avgValueQ, maxValueQ) {
  
  # if (filteredData$characteristic == "hearing.impairment") {
  #   filteredData$dat <- filter(filteredData$dat, missing_group != "missing comorbidities")
  # }
  
  if (filteredData$dat$prop[1] == 0) {
    # plot_dat <- filteredData$dat |> 
    #   group_by(new_description, skipped, description) |> 
    #   summarise(est = weighted.mean(parse_number(value), w = est),
    #             lwr.ci = weighted.mean(parse_number(value), w = lwr.ci), # not correct but all I can do for now
    #             upr.ci = weighted.mean(parse_number(value), w = upr.ci), .groups = "drop")
    ylab <- "Avg value (scaled Likert score)"
  } else {
    # plot_dat <- filter(filteredData$dat, value == "1")
    ylab <- "Proportion with characteristic"
  }
  
  # Function to check if the input is numeric and non-empty
  is_numeric_nonempty <- function(x) {
    return(!is.null(x) && nzchar(x) && !is.na(suppressWarnings(as.numeric(x))))
  }
  
  # Add the point with error bars only if minValue, avgValue, and maxValue are not NULL, non-empty, and numeric
  
  if (is_numeric_nonempty(minValueSvy) && is_numeric_nonempty(avgValueSvy) && is_numeric_nonempty(maxValueSvy) &&
      is_numeric_nonempty(minValueQ  ) && is_numeric_nonempty(avgValueQ  ) && is_numeric_nonempty(maxValueQ  )) {
    # minValueSvy <- 0
    # avgValueSvy <- 0
    # maxValueSvy <- 0
    # minValueQ <- 0
    # avgValueQ <- 0
    # maxValueQ <- 0
    
    
    filteredData$dat$type <- "Observed"
    filteredData$pred$type <- "Predicted"

    plot_dat <- bind_rows(filteredData$dat, filteredData$pred) |> 
      mutate(skipped = fct_recode(skipped, "Missing item" = "Skipped item"))
    
    acc <- ifelse(min(plot_dat$estimate) < .01, .001, .01)
    
    new_dat <- data.frame(estimate = c(avgValueQ, avgValueSvy),
                          description = c("Completed all surveys", "Missing survey(s)"),
                          skipped = c("Missing item", "Missing item"),
                          type = c("Your estimate", "Your estimate"),
                          # description = factor(levels(factor(filteredData$dat$description))[!levels(factor(filteredData$dat$description)) %in% unique(filteredData$dat$description)], 
                          #                      levels = levels(filteredData$dat$description)),
                          conf.low = c(minValueQ, minValueSvy), 
                          conf.high = c(maxValueQ, maxValueSvy))
    plot_dat_combined <- bind_rows(plot_dat, new_dat) |> 
      # filter(skipped %in% c("Observed item", "Missing item")) |> 
      mutate(type = fct_relevel(type, "Your estimate", "Observed", "Predicted"),
             type = fct_recode(type, 
                               "Prediction from model of those w/ complete data\n(prediction interval)" = "Predicted",
                               "Your estimate\n(reasonable range)" = "Your estimate",
                               "Actual estimate\n(95% CI)" = "Observed"),
             skipped = fct_relevel(skipped, "Observed item", "Missing item"),
             label = str_glue("{scales::number(estimate, accuracy = acc)} ({scales::number(conf.low, accuracy = acc)}, {scales::number(conf.high, accuracy = acc)})"))
    
    p <- ggplot(plot_dat_combined, 
                aes(x = 1, y = estimate, 
                    ymin = conf.low, ymax = conf.high,
                    color = type, label = label)) +
      geom_point(position = position_dodge(width = 0.25)) +
      geom_errorbar(width = 0.2, 
                    position = position_dodge(width = 0.25)) +
      geom_text(aes(y = conf.high, group = type), vjust = -.5, color = "black",
                position = position_dodge(width = 0.25)) +
      scale_y_continuous(expand = expansion(mult = c(.05, .10))) +
      theme_minimal() +
      facet_grid(cols = vars(description), rows = vars(skipped)) +
      labs(y = ylab, x = NULL, color = NULL) +
      theme(
        text = element_text(size = 18),
        axis.text.x = element_blank(),
        strip.background = element_rect(colour = "grey", fill = "grey"),
        panel.border = element_rect(fill = NA, color = "grey"),
        # panel.background = element_blank(),
        # panel.grid.major.y = element_line(color = "grey"),
        # panel.grid.minor.y = element_line(color = "grey", linetype = "dotted"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "top"
      )
    # p

    # %>% 
    #   filter(description != "Missing The Basics")
    # p <- p +
    #   geom_point(data = new_dat, aes(color = type), size = 3, position = position_dodge(width = 0.25)) +
    #   geom_errorbar(data = new_dat,aes(color = type), width = 0.2, position = position_dodge(width = 0.25)) +
    #   # scale_x_discrete(labels = scales::label_wrap(15)) +
    #   theme(
    #     text = element_text(size = 18)
    #   )
    # p
  }
  return(p)
}

# Create the characteristic table
create_characteristic_table <- function(filteredData) {
  # Create a table with demographic characteristics
  filteredData$tab %>% 
    gt::tab_header(title = paste0("Descriptive statistics for those with complete data and observed '", tolower(filteredData$dat$label[1]), "' item"))
}

# Save the input values for the current characteristic
save_input_values <- function(filteredData, minValueSvy, avgValueSvy, maxValueSvy, minValueQ, avgValueQ, maxValueQ, inputValues) {
  newEntry <- data.frame(characteristicIndex = filteredData$characteristicIndex,
                         characteristic = filteredData$characteristic, 
                         minValueSvy = minValueSvy, 
                         avgValueSvy = avgValueSvy, 
                         maxValueSvy = maxValueSvy, 
                         minValueQ = minValueQ, 
                         avgValueQ = avgValueQ,
                         maxValueQ = maxValueQ,
                         stringsAsFactors = FALSE)
  
  # If the characteristic is not already in the data frame, add the new entry
  if (!filteredData$characteristic %in% inputValues()$characteristic) {
    inputValues(rbind(inputValues(), newEntry))
  } else {
    # If the characteristic is already in the data frame, update the existing entry
    updatedValues <- inputValues()
    updatedValues[updatedValues$characteristic == filteredData$characteristic, ] <- newEntry
    inputValues(updatedValues)
  }
}

# Submit all inputs to Dropbox
submit_all_inputs_to_dropbox <- function(inputValues) {
  # Use Dropbox API to upload the CSV file
  fileName <- paste0("FI_sens", str_remove_all(Sys.time(), "[\\s\\-\\:A-Z]"), ".csv")
  
  filePath <- file.path(tempdir(), fileName)
  
  write_csv(inputValues, filePath, na = "")
  
  drop_upload(filePath)
}

labels <- function() {
  data.frame(
    characteristic = c("activities","af","all.blind",
                       "all.heart","anxiety","arthritis","asthma","cancer",
                       "chronic.lung","con.heart.fail","dementia",
                       "depression","diabetes","diff.bathing","diff.concentration",
                       "diff.errands","diff.walk.climb","emotional.7",
                       "fatigue.7","fractured.bone","gen.health","gen.mental.health",
                       "gen.social","health.material.help_b",
                       "hearing.impairment","hypertension","kidney","osteoporosis","pain.7",
                       "per.vas","social.satis","stroke.tia","transportation"),
    label = c("Can't complete everyday activities", 
              "Atrial fibrillation", "Vision impairment", "Coronary Artery Disease", 
              "Anxiety", "Arthritis", "Asthma", "Cancer", "Lung disease", "Heart failure", 
              "Dementia", "Depression", "Diabetes", "Difficulty bathing", "Difficulty Concentrating", 
              "Difficulty with errands", "Difficulty walking/stairs", "Emotional problems", 
              "Fatigue", "Fractured bone", "Poor general health", "Poor mental health", 
              "Poor social health", "Poor health literacy", "Hearing impairment", 
              "Hypertension", "Kidney disease", "Osteoporosis", "Pain", "Peripheral vascular disease", 
              "Poor social satisfaction", "Stroke / TIA", "Delayed care due to transporation"),
    prop = c(0, 
              1, 1, 1, 
              1, 1, 1, 1, 1, 1, 
              1, 1, 1, 1, 1, 
              1, 1, 0, 
              0, 1, 0, 0, 
              0, 0, 1, 
              1, 1, 1, 0, 1, 
              0, 1, 1),
    survey.category = c("The Basics",
                        "Personal Medical History","The Basics","Personal Medical History",
                        "Personal Medical History","Personal Medical History",
                        "Personal Medical History","Personal Medical History",
                        "Personal Medical History","Personal Medical History",
                        "Personal Medical History","Personal Medical History",
                        "Personal Medical History","The Basics Disability",
                        "The Basics Disability","The Basics Disability",
                        "The Basics Disability","The Basics","The Basics",
                        "Personal Medical History","The Basics","The Basics",
                        "The Basics","The Basics", "Personal Medical History","Personal Medical History",
                        "Personal Medical History","Personal Medical History",
                        "The Basics","Personal Medical History","The Basics",
                        "Personal Medical History",
                        "The Basics"),
    actual.survey = c("Overall Health",
                      "Personal Medical History","The Basics","Personal Medical History",
                      "Personal Medical History","Personal Medical History",
                      "Personal Medical History","Personal Medical History",
                      "Personal Medical History","Personal Medical History",
                      "Personal Medical History","Personal Medical History",
                      "Personal Medical History","The Basics",
                      "The Basics","The Basics",
                      "The Basics","Overall Health","Overall Health",
                      "Personal Medical History","Overall Health","Overall Health",
                      "Overall Health","Overall Health","The Basics",
                      "Personal Medical History","Personal Medical History",
                      "Personal Medical History","Overall Health",
                      "Personal Medical History","Overall Health","Personal Medical History",
                      "Healthcare Access and Utilization")
  )
}

get_label <- function(characteristic, val = "label") {
  labels()[labels()$characteristic == characteristic, val]
}





