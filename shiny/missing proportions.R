# install.packages('remotes')
# detach("package:ohdsilab", unload = TRUE)
remotes::install_github("roux-ohdsi/ohdsilab")
library(ohdsilab)

bookstore(c("tidyverse", "DescTools", "gtsummary", "marginaleffects"))

dat  <- read_csv(here::here("data", "fake_dat.csv"))

fi_var_names <- c("activities", 
"emotional.7", "fatigue.7", "gen.health", "gen.mental.health", 
"gen.social", "health.material.help_b", "pain.7", "social.satis", 
"all.blind", "diff.bathing", "diff.concentration", "diff.errands", 
"diff.walk.climb", "af", "all.heart", "anxiety", "arthritis", "asthma", "cancer", 
"chronic.lung", "con.heart.fail", "dementia", "depression", "diabetes", 
"fractured.bone", "hypertension", "kidney", "osteoporosis", "per.vas", 
"stroke.tia", "hearing.impairment", "transportation")

n_items <- length(fi_var_names)

dat_to_pred <- dat %>%
  mutate(gender_cat = fct_lump_n(gender, n = 3),
         age_cat = cut(age, breaks = c(seq(50, 80, by = 10), Inf), 
                       labels = c('50-59','60-69','70-79','80+'), right = FALSE)
  ) %>%
  select(all_of(fi_var_names), ends_with("_skip"), gender_cat, age, age_cat, race_eth_cat, income_cat, edu_cat, 
         alcohol_cat, smoking_cat, missing_group) %>%
  mutate(
      across(ends_with("cat"), ~replace_na(.x, "Missing")),
      across(all_of(fi_var_names), ~ifelse(.x %in% c("Skipped item", "Missing entire survey"), .x, "Observed item"),
             .names = "{col}_skip"),
      across(all_of(fi_var_names), ~na_if(.x, "Skipped item")),
      across(all_of(fi_var_names), ~na_if(.x, "Missing entire survey")),
      across(all_of(fi_var_names), parse_number),
      across(all_of(fi_var_names), ~as.numeric(is.na(.x)), .names = "{col}_missing"),
      across(all_of(fi_var_names), ~replace_na(.x, 0), .names = "{.col}_0"),
      denominator = n_items - (cancer_missing + hypertension_missing + per.vas_missing + stroke.tia_missing + af_missing + dementia_missing + con.heart.fail_missing +
                                 all.heart_missing + transportation_missing + hearing.impairment_missing + diff.concentration_missing +
                                 diff.bathing_missing + diff.errands_missing + diff.walk.climb_missing + diabetes_missing + kidney_missing + anxiety_missing +
                                 depression_missing + fatigue.7_missing + pain.7_missing + emotional.7_missing + activities_missing + gen.health_missing +
                                 gen.mental.health_missing + gen.social_missing + health.material.help_b_missing + social.satis_missing + asthma_missing +
                                 chronic.lung_missing + fractured.bone_missing + arthritis_missing + osteoporosis_missing + all.blind_missing),
      numerator = cancer_0 + hypertension_0 + per.vas_0 + stroke.tia_0 + af_0 + dementia_0 + con.heart.fail_0 +
        all.heart_0 + transportation_0 + hearing.impairment_0 + diff.concentration_0 +
        diff.bathing_0 + diff.errands_0 + diff.walk.climb_0 + diabetes_0 + kidney_0 + anxiety_0 +
        depression_0 + fatigue.7_0 + pain.7_0 + emotional.7_0 + activities_0 + gen.health_0 +
        gen.mental.health_0 + gen.social_0 + health.material.help_b_0 + social.satis_0 + asthma_0 +
        chronic.lung_0 + fractured.bone_0 + arthritis_0 + osteoporosis_0 + all.blind_0,
      fi = numerator / denominator,
      fi_cat = cut(fi, c(0, .15, .25, 1), right = FALSE, labels = c("Robust", "Pre-frail", "Frail")),
      is_fi_complete = denominator == n_items,
      is_fi_gt80pct = denominator >= n_items * .8
  ) %>%
  select(-ends_with("_0"))

missing_groups <- dat_to_pred

pred_deficit_prop <- function(deficit) {
  mod <- lm(as.formula(paste0(deficit, " ~ gender_cat + poly(age, 2) + race_eth_cat + income_cat + edu_cat + alcohol_cat + smoking_cat")),
    data = dat_to_pred, subset = missing_group == "complete")
  dat_to_pred %>% 
    filter(!if_any(all_of(paste0(deficit, "_skip")), ~.x %in% c("Missing entire survey"))) %>% 
    group_by(across(all_of(c("missing_group", paste0(deficit, "_skip"))))) %>%
    group_modify(~avg_predictions(mod, newdata = .x)) %>% 
    rename("skipped" = paste0(deficit, "_skip")) %>% 
    mutate(deficit = deficit)
}

map_dfr(fi_var_names, pred_deficit_prop) %>%
  write_rds("pred_deficits.rds")

group_count <- function(.data, v, ...) {
  .data %>% 
    group_by(...) %>% 
    group_modify(~count(.x, across(all_of(v)))) %>% 
    pivot_wider(names_from = all_of(v), values_from = n) %>% 
    rename_with(.fn = ~paste(v, .x, sep = "_"), .cols = -group_cols())
}

all_counts <- map(paste0(fi_var_names, "_missing"),
                  ~group_count(select(missing_groups, missing_group, ends_with("missing")), .x, missing_group)) %>%
  reduce(left_join, by = "missing_group")

props_skipped_per_missing_group <- all_counts %>%
  rename_with(~str_remove(.x, "_missing"), everything()) %>%
  pivot_longer(-missing_group, names_to = c(".value", "value"), names_pattern = "(.+)_(.+)$") %>%
  pivot_longer(-c(missing_group, value), names_to = "variable", values_to = "n") %>%
  group_by(missing_group, variable) %>%
  mutate(total = sum(n, na.rm = TRUE),
       prop = n / total)

props_skipped_per_missing_group %>%
  filter(n >= 20) %>%
  write_rds("skipped_per_missing_group.rds")

get_multinom_cis <- function(data, value, count) {
  values <- data[[value]]
  counts <- data[[count]]
  
  DescTools::MultinomCI(counts) %>% 
    as_tibble() %>% 
    mutate(value = values,
           count = counts)
}
 
get_skip_cis <- function(data, ...) {
  numbers <- data %>% 
    group_by(skip) %>% 
    summarise(count = sum(count)) %>% 
    pull(count) %>% 
    rev() %>% 
    binom.test() %>% 
    `[`(c("estimate", "conf.int")) %>% 
    flatten_dbl() %>% 
    set_names(c("est", "lwr.ci", "upr.ci"))
  
  tibble(!!!numbers)
}


all_vals <- map(fi_var_names, 
                  ~group_count(select(missing_groups, missing_group, all_of(fi_var_names)), .x, missing_group)) %>% 
  reduce(left_join, by = "missing_group")

vals_per_missing_group <- all_vals %>%
  pivot_longer(-missing_group, names_to = c(".value", "value"), names_pattern = "(.+)_(.+)$") %>%
  pivot_longer(-c(missing_group, value), names_to = "variable", values_to = "n") %>%
#   filter(value != "NA")%>% # keep skips in for now
  group_by(missing_group, variable) %>%
  mutate(total = sum(n, na.rm = TRUE),
       prop = n / total) %>% 
  filter(!is.na(n))

vals_per_missing_group %>% 
  filter(n >= 20) %>%
  write_rds("vals_per_missing_group.rds")

get_mean_ci <- function(deficit) {
  dat_to_pred %>% 
    filter(!if_any(all_of(paste0(deficit, "_skip")), ~.x %in% c("Skipped item", "Missing entire survey"))) %>% 
    group_by(across(all_of(c("missing_group", paste0(deficit, "_skip"))))) %>%
    summarise(across(all_of(deficit), ~broom::tidy(t.test(.x))), .groups = "drop") %>% 
    unnest(all_of(deficit)) %>% 
    rename("skipped" = paste0(deficit, "_skip")) %>% 
    mutate(deficit = deficit)
}

map_dfr(fi_var_names, get_mean_ci) %>%
  write_rds("prop_cis.rds")

# cis <- vals_per_missing_group %>% 
#   filter(!is.na(total)) %>% 
#   select(-prop) %>%
#   group_by(missing_group, total, variable, skip = value == "NA") %>% 
#   filter(!skip, n < total) %>%
#   group_modify(~get_multinom_cis(.x, "value", "n")) %>% 
#   ungroup()
# 
# cis %>% 
#   filter(count >= 20) %>%
#   write_rds("prop_cis.rds")

table_dat <- missing_groups %>%
  mutate(fi_cat = ifelse(is_fi_gt80pct, as.character(fi_cat), NA),
         age_cat = as.character(age_cat)) %>%
  # select(ends_with("_skip"), ends_with("_cat"), missing_group) %>%
  mutate(across(ends_with("cat"), ~na_if(as.character(.x), "Skip")),
    across(ends_with("cat"), ~na_if(as.character(.x), "Missing")),
    across(ends_with("cat"), fct_drop),
  fi_cat = fct_relevel(fi_cat, "Robust", "Pre-frail", "Frail"),
  age_cat = fct_relevel(age_cat, "50-59", "60-69", "70-79", "80+"),
  gender_cat = fct_infreq(gender_cat),
  race_eth_cat = fct_infreq(race_eth_cat),
  income_cat = fct_relevel(income_cat, "< 50k", "50-100k", "> 100k"),
  edu_cat = fct_relevel(edu_cat, "Less than high school", "High school graduate", "College graduate"),
  alcohol_cat = fct_relevel(alcohol_cat, "Never", "Monthly Or Less",  "Weekly Or More"))

axe_env <- function(x, verbose = FALSE, ...) {
  as.function(c(formals(x), body(x)), env = environment(x))
}

axe_subs <- function(tab) { 
    tab$`_substitutions`[[1]]$func <- map(tab$`_substitutions`[[1]]$func, axe_env) 
    return(tab)
}

small_cells <- function(df) {
  df$n[between(df$n, 1, 19)] <- NA
  df$p[between(df$n, 1, 19)] <- NA
}

make_comparison_table <- function(item, table_dat) {
  tab <- table_dat %>%
    filter(missing_group == "complete",
           !if_any(all_of(item), is.na)) %>% 
    tbl_summary(include = c(age_cat, gender_cat, race_eth_cat,
                            income_cat, edu_cat, smoking_cat, alcohol_cat,
                            fi_cat), missing_text = "Missing",
                type = all_dichotomous() ~ "categorical",
                label = list(fi_cat ~ "Frailty index (if <20% missing)",
                             age_cat ~ "Age",
                             gender_cat ~ "Gender",
                             race_eth_cat ~ "Race/ethnicity",
                             income_cat ~ "Income",
                             edu_cat ~ "Education",
                             alcohol_cat ~ "Alcohol",
                             smoking_cat ~ "Smoking")
               ) %>%
    bold_labels() %>% 
    modify_table_body(~.x %>% mutate(across(starts_with("stat"), ~ifelse(.x == "0 (NA%)", NA, .x)))) %>% 
    modify_footnote(everything() ~ NA)
  
  tab$meta_data$df_stats <- map(tab$meta_data$df_stats, small_cells)
  tab$inputs$data <- NULL
  tab <- tab %>% as_gt()

  tab
}

tables <- map(fi_var_name %>% s, make_comparison_table, table_dat = table_dat)
names(tables) <- fi_var_names

table_data <- map(tables, ~.x$`_data`)
table_boxhead <- map(tables, ~.x$`_boxhead`)

make_comparison_table("cancer", table_dat = table_dat %>% slice_sample(n = 1000)) %>%
  write_rds("comparison_table.rds")

table_data %>%
  write_rds("comparison_table_data.rds")

table_boxhead %>%
  write_rds("comparison_table_boxhead.rds")
