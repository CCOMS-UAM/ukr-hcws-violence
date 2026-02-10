#### "Mental health of nurses and doctors in Ukraine... ####
# we recommend running this is a fresh R session or restarting your current session

# cmdstanr is a backend engine for R tu run Bayesian Models.
# This next lines should only be run once.

# install.packages(
#   "cmdstanr",
#   repos = c('https://stan-dev.r-universe.dev',
#             getOption("repos"))
#   )

# Required packages -------------------------------------------------------

library(broom.mixed)
library(broom)
library(gt)
library(lme4)
library(geepack)
library(gtsummary)
library(knitr)
library(officer)
library(survey)
library(emmeans)
library(glue)
library(brms)
library(tidybayes)
library(sandwich)
library(lmtest)
library(cobalt)
library(modelsummary)
library(cmdstanr)
library(gtExtras)
library(posterior)

## Functions ## 


# Generate unweighted tables ----------------------------------------------


## Outcome prevalences with CI by scdm ------------------------------------
# Calculation of unweighted rows

calc_unweighted_row_out <- 
  function(data, outcome_var) {
  
  # Clean vector
  vec <- data[[outcome_var]]
  vec <- vec[!is.na(vec)] # Remove NAs
  
  n_total <- length(vec)
  
  # Placeholders if data is empty (prevents errors)
  if (n_total == 0) return("NA (NA-NA)")
  
  n_yes <- sum(vec == 1)
  
  # CI using prop.test (Wilson score is standard for props)
  ptest <- prop.test(n_yes, n_total, conf.level = 0.95)
  
  est <- ptest$estimate * 100
  ci_low <- ptest$conf.int[1] * 100
  ci_high <- ptest$conf.int[2] * 100
  
  sprintf("%.1f%% (%.1f-%.1f)", est, ci_low, ci_high)
}



# Loop through variables to calculate row for each subgroup

get_unweighted_stats_out <- 
  function(subset_df, outcome) {
  
  # Overall Row
  overall_val <- calc_unweighted_row_out(subset_df, outcome)
  
  overall_row <- tibble(
    Variable = "Overall",
    Category = "Overall",
    Prevalence = overall_val
  )
  
  # Loop through scdm variables
  rows_stats <- map_dfr(scdm_vars, function(var) {
    
    # Label 
    var_lbl <- tryCatch(attr(subset_df[[var]], "label"), 
                        error = function(e) var)
    
    if(is.null(var_lbl)) var_lbl <- var
    
    # group by the variable categories and calculate stats
    subset_df |>
      filter(!is.na(.data[[var]])) |> # remove missing categories
      group_by(Category = .data[[var]]) |>
      summarise(
        Prevalence = calc_unweighted_row_out(pick(everything()), outcome)
      ) |>
      mutate(Variable = var_lbl) |>
      select(Variable, Category, Prevalence)
  })
  
  # Combine
  bind_rows(overall_row, rows_stats)
}


# Build unweighted table

generate_unweighted_table_out <- 
  function(data, outcome_var, outcome_title) {
  
  # Subsets 
  d_m_doc <- 
    data |> 
    filter(scdm_2_rec == "Male", 
           work_2 == "Doctor")
  
  d_m_nur <- 
    data |> 
    filter(scdm_2_rec == "Male", 
           work_2 == "Nurse")
  
  d_f_doc <- 
    data |> 
    filter(scdm_2_rec == "Female",    
           work_2 == "Doctor")
  
  d_f_nur <- 
    data |> 
    filter(scdm_2_rec == "Female",    
           work_2 == "Nurse")
  
  d_all_doc <- 
    data |> filter(work_2 == "Doctor")
  
  d_all_nur <- 
    data |> filter(work_2 == "Nurse")
  
  # Ns for headers
  get_n <- function(df) {
    
    df |> 
      filter(!is.na(.data[[outcome_var]])) |> 
      nrow() |> 
      style_number()
  }
  
  n_m_doc <- get_n(d_m_doc)
  n_m_nur <- get_n(d_m_nur)
  n_f_doc <- get_n(d_f_doc)
  n_f_nur <- get_n(d_f_nur)
  n_all_doc <- get_n(d_all_doc)
  n_all_nur <- get_n(d_all_nur)
  
  # Calculate Stats
  res_m_doc <- 
    get_unweighted_stats_out(d_m_doc, outcome_var) |> 
    rename(M_Doc = Prevalence)
  
  res_m_nur <- 
    get_unweighted_stats_out(d_m_nur, outcome_var) |> 
    rename(M_Nur = Prevalence)
  
  res_f_doc <- 
    get_unweighted_stats_out(d_f_doc, outcome_var) |> 
    rename(F_Doc = Prevalence)
  
  res_f_nur <- 
    get_unweighted_stats_out(d_f_nur, outcome_var) |> 
    rename(F_Nur = Prevalence)
  
  res_all_doc <- 
    get_unweighted_stats_out(d_all_doc, outcome_var) |> 
    rename(All_Doc = Prevalence)
  
  res_all_nur <- 
    get_unweighted_stats_out(d_all_nur, outcome_var) |> 
    rename(All_Nur = Prevalence)
  
  # merge
  final_df <- 
    
    res_m_doc |>
    left_join(res_f_doc, by = c("Variable", "Category")) |>
    left_join(res_all_doc, by = c("Variable", "Category")) |>
    left_join(res_m_nur, by = c("Variable", "Category")) |>
    left_join(res_f_nur, by = c("Variable", "Category")) |> 
    left_join(res_all_nur, by = c("Variable", "Category")) 
  
  # gt table
  final_df |>
    gt(groupname_col = "Variable", 
       rowname_col = "Category") |>
    cols_label(
      Category = md("**Subgroup**"),
      M_Doc = md(glue("**Male**<br>(N = {n_m_doc})")),
      M_Nur = md(glue("**Male**<br>(N = {n_m_nur})")),
      All_Doc = md(glue("**Overall**<br>(N = {n_all_doc})")),
      F_Doc = md(glue("**Female**<br>(N = {n_f_doc})")),
      F_Nur = md(glue("**Female**<br>(N = {n_f_nur})")),
      All_Nur = md(glue("**Overall**<br>(N = {n_all_nur})"))
    ) |>
    tab_spanner(
      label = md("**Doctor**"),
      columns = c(M_Doc, F_Doc, All_Doc)
    ) |>
    tab_spanner(
      label = md("**Nurse**"),
      columns = c(M_Nur, F_Nur, All_Nur)
    ) |>
    tab_header(
      title = md(glue("**{outcome_title} (Unweighted)**"))
    ) |>
    cols_align(align = "center", 
               columns = contains("_")) |>
    tab_stub_indent(rows = Category != "Overall", 
                    indent = 3) |>
    tab_footnote(footnote = "Unweighted Prevalence (95% CI).")
}




# Exposure prevalences by scdm --------------------------------------------
# Get unweighted rows

calc_unweighted_row_exp <- function(data, exposure_var) {
  
  # clean vector
  vec <- data[[exposure_var]]
  vec <- vec[!is.na(vec)] # Remove NAs
  
  n_total <- length(vec)
  
  # placeholders if data is empty (prevents errors)
  if (n_total == 0) return("NA (NA-NA)")
  
  n_yes <- sum(vec == "Yes")
  
  # CI using prop.test (Wilson score is standard for props)
  ptest <- prop.test(n_yes, n_total, conf.level = 0.95)
  
  est <- ptest$estimate * 100
  ci_low <- ptest$conf.int[1] * 100
  ci_high <- ptest$conf.int[2] * 100
  
  sprintf("%.1f%% (%.1f-%.1f)", est, ci_low, ci_high)
}

# Loop through variables to generate calculation by subgroups
get_unweighted_stats_exp <- function(subset_df, exposure) {
  
  # Overall Row
  overall_val <- calc_unweighted_row_exp(subset_df, exposure)
  
  overall_row <- tibble(
    Variable = "Overall",
    Category = "Overall",
    Prevalence = overall_val
  )
  
  # Loop through scdm variables
  rows_stats <- map_dfr(scdm_vars, function(var) {
    
    # Label 
    var_lbl <- tryCatch(attr(subset_df[[var]], "label"), 
                        error = function(e) var)
    
    if(is.null(var_lbl)) var_lbl <- var
    
    # group by the variable categories and calculate stats
    subset_df |>
      filter(!is.na(.data[[var]])) |> # remove missing categories
      group_by(Category = .data[[var]]) |>
      summarise(
        Prevalence = calc_unweighted_row_exp(pick(everything()), exposure)
      ) |>
      mutate(Variable = var_lbl) |>
      select(Variable, Category, Prevalence)
  })
  
  # Combine
  bind_rows(overall_row, rows_stats)
}

# Build table
generate_unweighted_table_exp <- function(data, exposure_var, exposure_title) {
  
  # Subsets 
  d_m_doc <- 
    data |> 
    filter(scdm_2_rec == "Male", 
           work_2 == "Doctor")
  
  d_m_nur <- 
    data |> 
    filter(scdm_2_rec == "Male", 
           work_2 == "Nurse")
  
  d_f_doc <- 
    data |> 
    filter(scdm_2_rec == "Female",    
           work_2 == "Doctor")
  
  d_f_nur <- 
    data |> 
    filter(scdm_2_rec == "Female",    
           work_2 == "Nurse")
  
  d_all_nur <- 
    data |> 
    filter(work_2 == "Nurse")
  
  d_all_doc <- 
    data |> 
    filter(work_2 == "Doctor")
  
  # Ns for headers
  get_n <- function(df) {
    
    df |> 
      filter(!is.na(.data[[exposure_var]])) |> 
      nrow() |> 
      style_number()
  }
  
  n_m_doc <- get_n(d_m_doc)
  n_m_nur <- get_n(d_m_nur)
  n_f_doc <- get_n(d_f_doc)
  n_f_nur <- get_n(d_f_nur)
  n_all_doc <- get_n(d_all_doc)
  n_all_nur <- get_n(d_all_nur)
  
  # Calculate stats
  res_m_doc <- 
    get_unweighted_stats_exp(d_m_doc, exposure_var) |> 
    rename(M_Doc = Prevalence)
  
  res_m_nur <- 
    get_unweighted_stats_exp(d_m_nur, exposure_var) |> 
    rename(M_Nur = Prevalence)
  
  res_f_doc <- 
    get_unweighted_stats_exp(d_f_doc, exposure_var) |> 
    rename(F_Doc = Prevalence)
  
  res_f_nur <- 
    get_unweighted_stats_exp(d_f_nur, exposure_var) |> 
    rename(F_Nur = Prevalence)
  
  res_all_doc <- 
    get_unweighted_stats_exp(d_all_doc, exposure_var) |> 
    rename(All_Doc = Prevalence)
  
  res_all_nur <- 
    get_unweighted_stats_exp(d_all_nur, exposure_var) |> 
    rename(All_Nur = Prevalence)
  
  # merge
  final_df <- 
    
    res_m_doc |>
    left_join(res_f_doc, by = c("Variable", "Category")) |>
    left_join(res_all_doc, by = c("Variable", "Category")) |>
    left_join(res_m_nur, by = c("Variable", "Category")) |>
    left_join(res_f_nur, by = c("Variable", "Category")) |> 
    left_join(res_all_nur, by = c("Variable", "Category")) 
  
  # gt table
  final_df |>
    gt(groupname_col = "Variable", 
       rowname_col = "Category") |>
    cols_label(
      Category = md("**Subgroup**"),
      M_Doc = md(glue("**Male**<br>(N = {n_m_doc})")),
      M_Nur = md(glue("**Male**<br>(N = {n_m_nur})")),
      All_Doc = md(glue("**Overall**<br>(N = {n_all_doc})")),
      F_Doc = md(glue("**Female**<br>(N = {n_f_doc})")),
      F_Nur = md(glue("**Female**<br>(N = {n_f_nur})")),
      All_Nur = md(glue("**Overall**<br>(N = {n_all_nur})"))
    ) |>
    tab_spanner(
      label = md("**Doctor**"),
      columns = c(M_Doc, F_Doc, All_Doc)
    ) |>
    tab_spanner(
      label = md("**Nurse**"),
      columns = c(M_Nur, F_Nur, All_Nur)
    ) |>
    tab_header(
      title = md(glue("**{exposure_title} (Unweighted)**"))
    ) |>
    cols_align(align = "center", 
               columns = contains("_")) |>
    tab_stub_indent(rows = Category != "Overall", 
                    indent = 3) |>
    tab_footnote(footnote = "Unweighted Prevalence (95% CI)")
}


# Overall exposure prevalences --------------------------------------------
# Prevalence calculation

calc_prev_exp <- 
  function(data, exposure_var, level = "Yes") {
    
    var_label <- tryCatch(attr(data[[exposure_var]], "label"), 
                        error = function(e) exposure_var)
    
    if (is.null(var_label)) var_label <- exposure_var
    
    
    vec <- data[[exposure_var]]
    vec <- vec[!is.na(vec)] # Remove NAs
    
    
    n_total <- length(vec)
    
    # placeholders if data is empty (prevents errors)
    if (n_total == 0) {
    return(tibble(Variable = var_label, Prevalence = "-"))
  }
    
    n_yes <- sum(vec == "Yes")
    
    # CI using prop.test (Wilson score is standard for props)
    ptest <- suppressWarnings(prop.test(n_yes, n_total, conf.level = 0.95))
    
    
    est <- ptest$estimate * 100
    ci_low <- ptest$conf.int[1] * 100
    ci_high <- ptest$conf.int[2] * 100
  
    
    tibble(
      Variable = var_label,
      Prevalence = sprintf("%.1f%% (%.1f-%.1f)", est, ci_low, ci_high) 
    )
  }


# Loop through exposures
calc_prev_exposure <- 
  function(data, exposure_list) {
    
    map_dfr(exposure_list, ~calc_prev_exp(data, .x))
  }


# Build table
gen_exp_tbl_unweighted <- 
  function(data, exposure_list) {
    
    d_m_doc <- 
      data |> 
      filter(scdm_2_rec == "Male", 
             work_2 == "Doctor")
    
    d_m_nur <-
      data |>
      filter(scdm_2_rec == "Male",
             work_2 == "Nurse")
    
    d_f_doc <-
      data |>
      filter(scdm_2_rec == "Female",    
           work_2 == "Doctor")
    
    d_f_nur <-
      data |>
      filter(scdm_2_rec == "Female",
             work_2 == "Nurse")
    
    d_all_doc <-
      data |>
      filter(work_2 == "Doctor")
    
    d_all_nur <-
      data |>
      filter(work_2 == "Nurse")
  
  # count valid rows
  get_n <- function(df) {
    nrow(df) |> 
      style_number()
  }
  
  n_m_doc <- get_n(d_m_doc)
  n_m_nur <- get_n(d_m_nur)
  n_f_doc <- get_n(d_f_doc)
  n_f_nur <- get_n(d_f_nur)
  n_all_doc <- get_n(d_all_doc)
  n_all_nur <- get_n(d_all_nur)
  
  # calculate
  col_m_doc <- 
    calc_prev_exposure(d_m_doc, exposure_list) |> 
    rename(M_Doc = Prevalence)
  
  col_m_nur <- 
    calc_prev_exposure(d_m_nur, exposure_list) |> 
    rename(M_Nur = Prevalence)
  
  col_f_doc <- 
    calc_prev_exposure(d_f_doc, exposure_list) |> 
    rename(F_Doc = Prevalence)
  
  col_f_nur <- 
    calc_prev_exposure(d_f_nur, exposure_list) |> 
    rename(F_Nur = Prevalence)
  
  col_all_doc <- 
    calc_prev_exposure(d_all_doc, exposure_list) |> 
    rename(All_Doc = Prevalence)
  
  col_all_nur <- 
    calc_prev_exposure(d_all_nur, exposure_list) |> 
    rename(All_Nur = Prevalence)
  
  # merge
  final_df <- 
    
    col_m_doc |>
    left_join(col_f_doc, by = "Variable") |>
    left_join(col_all_doc, by = "Variable") |>
    left_join(col_m_nur, by = "Variable") |>
    left_join(col_f_nur, by = "Variable") |> 
    left_join(col_all_nur, by = "Variable") 
  
  # table
  final_df |> 
    mutate(Variable = factor(Variable, levels = exposure_order),
           Category = if_else(Variable %in% c("Violent threats", 
                                              "Physical violence",
                                              "Sexual Harassment",
                                              "Bullying"),
                              "Risk factor",
                              "Protective factor")
    ) |> 
    arrange(Variable) |> 
    gt(groupname_col = "Category") |>
    cols_label(
      Variable = md("**Exposure**"),
      M_Doc = md(glue("**Male**<br>(N = {n_m_doc})")),
      M_Nur = md(glue("**Male**<br>(N = {n_m_nur})")),
      All_Doc = md(glue("**Overall**<br>(N = {n_all_doc})")),
      F_Doc = md(glue("**Female**<br>(N = {n_f_doc})")),
      F_Nur = md(glue("**Female**<br>(N = {n_f_nur})")),
      All_Nur = md(glue("**Overall**<br>(N = {n_all_nur})"))
    ) |>
    tab_spanner(
      label = md("**Doctor**"),
      columns = c(M_Doc, F_Doc, All_Doc)
    ) |>
    tab_spanner(
      label = md("**Nurse**"),
      columns = c(M_Nur, F_Nur, All_Nur)
    ) |>
    cols_align(align = "center", columns = contains("_")) |>
    tab_footnote(footnote = "Unweighted Prevalence (95% CI)")
    }


# Overall outcome prevalences ---------------------------------------------
# Prevalence calculation

calc_prev_out <- 
  function(data, outcome_var, level = 1) {
    
    var_label <- tryCatch(attr(data[[outcome_var]], "label"), 
                        error = function(e) outcome_var)
    
    if (is.null(var_label)) var_label <- outcome_var
    
    
    vec <- data[[outcome_var]]
    vec <- vec[!is.na(vec)] # Remove NAs
    
    
    n_total <- length(vec)
    
    # placeholders if data is empty (prevents errors)
    if (n_total == 0) {
    return(tibble(Variable = var_label, Prevalence = "-"))
  }
    
    n_yes <- sum(vec == 1)
    
    # CI using prop.test (Wilson score is standard for props)
    ptest <- suppressWarnings(prop.test(n_yes, n_total, conf.level = 0.95))
    
    
    est <- ptest$estimate * 100
    ci_low <- ptest$conf.int[1] * 100
    ci_high <- ptest$conf.int[2] * 100
  
    
    tibble(
      Variable = var_label,
      Prevalence = sprintf("%.1f%% (%.1f-%.1f)", est, ci_low, ci_high) 
    )
  }

# Loop through outcome variables

calc_prev_outcome <- 
  function(data, outcome_list) {
    
    map_dfr(outcome_list, ~calc_prev_out(data, .x))
  }


# Build table

gen_out_tbl_unweighted <- 
  function(data, outcome_list) {
    
    d_m_doc <- 
      data |> 
      filter(scdm_2_rec == "Male", 
             work_2 == "Doctor")
    
    d_m_nur <-
      data |>
      filter(scdm_2_rec == "Male",
             work_2 == "Nurse")
    
    d_f_doc <-
      data |>
      filter(scdm_2_rec == "Female",    
           work_2 == "Doctor")
    
    d_f_nur <-
      data |>
      filter(scdm_2_rec == "Female",
             work_2 == "Nurse")
    
    d_all_doc <-
      data |>
      filter(work_2 == "Doctor")
    
    d_all_nur <-
      data |>
      filter(work_2 == "Nurse")
  
  # count valid rows
  get_n <- function(df) {
    nrow(df) |> 
      style_number()
  }
  
  n_m_doc <- get_n(d_m_doc)
  n_m_nur <- get_n(d_m_nur)
  n_f_doc <- get_n(d_f_doc)
  n_f_nur <- get_n(d_f_nur)
  n_all_doc <- get_n(d_all_doc)
  n_all_nur <- get_n(d_all_nur)
  
  # calculate
  col_m_doc <- 
    calc_prev_outcome(d_m_doc, outcome_list) |> 
    rename(M_Doc = Prevalence)
  
  col_m_nur <- 
    calc_prev_outcome(d_m_nur, outcome_list) |> 
    rename(M_Nur = Prevalence)
  
  col_f_doc <- 
    calc_prev_outcome(d_f_doc, outcome_list) |> 
    rename(F_Doc = Prevalence)
  
  col_f_nur <- 
    calc_prev_outcome(d_f_nur, outcome_list) |> 
    rename(F_Nur = Prevalence)
  
  col_all_doc <- 
    calc_prev_outcome(d_all_doc, outcome_list) |> 
    rename(All_Doc = Prevalence)
  
  col_all_nur <- 
    calc_prev_outcome(d_all_nur, outcome_list) |> 
    rename(All_Nur = Prevalence)
  
  # merge
  final_df <- 
    
    col_m_doc |>
    left_join(col_f_doc, by = "Variable") |>
    left_join(col_all_doc, by = "Variable") |>
    left_join(col_m_nur, by = "Variable") |>
    left_join(col_f_nur, by = "Variable") |> 
    left_join(col_all_nur, by = "Variable")
  
  # table
  final_df |>
    mutate(
    Variable = case_when(
      Variable == "PHQ-9 cut-off" ~ "Probable Depression (PHQ-9) ",
      Variable == "GAD-7 cut-off" ~ "Probable Anxiety (GAD-7)",
      str_detect(Variable, "Suicidal") ~ 
        "Passive suicide thoughts (Item 9 of PHQ-9)", 
      TRUE ~ Variable # Keep others unchanged
    )
  ) |>
    gt() |>
    cols_label(
      Variable = md("**Outcome**"),
      M_Doc = md(glue("**Male**<br>(N = {n_m_doc})")),
      M_Nur = md(glue("**Male**<br>(N = {n_m_nur})")),
      All_Doc = md(glue("**Overall**<br>(N = {n_all_doc})")),
      F_Doc = md(glue("**Female**<br>(N = {n_f_doc})")),
      F_Nur = md(glue("**Female**<br>(N = {n_f_nur})")),
      All_Nur = md(glue("**Overall**<br>(N = {n_all_nur})"))
    ) |>
    tab_spanner(
      label = md("**Doctor**"),
      columns = c(M_Doc, F_Doc, All_Doc)
    ) |>
    tab_spanner(
      label = md("**Nurse**"),
      columns = c(M_Nur, F_Nur, All_Nur)
    ) |>
    cols_align(align = "center", columns = contains("_")) |>
    tab_footnote(footnote = "Unweighted Prevalence (95% CI). PHQ-9: 9-item Patient Health Questionnaire, a cut-off score ≥ 10 is defined for detecting probable depression. GAD-7: 7 item anxiety scale, a cut-off score ≥ 10 is defined for detecting probable anxiety. Any positive answer in the 9th item of the PHQ-9 is considered as having passive suicide thoughts.")
    }


# Aggregate Bayesian models -----------------------------------------------
# Run and save aggregate bayesian models

run_brms_agg <-
  function(outcome, exposure, design_df = ds_ua,
           output_dir = "out/bayesian_models_agg") {

    if(!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }


    # define short names for saving
    out_short <- case_when(
      outcome == "phq_co"    ~ "dep",
      outcome == "gad_co"    ~ "anx"
      )

    exp_short <- case_when(
      exposure == "work_30_dic"   ~ "har",
      exposure == "work_33_dic"   ~ "bul",
      exposure == "work_31_dic"   ~ "threats",
      exposure == "work_32_dic"   ~ "viol",
      TRUE ~ "exp"
      )

    data <- design_df

    specific_vars <- exposure_map_agg[[exposure]]
    if (is.null(specific_vars)) stop(glue("No covariates for {exposure}"))


    full_adjust_vars <- unique(c(covars_str_agg, specific_vars))

    f_adj <- as.formula(paste(outcome, "~", exposure, "+",
                              paste(full_adjust_vars, collapse = "+"),
                              "+ (1 | loc_3)"
                              ))

    # define family
    curr_family <- poisson(link = "log")
    curr_priors <- get("bin_priors", envir = .GlobalEnv)



    # base naming pattern
    base_name <- paste0("bay_", out_short, "_", exp_short)


    name_agg_adj <- paste0(base_name, "_agg_adj")
    path_agg_adj <- file.path(output_dir, name_agg_adj)

    message(glue("Running aggregate model: {name_agg_adj}"))


    model_agg_adjusted <- brm(
      formula = f_adj,
      data = data,
      family = curr_family,
      prior = curr_priors,
      chains = 4,
      cores = 4,
      iter = 2000,
      warmup = 1000,
      seed = 1210,
      save_pars = save_pars(all = TRUE),
      file = path_agg_adj, # auto-loads if existing
      file_refit = "on_change"  # only refit if formula/data changes
    )
    assign(name_agg_adj, model_agg_adjusted, envir = .GlobalEnv)


    return(invisible(TRUE))
  }



# Stratified Bayesian models ----------------------------------------------
# Run and save stratified bayesian models

run_brms_pair <- 
  function(outcome, exposure, design_df = ds_ua, 
           output_dir = "out/bayesian_models") {
    
    if(!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }
    
    
    # define short names for saving
    out_short <- case_when(
      outcome == "phq_co"    ~ "dep",
      outcome == "gad_co"    ~ "anx",
      outcome == "suic_idea" ~ "suic"
      )
    
    exp_short <- case_when(
      exposure == "work_21_dic"   ~ "cols",
      exposure == "work_22_dic"   ~ "sup",
      exposure == "work_30_dic"   ~ "har",
      exposure == "work_33_dic"   ~ "bul",
      exposure == "work_31_dic"   ~ "threats",
      exposure == "work_32_dic"   ~ "viol",
      exposure == "wb_wami_1_dic" ~ "mean",
      exposure == "wb_wami_2_dic" ~ "purp",
      TRUE ~ "exp"
      )
    
    specific_vars <- exposure_map[[exposure]]
    if (is.null(specific_vars)) stop(glue("No covariates for {exposure}"))
    
    
    f_cr <- as.formula(paste(outcome, "~", exposure, "+ (1 | loc_3)"))
    
    f_str <- as.formula(paste(outcome, "~", exposure, "+", 
                              paste(covars_structural, collapse = "+"),
                              "+ (1 | loc_3)"))
    
    
    full_adjust_vars <- setdiff(unique(c(covars_structural, 
                                         specific_vars)), "work_2")
    
    f_adj <- as.formula(paste(outcome, "~", exposure, "+",
                              paste(full_adjust_vars, collapse = "+"),
                              "+ (1 | loc_3)"
                              ))
    
    # define family 
    if (outcome == "wb_who_sc") {
      curr_family <- gaussian()
      curr_priors <- get("cont_priors", envir = .GlobalEnv)
    } else {
      curr_family <- poisson(link = "log")
      curr_priors <- get("bin_priors", envir = .GlobalEnv)
    }
    
    
    # prepare data
    data_doc <- 
      design_df |> 
      filter(work_2 == "Doctor")
    
    data_nur <- 
      design_df |> 
      filter(work_2 == "Nurse")
    
    # base naming pattern
    base_name <- paste0("bay_", out_short, "_", exp_short)
    
    
    # run doctor models
    name_doc_cr <- paste0(base_name, "_doc_cr")
    path_doc_cr <- file.path(output_dir, name_doc_cr)
    
    message(glue("Running doctor model: {name_doc_cr}"))
    
    model_doc_crude <- brm(
      formula = f_cr,
      data = data_doc,
      family = curr_family,
      prior = curr_priors,
      chains = 4,
      cores = 4,
      iter = 2000,
      warmup = 1000,
      seed = 1210,
      save_pars = save_pars(all = TRUE),
      file = path_doc_cr, # auto-loads if existing
      file_refit = "never"  # only refit if formula/data changes
    )
    assign(name_doc_cr, model_doc_crude, envir = .GlobalEnv) # save
    
    name_doc_str <- paste0(base_name, "_doc_str")
    path_doc_str <- file.path(output_dir, name_doc_str)
    
    message(glue("Running doctor model: {name_doc_str}"))
    
    model_doc_structural <- brm(
      formula = f_str,
      data = data_doc,
      family = curr_family,
      prior = curr_priors,
      chains = 4,
      cores = 4,
      iter = 2000,
      warmup = 1000,
      seed = 1210,
      save_pars = save_pars(all = TRUE),
      file = path_doc_str, # auto-loads if existing
      file_refit = "never"  # only refit if formula/data changes
    )
    assign(name_doc_str, model_doc_structural, envir = .GlobalEnv)
    
    
    name_doc_adj <- paste0(base_name, "_doc_adj")
    path_doc_adj <- file.path(output_dir, name_doc_adj)
    
    message(glue("Running doctor model: {name_doc_adj}"))
    
    
    model_doc_adjusted <- brm(
      formula = f_adj,
      data = data_doc,
      family = curr_family,
      prior = curr_priors,
      chains = 4,
      cores = 4,
      iter = 2000,
      warmup = 1000,
      seed = 1210,
      save_pars = save_pars(all = TRUE),
      file = path_doc_adj, # auto-loads if existing
      file_refit = "never"  # only refit if formula/data changes
    )
    assign(name_doc_adj, model_doc_adjusted, envir = .GlobalEnv)
    
    
    # run nurse models
    name_nur_cr <- paste0(base_name, "_nur_cr")
    path_nur_cr <- file.path(output_dir, name_nur_cr)
    
    message(glue("Running nurse model (Update): {name_nur_cr}"))
    
    # showtime
    model_nur_crude <- update(
      model_doc_crude,
      newdata = data_nur,
      file = path_nur_cr,
      cores = 4       # explicit so it always does it
    )
    assign(name_nur_cr, model_nur_crude, envir = .GlobalEnv)
    
    
    name_nur_str <- paste0(base_name, "_nur_str")
    path_nur_str <- file.path(output_dir, name_nur_str)
    
    message(glue("Running nurse model (Update): {name_nur_str}"))
    
    model_nur_structural <- update(
      model_doc_structural,
      newdata = data_nur,
      file = path_nur_str,
      cores = 4       # explicit so it always does it
    )
    assign(name_nur_str, model_nur_structural, envir = .GlobalEnv)
    
    
    name_nur_adj <- paste0(base_name, "_nur_adj")
    path_nur_adj <- file.path(output_dir, name_nur_adj)
    
    message(glue("Running nurse model (Update): {name_nur_adj}"))
    
    model_nur_adjusted <- update(
      model_doc_adjusted,
      newdata = data_nur,
      file = path_nur_adj,
      cores = 4       # explicit so it always does it
    )
    assign(name_nur_adj, model_nur_adjusted, envir = .GlobalEnv)
    
    return(invisible(TRUE))
  }

# Build table with stratified bayesian models results

build_bay_gt <- 
  function(data, metric_type, title_text, footnote_text) {
    
    data |> 
      filter(Type == metric_type) |> 
      mutate(
        Outcome = factor(Outcome, levels = outcome_order),
        Model = factor(Model, levels = adjustment_order),
        Exposure = factor(Exposure, levels = exposure_order)) |> 
      arrange(Outcome, Exposure, Model) |> 
      select(Outcome, Exposure, Profession, Model, Est_CI) |> 
      pivot_wider(
        names_from = c(Profession, Model),
        values_from = Est_CI,
        names_sep = "_"
      ) |> 
      gt(groupname_col = "Outcome",
         rowname_col = "Exposure") |> 
      tab_header(title = md(title_text)) |> 
      tab_spanner(
        label = md("**Doctor**"),
        columns = starts_with("Doctor")
        ) |> 
      tab_spanner(
        label = md("**Nurse**"),
        columns = starts_with("Nurse")
      ) |> 
      cols_label(
        ends_with("_Crude") ~ "Crude",
        ends_with("_Partially adjusted") ~ "Partially adjusted",
        ends_with("_Fully adjusted") ~ "Fully adjusted" 
      ) |> 
      cols_align(
        align = "center",
        columns = contains("_")
        ) |> 
      tab_style(
        style = cell_text(align = "left"),
        locations = cells_stub()
        ) |>
      tab_stub_indent(
        rows = everything(), 
        indent = 3
        ) |> 
      tab_footnote(footnote = footnote_text) |> 
      tab_footnote(
        locations = cells_column_labels(columns = contains("_Crude")),
        footnote = "Region as second-order covariate only."
      ) |> 
      tab_footnote(
        locations = cells_column_labels(columns = contains("_Part")),
        footnote = "Adjusted by Gender, Age group and Proximity to combat areas and occupied territories ."
      ) |> 
      tab_footnote(
        locations = cells_column_labels(columns = contains("_Full")),
        footnote =
          paste0("Partially adjusted models plus specific covariates:", "\n",
                 "Bullying and sexual harassment adjusted by Healthcare setting, Post-graduate training and Length of service.", "\n",
                 "Violent threats and physical violence adjusted by Healthcare setting.", "\n",
                 "Support from colleagues or superiors adjusted by Healthcare setting and Length of service.", "\n",
                 "Finding meaning or purpose in job adjusted by Healthcare setting, Relationship status, Having children, and Length of service.")
      )
  }



# Bayesian models with restrictive priors ---------------------------------
# Run and save sensitivity bayesian models

run_brms_sensitivity <- 
  function(outcome, exposure, design_df = ds_ua, 
           output_dir = "out/bayesian_models_sens") {
  
  # Check directory
  if(!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  # setup names and formulas
  out_short <- case_when(
    outcome == "phq_co" ~ "dep",
    outcome == "gad_co" ~ "anx"
  )
  
  exp_short <- case_when(
    exposure == "work_21_dic"    ~ "cols",
    exposure == "work_22_dic"    ~ "sup",
    exposure == "work_30_dic"    ~ "har",
    exposure == "work_33_dic"    ~ "bul",
    exposure == "work_31_dic"    ~ "threats",
    exposure == "work_32_dic"    ~ "viol",
    exposure == "wb_wami_1_dic"  ~ "mean",
    exposure == "wb_wami_2_dic"  ~ "purp"
  )
  
  # formulas
  specific_vars <- exposure_map[[exposure]]
  
  f_cr <- as.formula(paste(outcome, "~", exposure, "+ (1 | loc_3)"))
  
  f_str <- as.formula(paste(outcome, "~", exposure, "+", 
                            paste(covars_structural, collapse = "+"),
                            "+ (1 | loc_3)"))
  
  full_adjust_vars <- setdiff(unique(c(covars_structural, specific_vars)),
                              "work_2")
  
  f_adj <- as.formula(paste(outcome, "~", exposure, "+",
                            paste(full_adjust_vars, collapse = "+"),
                            "+ (1 | loc_3)"))
  
  # split Data
  data_doc <- design_df |> filter(work_2 == "Doctor")
  data_nur <- design_df |> filter(work_2 == "Nurse")
  
  base_name <- paste0("bay_sens_", out_short, "_", exp_short)
  
  # always poisson family
  curr_family <- poisson(link = "log")
  
  
  # doctor models
  
  # Select the correct prior based on outcome
  if (outcome == "phq_co") {
    
    curr_prior_doc <- priors_res_phq_doc
    message(glue(">> Doc Prior: PHQ Informative (Log-Odds: {round(intercept_prior_phq, 2)})"))
  
    } else if (outcome == "gad_co") {
      
      curr_prior_doc <- priors_res_gad_doc
      message(
        glue(">> Doc Prior: GAD Informative (Log-Odds: {round(intercept_prior_gad, 2)})"))
  
      } else {
        
        stop("Outcome must be phq_co or gad_co")
  }

  # crude
  name_doc_cr <- paste0(base_name, "_doc_cr")
  
  brm(
    formula = f_cr, 
    data = data_doc, 
    family = curr_family, 
    prior = curr_prior_doc,
    chains = 4, 
    cores = 4, 
    iter = 2000, 
    warmup = 1000, 
    seed = 1210,
    file = file.path(output_dir, name_doc_cr), 
    file_refit = "never")
  
  # partially adjusted
  name_doc_str <- paste0(base_name, "_doc_str")
  
  brm(
    formula = f_str, 
    data = data_doc, 
    family = curr_family, 
    prior = curr_prior_doc,
    chains = 4, 
    cores = 4, 
    iter = 2000, 
    warmup = 1000, 
    seed = 1210,
    file = file.path(output_dir, name_doc_str), 
    file_refit = "never")

  # full model
  name_doc_adj <- paste0(base_name, "_doc_adj")
  
  brm(
    formula = f_adj, 
    data = data_doc, 
    family = curr_family, 
    prior = curr_prior_doc,
    chains = 4, 
    cores = 4, 
    iter = 2000, 
    warmup = 1000, 
    seed = 1210,
    file = file.path(output_dir, name_doc_adj), 
    file_refit = "never")
  
  
  # nurse models
  # Always use the "Weak Intercept" set for nurses
  
  curr_prior_nur <- priors_sens_nur
  message(">> Nur Prior: Weak Intercept + Skeptical Beta")

  # crude
  name_nur_cr <- paste0(base_name, "_nur_cr")
  
  brm(
    formula = f_cr, 
    data = data_nur, 
    family = curr_family, 
    prior = curr_prior_nur,
    chains = 4, 
    cores = 4, 
    iter = 2000, 
    warmup = 1000, 
    seed = 1210,
    file = file.path(output_dir, name_nur_cr), 
    file_refit = "never")
  
  # partially adjusted
  name_nur_str <- paste0(base_name, "_nur_str")
  
  brm(
    formula = f_str, 
    data = data_nur, 
    family = curr_family, 
    prior = curr_prior_nur,
    chains = 4, 
    cores = 4, 
    iter = 2000,
    warmup = 1000, 
    seed = 1210,
    file = file.path(output_dir, name_nur_str), 
    file_refit = "never")
  
  # full model
  name_nur_adj <- paste0(base_name, "_nur_adj")
  
  brm(
    formula = f_adj, 
    data = data_nur, 
    family = curr_family, 
    prior = curr_prior_nur,
    chains = 4, 
    cores = 4, 
    iter = 2000, 
    warmup = 1000, 
    seed = 1210,
    file = file.path(output_dir, name_nur_adj), 
    file_refit = "never")

  return(invisible(FALSE))
}


# Bayesian diagnostics ---------------------------------------------------
# define diagnostic extractor

extract_diag <- 
  function(model, model_name) {
   
    # get summary of parameters
    # focus on fixed effect (population-level)
    
    summ <- tryCatch({
      as_draws(model) |> 
        summarise_draws(default_convergence_measures())
    }, error = function(e) return (NULL)) # if anything is broken
    
    if(is.null(summ)) return(NULL)
    
    # key stats
    tibble(
      model_id = model_name,
      max_rhat = max(summ$rhat, na.rm = TRUE),
      min_ess_bulk = min(summ$ess_bulk, na.rm = TRUE),
      min_ess_tail = min(summ$ess_tail, na.rm = TRUE),
      # Check for divergences (Stan specific)
      divergences = tryCatch(rstan::get_num_divergent(model$fit), 
                             error = function(e) NA)
    )
    
  }



# Process list of paths from disk

process_disk_models <- 
  function(file_list, type_label) {
    map_dfr(file_list, function(filepath) {
      
      clean_name <- tools::file_path_sans_ext(basename(filepath))
      
      # Load model
      mod <- readRDS(filepath)
      
      # Extract diagnostics with existing function
      res <- extract_diag(mod, clean_name)
      
      # Explicitly clear memory
      rm(mod)
      gc()
      
      return(res)
    }) |> 
      mutate(Type = type_label)
  }


# Function to generate diagnostic plots

create_diag_plots <- 
  function(model, title_text, param_name = "b_exposure_yes") {
    
    # trace plot: convergence - intercept and exposure
    p_trace <- mcmc_plot(
      model,
      type = "trace",
      variable = c("b_Intercept", param_name),
      facet_args = list(ncol = 1,
                        strip.position = "right") # stack vertically 
    ) +
      labs(title = paste0(title_text, " | Trace")) +
      theme_classic() + 
      theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size = 10),
        strip.text = element_text(size = 10),
        plot.title = element_text(size = 10, face = "bold"),
        legend.position = "none",
        plot.margin = margin(t = 2, r = 5, b = 2, l = 0, unit = "pt")
      )
    
    
    # posterior predictive checks
    p_ppc <- pp_check(
      model,
      ndraws = 100,
      type = "dens_overlay"
      ) + 
      labs(title = paste0(title_text, " | Fit")) + 
      theme_classic() +
      scale_x_continuous(
        labels = scales::label_number(
          accuracy = 0.01,
          decimal.mark = "·"
      )) + 
      theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size = 10),
        plot.title = element_text(size = 10, face = "bold"),
        strip.text = element_text(size = 10),
        legend.position = "none",
        plot.margin = margin(t = 2, r = 0, b = 2, l = 5, unit = "pt")
      )
    
    # return both
    list(trace = p_trace, ppc = p_ppc)
  }


# Build table with diagnostics

prepare_dx_table_data <- 
  function(df, analysis_type) {
    
    df |> 
      filter(Type == analysis_type) |> 
      pivot_wider(
        id_cols = c(outcome, exposure, adjustment),
        names_from = group,
        values_from = c(max_rhat, min_ess_bulk, divergences),
        names_glue = "{group}_{.value}"
      ) |> 
      arrange(outcome, exposure, adjustment) |> 
      select(
        outcome, exposure, adjustment, starts_with("Doctor"), 
        starts_with("Nurse"), starts_with("Aggregate")
      )
  }



# Weighted prevalences by scdm ------------------------------------
# Caculate outcomes' weighted prevalences

calc_out_prev_w <- 
  function(var, outcome, design, level = 1) {
    
    var_label <- tryCatch(attr(design$variables[[var]], "label"),
                          error = function(e) var)
    
    if (is.null(var_label)) { 
      var_label <- var 
    }
    
    
    form_outcome <- as.formula(paste0("~I(", outcome, " =='", level, "')"))
    form_by <- as.formula(paste0("~", var))
    
    prev <- svyby(
      form_outcome,
      form_by,
      design = design,
      FUN = svyciprop,
      vartype = c("ci"),
      method = "beta",
      level = 0.95,
      na.rm = TRUE
    ) |> 
      as_tibble() 
    
    prop_col <- grep("^I\\(", names(prev), value = TRUE)
    
    prev <- 
      prev |> 
      mutate(
        Variable = var_label,
        prev = 100 * .data[[prop_col]],
        ci_low = 100 * ci_l,
        ci_high = 100 * ci_u,
        Prevalence = sprintf("%.1f%% (%.1f-%.1f)", prev, ci_low, ci_high)
      ) |> 
      rename(Category = all_of(var)) |> 
      select(Variable, Category, Prevalence)
    
    return(prev)
  }


# Get outcome weighted stats 

get_w_out_column_stats <- function(design_subset, outcome, level = 1) {
  
  # calculate overall for this specific subgroup
  overall_stat <- svyciprop(
    as.formula(paste0("~I(", outcome, " == '", level ,"')")),
    design = design_subset, 
    vartype = "ci",
    method = "beta",
    na.rm = TRUE
  )
  
  overall_row <- tibble(
    Variable = "Overall",
    Category = "Overall",
    Prevalence = 
      sprintf(
        "%.1f%% (%.1f-%.1f)", 100 * coef(overall_stat),
        100 * confint(overall_stat)[1], 100* confint(overall_stat)[2])
  )
  
  rows_stats <- map_dfr(scdm_vars, 
                        calc_out_prev_w, 
                        outcome = outcome,
                        design = design_subset)
  
  bind_rows(overall_row, rows_stats)

  }


# Build weighted outcomes table

generate_outcome_table_w_out <- 
  function(outcome_var, outcome_title) {

    # generate subsets
    d_f_doc <-
      subset(svy_ua_w,
             scdm_2_rec == "Female" &
               work_2 == "Doctor")
    
    d_f_nur <-
      subset(svy_ua_w,
             scdm_2_rec == "Female" &
               work_2 == "Nurse")
    
    d_m_doc <-
      subset(svy_ua_w,
             scdm_2_rec == "Male" &
               work_2 == "Doctor")
    
    d_m_nur <-
      subset(svy_ua_w,
             scdm_2_rec == "Male" &
               work_2 == "Nurse")
    
    d_all_doc <-
      subset(svy_ua_w,
             work_2 == "Doctor")
    
    d_all_nur <-
      subset(svy_ua_w,
             work_2 == "Nurse")
    
    # unweighted n for headers
    n_f_doc <- 
      ds_ua |> 
      filter(scdm_2_rec == "Female" & 
               work_2 == "Doctor" &
               !is.na(.data[[outcome_var]])) |> 
      count() |> 
      pull() |> 
      style_number()
      
    n_f_nur <- 
      ds_ua |> 
      filter(scdm_2_rec == "Female" & 
               work_2 == "Nurse" &
               !is.na(.data[[outcome_var]])) |> 
      count() |> 
      pull() |> 
      style_number()
    
    n_m_doc <- 
      ds_ua |> 
      filter(scdm_2_rec == "Male" & 
               work_2 == "Doctor" &
               !is.na(.data[[outcome_var]])) |> 
      count() |> 
      pull() |> 
      style_number()
    
    n_m_nur <- 
      ds_ua |> 
      filter(scdm_2_rec == "Male" & 
               work_2 == "Nurse" &
               !is.na(.data[[outcome_var]])) |> 
      count() |> 
      pull() |> 
      style_number()
    
    n_all_doc <- 
      ds_ua |> 
      filter(work_2 == "Doctor" &
               !is.na(.data[[outcome_var]])) |> 
      count() |> 
      pull() |> 
      style_number()
    
    n_all_nur <- 
      ds_ua |> 
      filter(work_2 == "Nurse" &
               !is.na(.data[[outcome_var]])) |> 
      count() |> 
      pull() |> 
      style_number()
    
    
    
    # calculations
    res_f_doc <- get_w_out_column_stats(d_f_doc, outcome_var) |>
      rename(F_Doc = Prevalence)
    
    res_f_nur <- get_w_out_column_stats(d_f_nur, outcome_var) |>
      rename(F_Nur = Prevalence)
    
    res_m_doc <- get_w_out_column_stats(d_m_doc, outcome_var) |>
      rename(M_Doc = Prevalence)
    
    res_m_nur <- get_w_out_column_stats(d_m_nur, outcome_var) |>
      rename(M_Nur = Prevalence)
    
    res_all_doc <- get_w_out_column_stats(d_all_doc, outcome_var) |>
      rename(All_Doc = Prevalence)
    
    res_all_nur <- get_w_out_column_stats(d_all_nur, outcome_var) |>
      rename(All_Nur = Prevalence)
    
    
    # merge
    final_df <- 
      
      res_f_doc |>
      left_join(res_m_doc, by = c("Variable", "Category")) |>
      left_join(res_all_doc, by = c("Variable", "Category")) |>
      left_join(res_f_nur, by = c("Variable", "Category")) |>
      left_join(res_m_nur, by = c("Variable", "Category")) |> 
      left_join(res_all_nur, by = c("Variable", "Category"))
    
    
    final_df |>
      gt(groupname_col = "Variable", rowname_col = "Category") |>
      cols_label(
        Category = md("**Subgroup**"),
        M_Doc = md(glue("**Male**<br>(N = {n_m_doc})")),
        M_Nur = md(glue("**Male**<br>(N = {n_m_nur})")),
        All_Doc = md(glue("**Overall**<br>(N = {n_all_doc})")),
        F_Doc = md(glue("**Female**<br>(N = {n_f_doc})")),
        F_Nur = md(glue("**Female**<br>(N = {n_f_nur})")),
        All_Nur = md(glue("**Overall**<br>(N = {n_all_nur})"))
        ) |>
      tab_spanner(
        label = md("**Doctor**"),
        columns = c(M_Doc, F_Doc, All_Doc)
      ) |>
      tab_spanner(
        label = md("**Nurse**"),
        columns = c(M_Nur, F_Nur, All_Nur)
        ) |>
      tab_header(
        title = md(glue("**{outcome_title}**"))
        ) |>
      cols_align(align = "center", columns = contains("_")) |>
      tab_stub_indent(rows = Category != "Overall", indent = 3) |>
      tab_footnote(footnote = "Weighted Prevalence (95% CI)")
    }
 

# Exposures
# Calculate weighted exposure prevalences

calc_exp_prev_w <- 
  function(var, exposure, design, level = "Yes") {
    
    var_label <- tryCatch(attr(design$variables[[var]], "label"),
                          error = function(e) var)
    
    if (is.null(var_label)) { 
      var_label <- var 
    }
    
    
    form_exposure <- as.formula(paste0("~I(", exposure, " =='", level, "')"))
    form_by <- as.formula(paste0("~", var))
    
    prev <- svyby(
      form_exposure,
      form_by,
      design = design,
      FUN = svyciprop,
      vartype = c("ci"),
      method = "beta",
      level = 0.95,
      na.rm = TRUE
    ) |> 
      as_tibble() 
    
    prop_col <- grep("^I\\(", names(prev), value = TRUE)
    
    prev <- 
      prev |> 
      mutate(
        Variable = var_label,
        prev = 100 * .data[[prop_col]],
        ci_low = 100 * ci_l,
        ci_high = 100 * ci_u,
        Prevalence = sprintf("%.1f%% (%.1f-%.1f)", prev, ci_low, ci_high)
      ) |> 
      rename(Category = all_of(var)) |> 
      select(Variable, Category, Prevalence)
    
    return(prev)
  }

# Get weighted exposure prevalence and overall

get_w_exp_column_stats <- function(design_subset, exposure) {
  
  # calculate overall for this specific subgroup
  overall_stat <- svyciprop(
    as.formula(paste0("~I(", exposure, " == 'Yes')")),
    design = design_subset, 
    vartype = "ci",
    method = "beta",
    na.rm = TRUE
  )
  
  overall_row <- tibble(
    Variable = "Overall",
    Category = "Overall",
    Prevalence = 
      sprintf(
        "%.1f%% (%.1f-%.1f)", 100 * coef(overall_stat),
        100 * confint(overall_stat)[1], 100* confint(overall_stat)[2])
  )
  
  rows_stats <- map_dfr(scdm_vars, 
                        calc_exp_prev_w, 
                        exposure = exposures,
                        design = design_subset)
  
  bind_rows(overall_row, rows_stats)

  }


# Build weighted prevalence of exposures table

generate_exposure_table_w_exp <- 
  function(exposure_var, exposure_title) {

    # generate subsets
    d_f_doc <-
      subset(svy_ua_w,
             scdm_2_rec == "Female" &
               work_2 == "Doctor")
    
    d_f_nur <-
      subset(svy_ua_w,
             scdm_2_rec == "Female" &
               work_2 == "Nurse")
    
    d_m_doc <-
      subset(svy_ua_w,
             scdm_2_rec == "Male" &
               work_2 == "Doctor")
    
    d_m_nur <-
      subset(svy_ua_w,
             scdm_2_rec == "Male" &
               work_2 == "Nurse")
    
    d_all_doc <-
      subset(svy_ua_w,
             work_2 == "Doctor")
    
    d_all_nur <-
      subset(svy_ua_w,
             work_2 == "Nurse")
    
    # unweighted n for headers
    n_f_doc <- 
      ds_ua |> 
      filter(scdm_2_rec == "Female" & 
               work_2 == "Doctor" &
               !is.na(.data[[exposure_var]])) |> 
      count() |> 
      pull() |> 
      style_number()
      
    n_f_nur <- 
      ds_ua |> 
      filter(scdm_2_rec == "Female" & 
               work_2 == "Nurse" &
               !is.na(.data[[exposure_var]])) |> 
      count() |> 
      pull() |> 
      style_number()
    
    n_m_doc <- 
      ds_ua |> 
      filter(scdm_2_rec == "Male" & 
               work_2 == "Doctor" &
               !is.na(.data[[exposure_var]])) |> 
      count() |> 
      pull() |> 
      style_number()
    
    n_m_nur <- 
      ds_ua |> 
      filter(scdm_2_rec == "Male" & 
               work_2 == "Nurse" &
               !is.na(.data[[exposure_var]])) |> 
      count() |> 
      pull() |> 
      style_number()
    
    n_all_doc <- 
      ds_ua |> 
      filter(work_2 == "Doctor" &
               !is.na(.data[[exposure_var]])) |> 
      count() |> 
      pull() |> 
      style_number()
    
    n_all_nur <- 
      ds_ua |> 
      filter(work_2 == "Nurse" &
               !is.na(.data[[exposure_var]])) |> 
      count() |> 
      pull() |> 
      style_number()
    
    
    # calculations
    res_f_doc <- get_w_exp_column_stats(d_f_doc, exposure_var) |>
      rename(F_Doc = Prevalence)
    
    res_f_nur <- get_w_exp_column_stats(d_f_nur, exposure_var) |>
      rename(F_Nur = Prevalence)
    
    res_m_doc <- get_w_exp_column_stats(d_m_doc, exposure_var) |>
      rename(M_Doc = Prevalence)
    
    res_m_nur <- get_w_exp_column_stats(d_m_nur, exposure_var) |>
      rename(M_Nur = Prevalence)
    
    res_all_doc <- get_w_exp_column_stats(d_all_doc, exposure_var) |>
      rename(All_Doc = Prevalence)
    
    res_all_nur <- get_w_exp_column_stats(d_all_nur, exposure_var) |>
      rename(All_Nur = Prevalence)
    
    
    # merge
    final_df <- 
      
      res_f_doc |>
      left_join(res_f_nur, by = c("Variable", "Category")) |>
      left_join(res_all_nur, by = c("Variable", "Category")) |>
      left_join(res_m_doc, by = c("Variable", "Category")) |>
      left_join(res_m_nur, by = c("Variable", "Category")) |>
      left_join(res_all_doc, by = c("Variable", "Category")) 
    
    
    final_df |>
      gt(groupname_col = "Variable", rowname_col = "Category") |>
      cols_label(
        Category = md("**Subgroup**"),
        M_Doc = md(glue("**Male**<br>(N = {n_m_doc})")),
        M_Nur = md(glue("**Male**<br>(N = {n_m_nur})")),
        All_Doc = md(glue("**Overall**<br>(N = {n_all_doc})")),
        F_Doc = md(glue("**Female**<br>(N = {n_f_doc})")),
        F_Nur = md(glue("**Female**<br>(N = {n_f_nur})")),
        All_Nur = md(glue("**Overall**<br>(N = {n_all_nur})"))
        ) |>
      tab_spanner(
        label = md("**Doctor**"),
        columns = c(M_Doc, F_Doc, All_Doc)
      ) |>
      tab_spanner(
        label = md("**Nurse**"),
        columns = c(M_Nur, F_Nur, All_Nur)
        ) |>
      tab_header(
        title = md(glue("**{exposure_title}**"))
        ) |>
      cols_align(align = "center", columns = contains("_")) |>
      tab_stub_indent(rows = Category != "Overall", indent = 3) |>
      tab_footnote(footnote = "Weighted Prevalence (95% CI)")
    }


## Overall exposure weighted prevalences--------------------------------
# Calculate subgroup weighted prevalences

calc_w_prev_exp <- 
  function(design_subset, exposure, level = "Yes") {
    
    var_label <- tryCatch(attr(design_subset$variables[[exposure]], "label"),
                          error = function(e) exposure)
    
    if (is.null(var_label)) { 
      var_label <- exposure 
    }
    
    prev <-tryCatch({
      svyciprop(
        as.formula(paste0("~I(", exposure, " == '", level, "')")),
        design = design_subset,
        vartype = "ci",
        method = "beta",
        na.rm = TRUE
      )
    }, error = function(e) return(NULL)) 
    
    if(is.null(prev)) return(tibble(Variable = var_label, Prevalence = "-"))
    
    tibble(
      Variable = var_label,
      Prevalence = sprintf(
        "%.1f%% (%.1f-%.1f)", 
        100 * coef(prev),
        100 * confint(prev)[1], 
        100 * confint(prev)[2]
      ) 
    )
  }


# Get weighted prevalences

calc_prev_exposure_w <- 
  function(design_subset, exposure) {
    
    map_dfr(exposures, 
            ~calc_w_prev_exp(design_subset, .x))
    
  }


# Build weighted exposure table 

gen_w_exp_tbl <- 
  function(exposure) {
    
    d_f_doc <- subset(svy_ua_w, 
                       scdm_2_rec == "Female" & work_2 == "Doctor")
    d_f_nur <- subset(svy_ua_w,
                       scdm_2_rec == "Female" & work_2 == "Nurse")
    d_m_doc <- subset(svy_ua_w,
                       scdm_2_rec == "Male" & work_2 == "Doctor")
    d_m_nur <- subset(svy_ua_w,
                       scdm_2_rec == "Male" & work_2 == "Nurse")
    
    d_all_doc <- subset(svy_ua_w,
                      work_2 == "Doctor")
    d_all_nur <- subset(svy_ua_w,
                      work_2 == "Nurse")
    
    get_n <- function(gen, work) {
      ds_ua |> 
        filter(scdm_2_rec == gen & work_2 == work) |> 
        count() |> 
        pull() |> 
        style_number()
    }
    
    n_f_doc <- get_n("Female", "Doctor")
    n_f_nur <- get_n("Female", "Nurse")
    n_m_doc <- get_n("Male", "Doctor")
    n_m_nur <- get_n("Male", "Nurse")
    
    n_all_doc <-
      ds_ua |> 
        filter(work_2 == "Doctor") |> 
        count() |> 
        pull() |> 
        style_number()
    
    n_all_nur <-
      ds_ua |> 
      filter(work_2 == "Nurse") |> 
      count() |> 
      pull() |> 
      style_number()
    
    
    col_f_doc <- 
      calc_prev_exposure_w(d_f_doc, exposure) |> 
      rename(F_Doc = Prevalence)
    
    col_f_nur <- 
      calc_prev_exposure_w(d_f_nur, exposure) |> 
      rename(F_Nur = Prevalence)
    
    col_m_doc <- 
      calc_prev_exposure_w(d_m_doc, exposure) |> 
      rename(M_Doc = Prevalence)
    
    col_m_nur <- 
      calc_prev_exposure_w(d_m_nur, exposure) |> 
      rename(M_Nur = Prevalence)
    
    col_all_doc <- 
      calc_prev_exposure_w(d_all_doc, exposure) |> 
      rename(All_Doc = Prevalence)
    
    col_all_nur <- 
      calc_prev_exposure_w(d_all_nur, exposure) |> 
      rename(All_Nur = Prevalence)
    
    
    final_df <- 
      
      col_m_doc |>
      left_join(col_f_doc, by = "Variable") |>
      left_join(col_all_doc, by = "Variable") |>
      left_join(col_m_nur, by = "Variable") |>
      left_join(col_f_nur, by = "Variable") |> 
      left_join(col_all_nur, by = "Variable") 
    
    
    final_df |> 
      mutate(Variable = factor(Variable, levels = exposure_order),
             Category = if_else(Variable %in% c("Violent threats", 
                                              "Physical violence",
                                              "Sexual Harassment",
                                              "Bullying"),
                              "Risk factor",
                              "Protective factor")
             ) |> 
      arrange(Variable) |> 
      gt(groupname_col = "Category") |> 
      cols_label(
        Variable = md("**Exposure**"),
        M_Doc = md(glue("**Male**<br>(N = {n_m_doc})")),
        M_Nur = md(glue("**Male**<br>(N = {n_m_nur})")),
        All_Doc = md(glue("**Overall**<br>(N = {n_all_doc})")),
        F_Doc = md(glue("**Female**<br>(N = {n_f_doc})")),
        F_Nur = md(glue("**Female**<br>(N = {n_f_nur})")),
        All_Nur = md(glue("**Overall**<br>(N = {n_all_nur})"))
        
      ) |> 
      tab_spanner(
        label = md("**Doctor**"),
        columns = c(F_Doc, M_Doc, All_Doc)
      ) |> 
      tab_spanner(
        label = md("**Nurse**"),
        columns = c(F_Nur, M_Nur, All_Nur)
      ) |> 
      cols_align(align = "center", columns = contains("_")) |> 
      tab_footnote(footnote = "Weighted Prevalence (95% CI)")
    }

## Overall outcome weighted prevalences-------------------------
# Calculate outcome weighted prevalences

calc_w_prev_out <- 
  function(design_subset, outcome, level = 1) {
    
    var_label <- tryCatch(attr(design_subset$variables[[outcome]], "label"),
                          error = function(e) outcome)
    
    if (is.null(var_label)) { 
      var_label <- outcome 
    }
    
    prev <-tryCatch({
      svyciprop(
        as.formula(paste0("~I(", outcome, " == '", level, "')")),
        design = design_subset,
        vartype = "ci",
        method = "beta",
        na.rm = TRUE
      )
    }, error = function(e) return(NULL)) 
    
    if(is.null(prev)) return(tibble(Variable = var_label, Prevalence = "-"))
    
    tibble(
      Variable = var_label,
      Prevalence = sprintf(
        "%.1f%% (%.1f-%.1f)", 
        100 * coef(prev),
        100 * confint(prev)[1], 
        100 * confint(prev)[2]
      ) 
    )
  }

# Get outcome weighted prevalences

calc_prev_outcome_w <- 
  function(design_subset, outcome) {
    
    map_dfr(outcomes, 
            ~calc_w_prev_out(design_subset, .x))
    
  }


# Build weighted outcome prevalence table
gen_w_out_tbl <- 
  function(outcome) {
    
    d_f_doc <- subset(svy_ua_w, 
                       scdm_2_rec == "Female" & work_2 == "Doctor")
    d_f_nur <- subset(svy_ua_w,
                       scdm_2_rec == "Female" & work_2 == "Nurse")
    d_m_doc <- subset(svy_ua_w,
                       scdm_2_rec == "Male" & work_2 == "Doctor")
    d_m_nur <- subset(svy_ua_w,
                       scdm_2_rec == "Male" & work_2 == "Nurse")
    d_all_doc <- subset(svy_ua_w,
                      work_2 == "Doctor")
    d_all_nur <- subset(svy_ua_w,
                      work_2 == "Nurse")
    
    get_n <- function(gen, work) {
      ds_ua |> 
        filter(scdm_2_rec == gen & work_2 == work) |> 
        count() |> 
        pull() |> 
        style_number()
    }
    
    n_f_doc <- get_n("Female", "Doctor")
    n_f_nur <- get_n("Female", "Nurse")
    n_m_doc <- get_n("Male", "Doctor")
    n_m_nur <- get_n("Male", "Nurse")
    
    n_all_doc <- 
      ds_ua |> 
        filter(work_2 == "Doctor") |> 
        count() |> 
        pull() |> 
        style_number()
    
    n_all_nur <- 
      ds_ua |> 
      filter(work_2 == "Nurse") |> 
      count() |> 
      pull() |> 
      style_number()
    
    
    col_f_doc <- 
      calc_prev_outcome_w(d_f_doc, outcome) |> 
      rename(F_Doc = Prevalence)
    
    col_f_nur <- 
      calc_prev_outcome_w(d_f_nur, outcome) |> 
      rename(F_Nur = Prevalence)
    
    col_m_doc <- 
      calc_prev_outcome_w(d_m_doc, outcome) |> 
      rename(M_Doc = Prevalence)
    
    col_m_nur <- 
      calc_prev_outcome_w(d_m_nur, outcome) |> 
      rename(M_Nur = Prevalence)
    
    col_all_doc <- 
      calc_prev_outcome_w(d_all_doc, outcome) |> 
      rename(All_Doc = Prevalence)
    
    col_all_nur <- 
      calc_prev_outcome_w(d_all_nur, outcome) |> 
      rename(All_Nur = Prevalence)
    
    
    final_df <- 
      
      col_m_doc |>
      left_join(col_f_doc, by = "Variable") |>
      left_join(col_all_doc, by = "Variable") |>
      left_join(col_m_nur, by = "Variable") |>
      left_join(col_f_nur, by = "Variable") |> 
      left_join(col_all_nur, by = "Variable")
    
    
    final_df |>
    mutate(
    Variable = case_when(
      Variable == "PHQ-9 cut-off" ~ "Probable Depression (PHQ-9) ",
      Variable == "GAD-7 cut-off" ~ "Probable Anxiety (GAD-7)",
      str_detect(Variable, "Suicidal") ~ 
        "Passive suicide thoughts (Item 9 of PHQ-9)", 
      TRUE ~ Variable # Keep others unchanged
    )
  ) |>
      gt() |> 
      cols_label(
        Variable = md("**Outcome**"),
        M_Doc = md(glue("**Male**<br>(N = {n_m_doc})")),
        M_Nur = md(glue("**Male**<br>(N = {n_m_nur})")),
        All_Doc = md(glue("**Overall**<br>(N = {n_all_doc})")),
        F_Doc = md(glue("**Female**<br>(N = {n_f_doc})")),
        F_Nur = md(glue("**Female**<br>(N = {n_f_nur})")),
        All_Nur = md(glue("**Overall**<br>(N = {n_all_nur})"))
      ) |> 
      tab_spanner(
        label = md("**Doctor**"),
        columns = c(M_Doc, F_Doc, All_Doc)
      ) |>
      tab_spanner(
        label = md("**Nurse**"),
        columns = c(M_Nur, F_Nur, All_Nur)
      ) |> 
      cols_align(align = "center", columns = contains("_")) |> 
      tab_footnote(footnote = "Unweighted Prevalence (95% CI). PHQ-9: 9-item Patient Health Questionnaire, a cut-off score ≥ 10 is defined for detecting probable depression. GAD-7: 7 item anxiety scale, a cut-off score ≥ 10 is defined for detecting probable anxiety. Any positive answer in the 9th item of the PHQ-9 is considered as having passive suicide thoughts.")
    }

 
## Survey models------------------------------------------------------------
# Run and save every survey model

run_svy_models <- 
  function(outcome, exposure, profession, design_object, 
           output_dir = "out/svy_models") {
    
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }
    
    # outcome abbreviation
    out_short <- case_when(
      outcome == "phq_co"      ~ "dep",
      outcome == "gad_co"      ~ "anx",
      outcome == "suic_idea"   ~ "suic"
      )
    
    # exposure abbreviation
    exp_short <- case_when( 
      exposure == "work_21_dic"   ~ "cols",
      exposure == "work_22_dic"   ~ "sup",
      exposure == "work_30_dic"   ~ "har",
      exposure == "work_33_dic"   ~ "bul",
      exposure == "work_31_dic"   ~ "threats",
      exposure == "work_32_dic"   ~ "viol",
      exposure == "wb_wami_1_dic" ~ "mean",
      exposure == "wb_wami_2_dic" ~ "purp"
      )
    
    # profession abbreviation
    prof_short <- case_when(
      profession == "Doctor" ~ "doc",
      profession == "Nurse"  ~ "nur"
      )
    
    # determine model type
    if (outcome == "wb_who_sc") {
      current_family <- gaussian()
    } else {
      current_family <- quasipoisson(link = "log")
    }
    
    # This line cernters or adjust in the grand mean of neighboring strata
    # when any of these has few observations
    
    options(survey.lonely.psu = "adjust")
    
    # subset design
    sub_design <- subset(design_object, work_2 == profession)
    
    # look for its covariates
    specific_vars <- exposure_map[[exposure]] 
    
    # safety check
    if (is.null(specific_vars)) {
      stop(glue("Error: No covariates defined for '{exposure}'."))
    }
    
    
    # build formulas
    f_crude <- as.formula(paste(outcome, "~", exposure))
    
    f_str <- as.formula(paste(outcome, "~", exposure, "+",
                              paste(covars_structural, collapse = "+")))
    
    full_adjust <- unique(c(covars_structural, specific_vars))
    f_adj <- as.formula(paste(outcome, "~", exposure, "+",
                              paste(full_adjust, collapse = "+")))
    
    # run 
    
    
    m_str <- try(svyglm(
      f_str, 
      design = sub_design,
      family = current_family
    ))
    
    m_adj <- try(svyglm(
      f_adj,
      design = sub_design,
      family = current_family
    ))
    
    # construct systematic names
    base_name <- paste("svy", out_short, exp_short, prof_short, sep = "_")
    
    name_crude <- paste0(base_name, "_cr")
    file_crude <- file.path(output_dir, paste0(name_crude, ".rds"))
    
    if (file.exists(file_crude)) {
      
      message(glue("Loading from disk: {name_crude}"))
      m_crude <- readRDS(file_crude)
    
      } else {
        
        message(glue("Fitting new model: {name_crude}"))
        
        m_crude <- try(svyglm(
          f_crude,
          design = sub_design,
          family = current_family
          ))
      
      if (!inherits(m_crude, "try-error")) {
        saveRDS(m_crude, file_crude)
      }
    }
    
    assign(name_crude, m_crude, envir =.GlobalEnv)
    
    name_str <- paste0(base_name, "_str")
    file_str <- file.path(output_dir, paste0(name_str, ".rds"))
    
    if (file.exists(file_str)) {
      
      message(glue("Loading from disk: {name_str}"))
      m_str <- readRDS(file_str)
      
      } else {
        
        message(glue("Fitting new model: {name_str}"))
        
        m_str <- try(svyglm(
          f_str,
          design = sub_design,
          family = current_family
          ))
      
      if (!inherits(m_str, "try-error")) {
        saveRDS(m_str, file_str)
      }
    }
    
    assign(name_str, m_str, envir = .GlobalEnv)
    
    
    name_adj <- paste0(base_name, "_adj")
    file_adj <- file.path(output_dir, paste0(name_adj, ".rds"))
    
    if (file.exists(file_adj)) {
      
      message(glue("Loading from disk: {name_adj}"))
      m_adj <- readRDS(file_adj)
      
      } else {
        
        message(glue("Fitting new model: {name_adj}"))
        
        m_adj <- try(svyglm(
          f_adj, 
          design = sub_design, 
          family = current_family
          ))
      
      if (!inherits(m_adj, "try-error")) {
        saveRDS(m_adj, file_adj)
      }
    }
    
    assign(name_adj, m_adj, envir = .GlobalEnv)
    
    
    return(invisible(TRUE))
  }

## Survey results table----------------------------------------
# Function to build table with survey results

build_svy_gt <- 
  function(data, metric_type, title_text, footnote_text) {
  
  # prepare data
  wide_df <- 
    data |> 
    filter(Type == metric_type) |> 
    mutate(
      Outcome = factor(Outcome, levels = outcome_order),
      Model = factor(Model, levels = c("Crude", "Partially", "Fully Adj.")),
      Exposure = factor(Exposure, levels = exposure_order)
    ) |> 
    arrange(Outcome, Exposure, Model) |>
    select(Outcome, Exposure, Profession, Model, Est_CI) |> 
    pivot_wider(
      names_from = c(Profession, Model),
      values_from = Est_CI,
      names_sep = "_"
    )
  
  # build gt table
  wide_df |> 
    gt(groupname_col = "Outcome", 
       rowname_col = "Exposure") |> 
    
    # Titles
    tab_header(title = md(title_text)) |> 
    
    # Spanners
    tab_spanner(
      label = md("**Doctor**"),
      columns = starts_with("Doctor")
    ) |> 
    tab_spanner(
      label = md("**Nurse**"),
      columns = starts_with("Nurse")
    ) |> 
    
    # clean names
    cols_label(
      ends_with("_Crude") ~ "Crude",
      ends_with("_Partially") ~ "Partially adjusted",
      ends_with("_Fully Adj.") ~ "Fully adjusted"
    ) |> 
    
    # Formatting
    cols_align(
      align = "center", 
      columns = contains("_")) |> 
    tab_stub_indent(
      rows = everything(), 
      indent = 3) |> 
  tab_style(
      style = cell_text(align = "left"),
      locations = cells_stub()
    ) |>
    tab_footnote(
      footnote = footnote_text
    ) |> 
    tab_footnote(
        locations = cells_column_labels(columns = contains("_Crude")),
        footnote = "No covariates included."
      ) |> 
      tab_footnote(
        locations = cells_column_labels(columns = contains("_Part")),
        footnote = "Adjusted by Gender, Age group and Proximity to combat areas and occupied territories ."
      ) |> 
      tab_footnote(
        locations = cells_column_labels(columns = contains("_Full")),
        footnote =
          paste0("Partially adjusted models plus specific covariates:", "\n",
                 "Bullying and sexual harassment adjusted by Healthcare setting, Post-graduate training and Length of service.", "\n",
                 "Violent threats and physical violence adjusted by Healthcare setting.", "\n",
                 "Support from colleagues or superiors adjusted by Healthcare setting and Length of service.", "\n",
                 "Finding meaning or purpose in job adjusted by Healthcare setting, Relationship status, Having children, and Length of service.")
      )
  }

## Multi-level models with robust SEs  --------------------------------------- 
# Crude models

run_crude_glm <- 
  function(data, outcome_var, exposure_var, profession, 
           cluster_var, family_type = "poisson",
           output_dir = "out/glm_models") {
    
    if(!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }
    
    out_short <- case_when(
      outcome_var == "phq_co"      ~ "dep",
      outcome_var == "gad_co"      ~ "anx",
      outcome_var == "suic_idea"   ~ "suic"
      )
    
    # exposure abbreviation
    exp_short <- case_when( 
      exposure_var == "work_21_dic"   ~ "cols",
      exposure_var == "work_22_dic"   ~ "sup",
      exposure_var == "work_30_dic"   ~ "har",
      exposure_var == "work_33_dic"   ~ "bul",
      exposure_var == "work_31_dic"   ~ "threats",
      exposure_var == "work_32_dic"   ~ "viol",
      exposure_var == "wb_wami_1_dic" ~ "mean",
      exposure_var == "wb_wami_2_dic" ~ "purp"
      )
    
    # profession abbreviation
    prof_short <- case_when(
      profession == "Doctor" ~ "doc",
      profession == "Nurse"  ~ "nur"
      )
    
    # stratify data
    ds_sub <- data |> 
      filter(work_2 == profession)
    
    # Clean
    req_vars <- c(outcome_var, exposure_var, cluster_var)
    
    
    
    ds_clean <- ds_sub |> 
      select(all_of(req_vars)) |> 
      drop_na() 
    
    
    n_size <- nrow(ds_clean)
    
    # calculate absolute risks/means 
    y_vals <- as.numeric(ds_clean[[outcome_var]]) 
    x_vals <- as.character(ds_clean[[exposure_var]])
    
    if (family_type != "gaussian") {
      # ensure it is 0/1
      if(max(y_vals, na.rm = TRUE) > 1) y_vals <- y_vals - 1
      
      # calculations
      risk_unexp_val <- mean(y_vals[x_vals == "No"], na.rm = TRUE) * 100
      risk_exp_val   <- mean(y_vals[x_vals == "Yes"], na.rm = TRUE) * 100
      
      risk_unexp_str <- sprintf("%.1f%%", risk_unexp_val)
      risk_exp_str   <- sprintf("%.1f%%", risk_exp_val)
      
      } else {
        
        mean_unexp_val <- mean(y_vals[x_vals == "No"], na.rm = TRUE)
        mean_exp_val   <- mean(y_vals[x_vals == "Yes"], na.rm = TRUE)
        
        risk_unexp_str <- sprintf("%.2f", mean_unexp_val)
        risk_exp_str   <- sprintf("%.2f", mean_exp_val)
        }
    
    # Define family
    
    if (family_type == "gaussian") {
      curr_family <- gaussian()
      } else {
        curr_family <- poisson(link = "log")
        }
    
    # Define names
    file_name_ml <- paste0("glm_", out_short, "_", exp_short, "_", prof_short, 
                        "_cr_ml", ".rds")
    
    file_path_ml <- file.path(output_dir, file_name_ml)
    
    # Run or load
    if(file.exists(file_path_ml)) {
      
      message(glue("Loading ML model: {file_name_ml}"))
      ml <- readRDS(file_path_ml)
    
      } else {
        message(glue("Fitting ML model: {file_name_ml}"))
        
        
        f_ml <- as.formula(
          paste0(outcome_var, " ~ " , exposure_var, 
                 " + (1 | ", cluster_var, ")"))
        
        ml <- glmer(
          f_ml, 
          data = ds_clean, 
          family = curr_family,
          control = glmerControl(
            optimizer = "bobyqa", 
            optCtrl = list(maxfun = 2e5)
            )
          )
        
        
        saveRDS(ml, file_path_ml)
    }
    
    
    # Names
    file_name_rob <-  paste0("glm_", out_short, "_", exp_short, "_", prof_short, 
                             "_cr_rob", ".rds")
    
    file_path_rob <- file.path(output_dir, file_name_rob)
    
    # Run or load
    if(file.exists(file_path_rob)) {
      
      message(glue("Loading Robust model: {file_name_rob}"))
      rob <- readRDS(file_path_rob)
      
      } else {
        message(glue("Fitting Robust model: {file_name_rob}"))
        
        f_rob <- as.formula(
          paste0(outcome_var, " ~ " , exposure_var))
        
        rob <- glm(
          f_rob, 
          data = ds_clean, 
          family = curr_family)
        
        
        saveRDS(rob, file_path_rob)
        
        }
      
    
    # Extract for ml
    coef_ml <- summary(ml)$coefficients
    
    beta_ml <- coef_ml[paste0(exposure_var, "Yes"), "Estimate"]
    
    se_ml   <- coef_ml[paste0(exposure_var, "Yes"), "Std. Error"]
    
    pr_ml <-  exp(beta_ml)
    
    ci_ml <- exp(beta_ml + c(-1, 1) * 1.96 * se_ml)
        
    # Generate data
    
    ml_results <- tibble(
      
      model = "Multilevel Poisson",
      
      Outcome = outcome_var,
      
      Exposure = exposure_var,
      
      Profession = profession,
      
      Adjustment = "Crude",
      
      beta  = beta_ml,
      
      SE    = se_ml,
      
      PR    = pr_ml,
      
      CI_l  = ci_ml[1],
      
      CI_u  = ci_ml[2],
      
      N_Analysis = n_size, # number of observations used
      
      Risk_Unexp = risk_unexp_str,
      
      Risk_Exp = risk_exp_str
      
    )
    
    
    # Extract for robust SE
    
    vcov_rob <- sandwich::vcovCL(rob, cluster = ds_clean[[cluster_var]])
    
    coef_glm <- summary(rob)$coefficients
    
    beta_rb <- coef_glm[paste0(exposure_var, "Yes"), "Estimate"]
    
    se_rb <- sqrt(vcov_rob[paste0(exposure_var, "Yes"), 
                           paste0(exposure_var, "Yes")])
    
    pr_rb <- exp(beta_rb)
    
    ci_rb <- exp(beta_rb + c(-1, 1) * 1.96 * se_rb)
    
    rb_results <- tibble(
      
      model = "Poisson + RSVE",
      
      Outcome = outcome_var,
      
      Exposure = exposure_var,
      
      Profession = profession,
      
      Adjustment = "Crude",
      
      beta  = beta_rb,
      
      SE    = se_rb,
      
      PR    = pr_rb,
      
      CI_l  = ci_rb[1],
      
      CI_u  = ci_rb[2],
      
      N_Analysis = n_size # number of observations used
    )
    
    return(list(
      multilevel = ml_results,
      robust     = rb_results
    ))
    

  }



# Function for adjusted models


run_glm_adj <- 
  function(data, outcome_var, exposure_var, confounder_list, profession,
           cluster_var, family_type = "poisson", model_label,
           output_dir = "out/glm_models") {
    
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }
    
    
    ds_sub <- data |> 
      filter(work_2 == profession)
    
    # remove work_2 cause it is a constant
    active_confounders <- setdiff(confounder_list, "work_2")
    
    # define clean set
    required_vars <- unique(c(outcome_var, exposure_var, cluster_var, 
                              active_confounders))
    
    out_short <- case_when(
      outcome_var == "phq_co"      ~ "dep",
      outcome_var == "gad_co"      ~ "anx",
      outcome_var == "suic_idea"   ~ "suic"
      )
    
    # exposure abbreviation
    exp_short <- case_when( 
      exposure_var == "work_21_dic"   ~ "cols",
      exposure_var == "work_22_dic"   ~ "sup",
      exposure_var == "work_30_dic"   ~ "har",
      exposure_var == "work_33_dic"   ~ "bul",
      exposure_var == "work_31_dic"   ~ "threats",
      exposure_var == "work_32_dic"   ~ "viol",
      exposure_var == "wb_wami_1_dic" ~ "mean",
      exposure_var == "wb_wami_2_dic" ~ "purp"
      )
    
    
    # profession abbreviation
    prof_short <- case_when(
      profession == "Doctor" ~ "doc",
      profession == "Nurse"  ~ "nur"
      )
    
    
    ds_clean <- ds_sub |> 
      select(all_of(required_vars)) |> 
      drop_na()
    
    
    n_size <- nrow(ds_clean)
    
    if(n_size < 10) return(NULL)
    
    # Convert Exposure to Factor for prediction consistency
    ds_clean[[exposure_var]] <- 
      
      factor(ds_clean[[exposure_var]], 
             levels = c("No", "Yes"))
    
    # Convert Outcome to 0/1 for Poisson/Logit
    y_raw <- as.numeric(ds_clean[[outcome_var]])
    
    if(max(y_raw, na.rm = TRUE) > 1) {
      ds_clean[[outcome_var]] <- y_raw - 1
    }
    
    # Define family 
    if (family_type == "gaussian") {
      curr_family <- gaussian()
      } else {
        curr_family <- poisson(link = "log")
        }
    
    
    # Naming logic for each adjustment
    
    model_suffix <- case_when(
      model_label == "Partially adjusted" ~ "_str",
      model_label == "Fully adjusted" ~ "_adj",
      TRUE ~ tolower(gsub(" ", "_", model_label))
    )
    
    
    # Define names
    file_name_ml <- paste0("glm_", out_short, "_", exp_short , "_", prof_short, 
                        model_suffix, "_ml.rds")
    file_path_ml <- file.path(output_dir, file_name_ml)
    
    
    # run or load
    if(file.exists(file_path_ml)) {
      
      message(glue("Loading ML model: {file_name_ml}"))
      ml <- readRDS(file_path_ml)
      
    } else {
      message(glue("Fitting ML model: {file_name_ml}"))
      
      
      f_ml <- as.formula(
        paste0(outcome_var, " ~ " , exposure_var, "+", 
               paste(active_confounders, collapse = "+"),
               " + (1 | ", cluster_var, ")"))
      
      ml <- glmer(
        f_ml, 
        data = ds_clean, 
        family = curr_family,
        control = glmerControl(
          optimizer = "bobyqa", 
          optCtrl = list(maxfun = 2e5)
        )
      )
      
      
      saveRDS(ml, file_path_ml)
    }
    
    
    # Define names
    file_name_rob <- paste0("glm_", out_short, "_", exp_short , "_", prof_short, 
                           model_suffix, "_rob.rds")
    file_path_rob <- file.path(output_dir, file_name_rob)
    
    
    # run or load
    if(file.exists(file_path_rob)) {
      
      message(glue("Loading Robust SE model: {file_name_rob}"))
      rob <- readRDS(file_path_rob)
      
    } else {
      message(glue("Fitting Robust SE model: {file_name_rob}"))
      
      
      f_rob <- as.formula(
        paste0(outcome_var, " ~ " , exposure_var, "+", 
               paste(active_confounders, collapse = "+"),
               " + factor(", cluster_var, ")"))
      
      rob <- glm(    # robust needs to be glm not glmer
        f_rob, 
        data = ds_clean, 
        family = curr_family
      )
      
      
      saveRDS(rob, file_path_rob)
    }
    
    # Calculate adjusted risks (Marginal standardization)
    # Use ML as it accounts for the cluster variance structure
    
    # synthetic ds
    dat_unexp <- ds_clean
    dat_unexp[[exposure_var]] <- factor("No", levels = c("No", "Yes"))
    
    dat_exp <- ds_clean
    dat_exp[[exposure_var]] <- factor("Yes", levels = c("No", "Yes"))
    
    # Predict: Average predicted probability if everyone UNEXPOSED
    pred_unexp <- 
      
      predict(
        ml, 
        newdata = dat_unexp, 
        type = "response", 
        re.form = NA
        )
    
    
    pred_exp   <- 
      
      predict(
        ml, 
        newdata = dat_exp,   
        type = "response", 
        re.form = NA
        )
    
    risk_unexp_str <- sprintf("%.1f%%", mean(pred_unexp, na.rm = TRUE) * 100)
    
    risk_exp_str   <- sprintf("%.1f%%", mean(pred_exp,   na.rm = TRUE) * 100)
    
    # Extract for ml
    coef_ml <- summary(ml)$coefficients
    
    beta_ml <- coef_ml[paste0(exposure_var, "Yes"), "Estimate"]
    
    se_ml   <- coef_ml[paste0(exposure_var, "Yes"), "Std. Error"]
    
    pr_ml <-  exp(beta_ml)
    
    ci_ml <- exp(beta_ml + c(-1, 1) * 1.96 * se_ml)
    
    # Generate data
    
    ml_results <- tibble(
      
      model = "Multilevel Poisson",
      
      Outcome = outcome_var,
      
      Exposure = exposure_var,
      
      Profession = profession,
      
      Adjustment = model_label,
      
      beta  = beta_ml,
      
      SE    = se_ml,
      
      PR    = pr_ml,
      
      CI_l  = ci_ml[1],
      
      CI_u  = ci_ml[2],
      
      N_Analysis = n_size, # number of observations used
      
      Risk_Unexp = risk_unexp_str,
      
      Risk_Exp = risk_exp_str
      
    )
    
    
    # Extract for robust SE
    
    vcov_rob <- sandwich::vcovCL(rob, cluster = ds_clean[[cluster_var]])
    
    coef_glm <- summary(rob)$coefficients
    
    beta_rb <- coef_glm[paste0(exposure_var, "Yes"), "Estimate"]
    
    se_rb <- sqrt(vcov_rob[paste0(exposure_var, "Yes"), 
                           paste0(exposure_var, "Yes")])
    
    pr_rb <- exp(beta_rb)
    
    ci_rb <- exp(beta_rb + c(-1, 1) * 1.96 * se_rb)
    
    rb_results <- tibble(
      
      model = "Poisson + RSVE",
      
      Outcome = outcome_var,
      
      Exposure = exposure_var,
      
      Profession = profession,
      
      Adjustment = model_label,
      
      beta  = beta_rb,
      
      SE    = se_rb,
      
      PR    = pr_rb,
      
      CI_l  = ci_rb[1],
      
      CI_u  = ci_rb[2],
      
      N_Analysis = n_size # number of observations used
    )
    
    return(list(
      multilevel = ml_results,
      robust     = rb_results
    ))
  }


##  GLM Results ------------------------------------------------------------
# Build GLM results table

make_stratified_table <- 
  function(data, title_text, note_text) {
    
    wide_df <- 
      data |> 
      mutate(Outcome = factor(Outcome, outcome_order),
             Exposure = factor(Exposure, 
                                     levels = exposure_order)) |> 
      mutate(Adjustment = factor(Adjustment, levels = adjustment_order)) |> 
      arrange(Exposure, Adjustment) |> 
      select(Outcome, Exposure, Profession, Adjustment, Est_CI) |> 
      pivot_wider(
        names_from = c(Profession, Adjustment),
        values_from = Est_CI,
        names_sep = "_"
      )
    
    wide_df |> 
      gt(groupname_col = "Outcome",
         rowname_col = "Exposure") |> 
      tab_header(title = md(title_text)) |>
      tab_spanner(
        label = md("**Doctor**"),
        columns = starts_with("Doctor")
      ) |> 
      tab_spanner(
        label = md("**Nurse**"),
        columns = starts_with("Nurse")
      ) |> 
      cols_label(
        ends_with("_Crude") ~ "Crude",
        ends_with("Partially adjusted") ~ "Partially adjusted",
        ends_with("Fully adjusted") ~ "Fully adjusted"
      ) |> 
      cols_align(
        align = "center",
        columns = contains(c("Doctor_", "Nurse_"))
      ) |> 
      tab_style(
        style = cell_text(align = "left"),
        locations = cells_stub()
        ) |>
      tab_stub_indent(
        rows = everything(), indent = 3
      ) |> 
      tab_footnote(footnote = note_text) |> 
      tab_footnote(
        locations = cells_column_labels(columns = contains("_Crude")),
        footnote = "Region as second-order covariate only."
      ) |> 
      tab_footnote(
        locations = cells_column_labels(columns = contains("_Part")),
        footnote = "Adjusted by Gender, Age group and Proximity to combat areas and occupied territories ."
      ) |> 
      tab_footnote(
        locations = cells_column_labels(columns = contains("_Full")),
        footnote =
          paste0("Partially adjusted models plus specific covariates:", "\n",
                 "Bullying and sexual harassment adjusted by Healthcare setting, Post-graduate training and Length of service.", "\n",
                 "Violent threats and physical violence adjusted by Healthcare setting.", "\n",
                 "Support from colleagues or superiors adjusted by Healthcare setting and Length of service.", "\n",
                 "Finding meaning or purpose in job adjusted by Healthcare setting, Relationship status, Having children, and Length of service.")
      )
  }



# GLM Dose-Response models ------------------------------------------------
# Dose-response GLM models

run_glm_dose_emm <- 
  function(data, outcome_var, exposure_var, confounder_list,
           cluster_var = "loc_3", family_type = "gaussian",
           model_label, output_dir = "out/glm_dose", force_fit = FALSE) {
    
    if(!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
    
    
    required_vars <- unique(c(outcome_var, exposure_var, 
                            cluster_var, confounder_list))
    
    # Clean dataset
    
    ds_clean <-
      
      data |>
      select(all_of(required_vars)) |>
      drop_na()
    
    
    n_size <- nrow(ds_clean)
    
    if (n_size < 10) return(NULL)
  
    # Short labels
  
    out_short <- case_when(
      outcome_var == "phq_sc"   ~ "dep",
      outcome_var == "gad_sc"   ~ "anx",
      outcome_var == "mh_phq_9" ~ "suic"
      )
  
  
  
    # exposure abbreviation
    exp_short <- case_when( 
      exposure_var == "work_21"   ~ "cols",
      exposure_var == "work_22"   ~ "sup",
      exposure_var == "work_30"   ~ "har",
      exposure_var == "work_33"   ~ "bul",
      exposure_var == "work_31"   ~ "threats",
      exposure_var == "work_32"   ~ "viol",
      exposure_var == "wb_wami_1" ~ "mean",
      exposure_var == "wb_wami_2" ~ "purp"
      )
    
    
    file_name <- paste0("dose_", out_short, "_", exp_short, "_",
                      tolower(gsub(" ", "_", model_label)), ".rds")
    
    file_path <- file.path(output_dir, file_name)
    
    # Fit or load model
    
    if(file.exists(file_path) & !force_fit){
      
      message(glue("Loading saved bundle: {file_name}"))
      bundle <- readRDS(file_path)
      model <- bundle$model
      balance_report <- bundle$balance
      
      } else {
        
        message(glue("Fitting Dose-response: {file_name}"))
        balance_report <- NULL
        
        model_formula <- as.formula(
          paste(outcome_var, "~",
                paste(c(exposure_var, confounder_list), collapse = " + "),
                "+ (1 |", cluster_var, ")"
                )
          )
        
        
        model <- lmer(
          model_formula,
          data = ds_clean,
          control = lmerControl(optimizer = "bobyqa")
          )
        
        saveRDS(list(model = model, balance = balance_report), file_path)
        
        }
    
    
    # Extract EMMs
    
    emm_obj <- try(
      emmeans(
        model,
        specs = exposure_var,
        type = "response",
        df = NA,      # Use NA or asymptotic for large datasets to save time
        rg.limit = 20000  # <--- INCREASE LIMIT HERE (Standard is 10000)
        ),
      silent = TRUE
      )
    
    if (inherits(emm_obj, "try-error")) {
      
      message(glue("Error calculating EMMs for {exposure_var}: {emm_obj}"))
      
      return(NULL)
      }
    
    
    emm_df <-
      as.data.frame(emm_obj) |>
      rename(estimate = emmean, std.error = SE) |>
      mutate(
        conf.low = estimate - 1.96 * std.error,
        conf.high = estimate + 1.96 * std.error,
        Level = as.character(.data[[exposure_var]]),
        Outcome = outcome_var,
        Exposure = exposure_var,
        Model = model_label,
        N_Analysis = n_size
        ) |>
      select(Outcome, Exposure, Level, estimate, conf.low, conf.high, Model,
             N_Analysis)
    
    
    if(is.null(emm_df) || nrow(emm_df) == 0){
      
      message(glue("Warning: No EMMs found for {exposure_var}"))
      
      return(NULL)
    }
    
    return(emm_df)
    
    }
