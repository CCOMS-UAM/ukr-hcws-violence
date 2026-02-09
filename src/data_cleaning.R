# Data cleaning script

# Required packages

library(tidyverse)
library(stringi)

#### Read data file and codebook ####

ds <- 
  read.csv("data/who-survey.csv", 
           na.strings = "",
           sep = ";") |>  
  tibble()

cb <- 
  read.csv("ext/data_mapping.csv",
           na.strings = "")  |>  
  
  # this will be used later
  
  mutate(answer_num = as.numeric(gsub(".*_(\\d+)$", "\\1", answer_uuid))) |> 
  tibble()

#### Initial transformations ####

# issues with data collection

ds <- 
  ds |> 
  mutate(
    
    # responses to CL scale were being codified as NAs in
    # this particular case
    
    wb_cl = if_else(!is.na(wb_who_5) & is.na(wb_cl), 5, wb_cl)
    ) |> 
  
  # Problematic free-text replies to number of weeks on sick leave
  
  mutate(work_36_a = if_else(work_36_a > 52,
                             NA,
                             as.integer(work_36_a))) |> 
  
  # Mutation in case we have few "other" responses
  mutate(scdm_2_rec = if_else(scdm_2 %in% c("Female", "Male"),
                              scdm_2,
                              NA)) |> 
  
  # First responses had a wrong translation for Ukraine
  mutate(loc_2 = case_when(
    loc_2 == "Ukrania" ~ "Ukraine",
    TRUE ~ loc_2
  )) |> 
  
  # Some regions got mixed for a couple responses:
  
  mutate(loc_2 = if_else(
    loc_3 == "Chișinău Ciocana" & loc_2 != "Moldova",
    "Moldova",
    loc_2),
  loc_2 = if_else(
    loc_3 == "თბილისი" & loc_2 != "Georgia",
    "Georgia",
    loc_2),
  loc_2 = if_else(
    loc_3 == "Ар Крим" & loc_2 != "Ukraine",
    "Ukraine",
    loc_2)
  )
  
  # Some responses have loc_3 but not loc_2

regions <- 
  readxl::read_excel("ext/regions_full.xlsx")

# Define which belong where

am_reg <- 
  regions |> 
  select(Armenia) |> 
  drop_na() |> 
  mutate(Armenia = str_trim(Armenia)) |> 
  pull(Armenia)
  
ge_reg <- 
  regions |> 
  select(Georgia) |> 
  drop_na() |> 
  mutate(Georgia = str_trim(Georgia)) |> 
  pull(Georgia)
  
md_reg <- 
  regions |> 
  select(Moldova) |> 
  drop_na() |> 
  mutate(Moldova = str_trim(Moldova)) |> 
  pull(Moldova)

ua_reg <- 
  regions |> 
  select(Ukraine) |> 
  drop_na() |> 
  mutate(Ukraine = str_trim(Ukraine)) |> 
  pull(Ukraine)
  
# define in ds

ds <-

  ds |>
  mutate(loc_2 = case_when(
    loc_3 %in% am_reg ~ "Armenia",
    loc_3 %in% ge_reg ~ "Georgia",
    loc_3 %in% md_reg ~ "Moldova",
    loc_3 %in% ua_reg ~ "Ukraine",
    TRUE ~ loc_2)
  )|>
  
  # Some do not have loc_2, loc_3, if language corresponds to IP, assign
  
  mutate(loc_2 = case_when(
    loc_1 == "Armenian" & is.na(loc_2) & country_by_ip == "AM" ~ "Armenia",
    loc_1 == "Georgian" & is.na(loc_2) & country_by_ip == "GE" ~ "Georgia",
    loc_1 == "Romanian" & is.na(loc_2) & country_by_ip == "MD" ~ "Moldova",
    loc_1 == "Ukrainian" & is.na(loc_2) & country_by_ip == "UA" ~ "Ukraine",
    TRUE ~ loc_2
  ))

rm(am_reg, ge_reg, md_reg, ua_reg)

# characters into integers

ds <- 
  ds|> 
  mutate(
    across(
      contains(c("_gad_", "_phq_")),
      \(x) recode(x,
                  "Not at all" = "0",
                  "Several days" = "1",
                  "More than half of the days" = "2",
                  "Nearly every day" = "3")  |> as.integer()),
    across(
      contains("_who_"),
      \(x) recode(x,
                  "At no time" = "0",
                  "Some of the time" = "1",
                  "Less than half of the time" = "2",
                  "More than half of the time" = "3",
                  "Most of the time" = "4",
                  "All of the time" = "5") |>  
        as.integer()),
    across(
      contains("_cage_"),
      \(x) recode(x,
                  "Yes" = "1",
                  "No" = "0")  |>  
        as.integer())
  )

# dates

ds <- 
  ds |> 
  mutate(date = ymd(date))


# Recoding "other (specify)"
# Recoding work setting

ds <- 
  ds |> 
  mutate(work_5_aux = work_5) 

work_set <-
  cb |> 
  filter(question_uuid == "work_5") |> 
  pull(answer)  # Creates object with names of specialties

ds <- 
  ds |> 
  mutate(work_5 = case_when(
    is.na(work_5) ~ NA_character_,
    work_5 %in% work_set ~ work_5,  # If matches with names, assigns it
    TRUE ~ "Other"  # Else, assign "other"
  ))

rm(work_set)

# Assigning levels

cb_red <- 
  cb |> 
  filter(str_detect(answer, "specify"),
         !str_detect(question_uuid, "work_2_a")) |> # leaves out med spec
  select(question_uuid, answer_num)


levels <- c(scdm_2 = 2, # requires 1 level less than the number of levels (3)
            work_5 = 6) # IBID 

ds <- 
  ds |> 
  mutate(scdm_2_aux = scdm_2, 
         work_5_aux = work_5) 
 

ds <- 
  ds |> 
  mutate(
    across(
      .cols = all_of(cb_red$question_uuid),
      .fns = \(x) fct_lump(x, n = levels[[cur_column()]]),
      .names = "{.col}"
    )
  )

rm(cb_red,
   levels)

# Recoding medical specialties

ds <- 
  ds |> 
  mutate(work_2_a_aux = work_2_a) 

med_spec <-
  cb |> 
  filter(question_uuid == "work_2_a") |> 
  pull(answer)  # Creates object with names of specialties

ds <- 
  ds |> 
  mutate(work_2_a = case_when(
    is.na(work_2_a) ~ NA_character_,
    work_2_a %in% med_spec ~ work_2_a,  # If matches with names, assigns it
    TRUE ~ "Other specialty"  # Else, assign "other"
  ))

rm(med_spec)


# Check "other" specialties:


# ds_med_spec_other <- 
#   ds |> 
#   filter(work_2_a == "Other specialty")
# 
# ds_med_spec_other |> 
#   filter(loc_1 == "Spanish") |> 
#   group_by(work_2_a_aux) |> 
#   summarise() |> 
#   print(n = 350)

# Factors

factor_levels <- 
  
  cb |>  
  group_by(question_uuid)  |>  
  arrange(answer_num) |>   # Sort by the numerical part of answer_uuid 
  summarize(levels = list(answer)) |>  # Assign levels to corresponding items
  deframe() 

level_vars <- c("scdm_1", 
                "scdm_3", 
                "scdm_5_a",
                "scdm_6",
                "work_1_a", 
                "work_2_b", 
                "work_3",
                "work_4",
                "work_8",
                "work_10",
                "work_11",
                "work_15",
                "work_19",
                "work_20",
                "work_21",
                "work_22",
                "work_23",
                "work_24",
                "work_30",
                "work_31",
                "work_32",
                "work_33",
                "work_34",
                "work_35",
                "wb_wami_1",
                "wb_wami_2") 

ds <- 
  ds |>  
  mutate(
    across(
      all_of(level_vars),
      ~factor(.x, 
              levels = factor_levels[[cur_column()]], 
              ordered = TRUE)
      )
    )

rm(factor_levels,
   level_vars)

ds <- 
  ds |>  
  mutate(
    across(
      where(is.character),
      as.factor
    )
  )

# issue with capitalisation in work_22

ds <- 
  
  ds |> 
  mutate(work_22 = fct_recode(work_22, 
                              "Never / Hardly ever" = "Never/hardly ever"))

# Some need reversing of levels
rev_vars = c("work_19",
             "work_20",
             "work_21",
             "work_22",
             "work_23",
             "work_24",
             "work_25",
             "work_26",
             "work_27",
             "work_28",
             "work_29",
             "work_30",
             "work_31",
             "work_32",
             "work_33",
             "work_34",
             "work_35")

ds <- 
  ds |> 
  mutate(
    across(
      all_of(rev_vars),
      fct_rev
    )
  )

rm(rev_vars)

# some need reorder

#### New variables ####

# Age recoded (matches EUROSTAT datasets) 

ds <-
  ds |>
  mutate(
    age_eurostat = case_when(
      scdm_1 %in% c("Less than 20 years old",
                    "20-25",
                    "26-30",
                    "31-35")          ~ "Less than 35 years",
      scdm_1 %in% c("36-40", "41-45") ~ "From 35 to 44 years",
      scdm_1 %in% c("46-50", "51-55") ~ "From 45 to 54 years",
      scdm_1 %in% c("56-60", "61-65") ~ "From 55 to 64 years",
      scdm_1 %in% c("66-70")          ~ "From 65 to 74 years",
      scdm_1 %in% c("Over 70")    ~ "75 years or over",
      TRUE                            ~ scdm_1
    ) |>
      factor(
        levels = c("Less than 35 years",
                   "From 35 to 44 years",
                   "From 45 to 54 years",
                   "From 55 to 64 years",
                   "From 65 to 74 years",
                   "75 years or over"),
        ordered = FALSE # Required to match ecological sets
      )
  )
# 
# # Age recoded (fewer categories)
# 
# ds <- 
#   
#   ds  |> 
#   mutate(
#     scdm_1_rec = case_when(
#       scdm_1 %in% c("Less than 20 years old", "20-25", "26-30") ~ "<30",
#       scdm_1 %in% c("31-35", "36-40")                           ~ "30-39",
#       scdm_1 %in% c("41-45", "46-50")                           ~ "40-49",
#       scdm_1 %in% c("51-55", "56-60")                           ~ "50-59",
#       scdm_1 %in% c("61-65", "66-70", "Over 70")                ~ "60+",
#       TRUE                                                      ~ NA_character_
#     ),
#     scdm_1_rec = factor(
#       scdm_1_rec,
#       levels = c("<30", "30-39", "40-49", "50-59", "60+"),
#       ordered = TRUE
#     )
#   )


# Look for invalid or incomplete surveys based on thresholds
ds <- 
  ds |> 
  mutate(na_count = rowSums(is.na(ds)),
         na_perc = 
           rowSums(is.na(ds)) / 
           (rowSums(is.na(ds)) + rowSums(!is.na(ds)))*100,
         complete = if_else(!is.na(wb_cl),  # If last item is answered
                            "complete",
                            "not complete") |> 
           as.factor(),
         valid_survey = if_else(na_perc < 90,
                                "valid",
                                "not valid") |> 
           as.factor() |> fct_rev() 
         )

ds <- 
  ds |> 
  mutate(date_time = ymd_hms(paste(ds$date, sep = " ", ds$time_first_answer)))

# Another adjustment
ds <- 
  
  ds |>   
  # Many responses from Ukraine do not contain loc_2 or loc_3
  
  mutate(loc_2 = if_else(
    country_by_ip == "UA" &
      is.na(loc_2) &
      na_perc < 75,
    "Ukraine",
    loc_2
  )) 
  

# Assign scores, cut-offs and categories to tools


ds <- 
  ds |> 
  mutate(phq_sc = rowSums(  # Total score                 
    across(                 # rowSums has to be the 1st verb
      contains("_phq_")), na.rm = FALSE),   # NA if incomplete
    phq_co = case_when(       # Cut-off values
      phq_sc >= 10 ~ "Yes",
      is.na(phq_sc) ~ NA_character_, 
      TRUE ~ "No"
    ) |> as.factor(),      
    phq_cat = case_when(phq_sc < 4 ~ "Minimal symptoms",
                        phq_sc >= 4 & phq_sc < 10 ~ "Mild symptoms",
                        phq_sc >= 10 & phq_sc < 14 ~ "Moderate symptoms",
                        phq_sc >= 14 & phq_sc < 20 ~ "Moderately severe symptoms", 
                        phq_sc >= 20 ~ "Severe symptoms"), # Categories
    phq_cat = as.ordered(phq_cat)
  )

ds <- 
  ds |> 
  mutate(gad_sc = rowSums(
    across(
      contains("_gad_")), na.rm = FALSE),
    gad_co = case_when(
      gad_sc >= 10 ~ "Yes",
      is.na(phq_sc) ~ NA_character_,
      TRUE ~ "No"
    ) |> as.factor(),
    gad_cat = case_when(gad_sc < 4 ~ "Minimal symptoms",
                        gad_sc >= 4 & gad_sc < 10 ~ "Mild symptoms",
                        gad_sc >= 10 & gad_sc < 14 ~ "Moderate symptoms",
                        gad_sc >= 14 & gad_sc < 20 ~ "Moderately severe symptoms", 
                        gad_sc >= 20 ~ "Severe symptoms"),
    gad_cat = as.ordered(gad_cat)
  )

ds <- 
  ds |> 
  mutate(cage_sc = rowSums(
    across(
      contains("_cage_")), na.rm = FALSE),
    cage_co = as_factor(if_else(cage_sc > 2,
                                "Yes",
                                "No")) # Cut-off as defined in literature.
  )   # Didn't create categories as literature does not define them.

ds <- 
  
  ds |> 
  mutate(wb_who_sc = rowSums(
    across(
      contains("wb_who_")), na.rm = FALSE)*4, # Needs to be multiplied
    wb_who_co = as_factor(if_else(wb_who_sc >= 50,
                                  "Yes",
                                  "No"))  # Cut-off as defined in literature.
  )  # Same situation with categories as with CAGE.

# Determine validity of questionnaires

ds <- 
  ds |> 
  mutate(
    gad_comp = case_when( # Must be fully answered
      if_all(mh_gad_1:mh_gad_7, ~ !is.na(.)) ~ "Complete GAD-7",
      TRUE ~ "Incomplete GAD-7") |> as.factor(),
    phq_comp = case_when(
      if_all(mh_phq_1:mh_phq_9, ~ !is.na(.)) ~ "Complete PHQ-9",
      TRUE ~ "Incomplete PHQ-9") |> as.factor(),
    cage_comp = case_when(
      if_all(mh_cage_1:mh_cage_4, ~ !is.na(.)) ~ "Complete CAGE",
      TRUE ~ "Incomplete CAGE") |> as.factor(),
    whowb_comp = case_when(
      if_all(wb_who_1:wb_who_5, ~ !is.na(.)) ~ "Complete WHO-5",
      TRUE ~ "Incomplete WHO-5") |> as.factor())

## Suicidal ideation ##

# Item 9 of PHQ-9 asks “Over the last two weeks, how often have you been 
# bothered by thoughts that you would be better off dead or of hurting 
# yourself in some way?”

# A study found that having any score in this particular question increased
# the risk for suicide within 30 days or one year. The total questionnaires
# answered was n = 939,268 from people aged 18 or older (n = 297,290).
# "There was no significant interaction between suicidal ideation and age for 
# suicide attempts (p=0.116) or the risk of death from suicide (p=0.346).":
# https://doi.org/10.1016/j.jad.2017.03.037

# Another study included 84,148 patients completing 207,265 questionnaires:
# It reports that for each one-step increase on the reported frequency, risk 
# of suicide attempt was 91%.
# This one-step increase represented a 92% increase in the risk of suicide 
# death.
# Of those responding "not at all" (70%), 22% attempted suicide.
# A 13% responded "more than half of the days" or "nearly every day", which
# accounted for 53% of suicide attempts and 54% of suicide deaths.
# https://doi.org/10.1176/appi.ps.201200587

ds <- 
  ds |> 
  mutate(suic_idea = factor(if_else(
    mh_phq_9 >= 1,
    "Yes",
    "No"
  ))) |> 
  relocate(suic_idea, .after = wb_who_co)

# uuid is the id that we have been seeing in the ds as user_id
# IP country of origin is coded with ISO 3166-1 alpha-2 codes

alpha_2_codes <- 
  read.csv("ext/countries_alpha2.csv",
           na.strings = "") |> 
  tibble() |> 
  mutate(Name = case_when(  # Check Ukraine
    Name == "Moldova, Republic of" ~ "Moldova",
    TRUE ~ Name)) |>
  rename(loc_2 = Name,
         iso_country_code = Code)

# Add these for further data merging
ds <- 
  ds |> 
  left_join(alpha_2_codes, 
            by = c("loc_2"))

# Now we can use the ds.
ds <- 
  ds |> 
  relocate(iso_country_code, .after = time_first_answer) |> 
  mutate(iso_country_code = factor(iso_country_code))

rm(alpha_2_codes)


#### DS full ####

ds_full <- ds

variable_labels <- readxl::read_excel("ext/var_labels.xlsx")

for (i in 1:nrow(variable_labels)) {
  uuid <- variable_labels$Question_uuid[i]
  label <-  variable_labels$Label[i]
  ds_full[[uuid]] <- sjlabelled::set_label(ds_full[[uuid]], label)
}

rm(variable_labels,
   i,
   uuid,
   label)

ds_full <- 
  ds_full |> 
  sjlabelled::var_labels(na_count = "Missing values count",
                         na_perc = "Missing values percentage",
                         complete = "Completed survey",
                         valid_survey = "Valid survey",
                         phq_sc = "PHQ-9 score", 
                         phq_co = "PHQ-9 cut-off", 
                         phq_cat = "PHQ-9 category", 
                         gad_sc = "GAD-7 score", 
                         gad_co = "GAD-7 cut-off", 
                         gad_cat = "GAD-7 category", 
                         cage_sc = "CAGE score", 
                         cage_co = "CAGE cut-off", 
                         wb_who_sc = "WHO-5 Well-being index score",
                         wb_who_co = "WHO-5 Well-being index cut-off")

#### Remove cases ####

ds <- 
  ds |> 
  filter(!str_detect(scdm_2_aux, 
                     regex("test", ignore_case = TRUE)) | is.na(scdm_2),
         !str_detect(work_2_a_aux, 
                     regex("test", ignore_case = TRUE)) | is.na(work_2_a),
         !str_detect(work_5_aux, 
                     regex("test", ignore_case = TRUE)) | is.na(work_5),
         !date < "2025-05-20")

ds <- 
  ds |> 
  filter(valid_survey == "valid")

ds <- # includes professional nurses only
  
  ds |> 
  filter((is.na(work_2_b) | work_2_b %in% c("Three years", 
                                            "More than three years"))
    )

ds <- # includes doctors and nurses only
  
  ds |> 
  filter(!is.na(work_2))

# Assign locations as factors
ds <- 
  ds |> 
  mutate(loc_2 = factor(loc_2),
         loc_3 = factor(loc_3))

#### Generating medical specialty groups ####

clinical_ms <- 
  c("Allergology",
    "Accident and emergency medicine",
    "Cardiology",
    "Child psychiatry",
    "Clinical neurophysiology",
    "Communicable diseases",
    "Dental, oral and maxillo-facial surgery (basic medical and dental training)",
    "Dermatology",
    "Dermato-venereology",
    "Endocrinology",
    "Gastroenterology",
    "General (internal) medicine",
    "General Haematology",
    "Geriatrics",
    "Immunology",
    "Medical genetics",
    "Medical oncology",
    "Neurology",
    "Neuropsychiatry",
    "Occupational medicine",
    "Paediatrics",
    "Physiotherapy",
    "Psychiatry",
    "Radiotherapy",
    "Renal diseases",
    "Respiratory medicine",
    "Rheumatology",
    "Stomatology",
    "Tropical medicine",
    "Venerology")

primary_care_ms <- 
  c("Community medicine",
    "General practitioner")

surgical_ms <- 
  c("Anaesthetics",
    "Gastroenterological surgery",
    "General surgery",
    "Maxillo-facial surgery (basic medical training)",
    "Neurological surgery",
    "Obstetrics and Gynaecology",
    "Ophthalmology",
    "Orthopaedics",
    "Otorhinolaryngology",
    "Paediatric surgery",
    "Plastic surgery",
    "Thoracic surgery",
    "Urology",
    "Vascular surgery")


diagnostic_ms <- 
  c("Clinical biology",
    "Diagnostic radiology",
    "Microbiology — bacteriology",
    "Nuclear medicine",
    "Pathological anatomy",
    "Radiology")


other_ms <- 
  c("Biological chemistry",
    "Biological haematology",
    "Other specialty",
    "Pharmacology")

ds <-
  ds |> 
  mutate(specialty_cat = case_when(
    work_2_a %in% clinical_ms ~ "Clinical",
    work_2_a %in% surgical_ms ~ "Surgical",
    work_2_a %in% diagnostic_ms ~ "Diagnostic",
    work_2_a %in% primary_care_ms ~ "Primary Care",
    work_2_a %in% other_ms ~ "Other"
  ) |> as.factor()) 

rm(clinical_ms, diagnostic_ms, other_ms, surgical_ms, 
   primary_care_ms)


#### Add labels ####

variable_labels <- readxl::read_excel("ext/var_labels.xlsx")

for (i in 1:nrow(variable_labels)) {
  uuid <- variable_labels$Question_uuid[i]
  label <-  variable_labels$Label[i]
  ds[[uuid]] <- sjlabelled::set_label(ds[[uuid]], label)
}

rm(variable_labels,
   i,
   uuid,
   label)

# Filter Ukr

ds_ua <-
  
  ds |> 
  filter(loc_2 == "Ukraine")

write_rds(ds_ua, "data/ukr_data.rds")

