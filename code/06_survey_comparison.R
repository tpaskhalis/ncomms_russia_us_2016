#!/usr/bin/env Rscript
# coding: utf-8

#===============================================================================
#  
#  Tom Paskhalis, 2020-10-19
#  Jan Zilinsky, 2020-10-07; 2021-01-14.
#
#  Script for comparing YouGov/SMaPP with ACS and Pew samples
#
#===============================================================================

library("haven")
library("readr")
library("dplyr")
library("tidyr")

# # SMaPP/YouGov Panel
# # Original survey
# us2016 <- haven::read_dta("~/Dropbox/SMaPP_US2016B/survey_data/SMaPP_US2016_waves123_idist_ideo_B.dta")
# Merged survey responses and exposure
us2016_twitter <- readRDS("./data/Survey_Data.rds")

# ACS (US Census Bureau)
# https://www.socialexplorer.com/tables/ACS2016/R12649774
# Codebook: ./data/comparison_surveys/ACS_Data_Dictionary_R12649774.txt
acs2016 <- readr::read_csv("./data/comparison_surveys/ACS/ACS_2016_1year_R12649774_SL010.csv")

# Pew
# https://www.pewresearch.org/internet/2019/04/24/sizing-up-twitter-users/
# Full report: ./data/comparison_surveys/Pew/twitter_opinions_4_18_final_clean.pdf
# Data: Figure 3, page 5
summary_pew2018 <- readr::read_csv("./data/comparison_surveys/Pew/summary_pew2018.csv")


# Summarize SMaPP/YouGov Panel --------------------------------------------

us2016_recoded <- us2016_twitter %>%
  # remove respondents with no Twitter handles
  dplyr::filter(have_twitter_data == 1) %>%
  dplyr::mutate(
    income = as.numeric(income),
    non_college = ifelse(college_grad==0,1,0),
    race = factor(dplyr::case_when(
      race == "White" ~ "White",
      race %in% c("Black", "Hispanic", "Other") ~ "Non-White"
    ), levels = c("White", "Non-White")),
    region = factor(region, levels = c("South", "Northeast", "Midwest", "West"))
  )

summary_us2016 <- us2016_recoded %>%
  dplyr::summarize(
    # Sex
    sex_male = sum(gender_female == 0),
    sex_female = sum(gender_female),
    # Age
    `age_18-29` = sum(age >= 18 & age <= 29),
    `age_30-49` = sum(age >= 30 & age <= 49),
    `age_50-64` = sum(age >= 50 & age <= 64),
    `age_65+` = sum(age >= 65),
    # Education
    `education_no college` = sum(non_college),
    `education_college+` = sum(college_grad),
    # Household income
    `income_< 30,000` = sum(income_030, na.rm = TRUE),
    `income_30,000+` = sum(income_3070, income_70120, income_120plus, na.rm = TRUE),
    # Race
    `race_white` = sum(race_white),
    `race_non-white` = sum(race_black, race_hispanic, race_other)
  ) %>%
  tidyr::pivot_longer(cols = tidyselect::everything()) %>%
  tidyr::separate(col = "name", into = c("var", "category"), sep = "_") %>%
  dplyr::group_by(var) %>%
  dplyr::mutate(total = sum(value)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(percentage = (value/total) * 100)


# Summarize ACS -----------------------------------------------------------

summary_acs2016 <- acs2016 %>%
  dplyr::summarize(
    # Sex
    # Older than 18
    sex_male = SE_A02002B_006,
    sex_female = SE_A02002B_018,
    # Age
    # Older than 18
    `age_18-29` = sum(SE_C01001_006, SE_C01001_007, SE_C01001_008, SE_C01001_009, SE_C01001_010),
    `age_30-49` = sum(SE_C01001_011, SE_C01001_012, SE_C01001_013, SE_C01001_014),
    `age_50-64` = sum(SE_C01001_015, SE_C01001_016, SE_C01001_017, SE_C01001_018),
    `age_65+` = sum(SE_C01001_019, SE_C01001_020, SE_C01001_021, SE_C01001_022, SE_C01001_023, SE_C01001_024),
    # Education
    # Older than 25
    `education_no college` = sum(SE_A12001_002, SE_A12001_003, SE_A12001_004),
    `education_college+` = sum(SE_A12001_005, SE_A12001_006, SE_A12001_007, SE_A12001_008),
    # Household income
    `income_< 30,000` = SE_A14001A_006,
    `income_30,000+` = SE_A14001B_006,
    # Race
    `race_white` = SE_A03001_002,
    `race_non-white` = sum(SE_A03001_003, SE_A03001_004, SE_A03001_005, SE_A03001_006, SE_A03001_007, SE_A03001_008)
  ) %>%
  tidyr::pivot_longer(cols = tidyselect::everything()) %>%
  tidyr::separate(col = "name", into = c("var", "category"), sep = "_") %>%
  dplyr::group_by(var) %>%
  dplyr::mutate(total = sum(value)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(percentage = (value/total) * 100)


# Create Survey Comparison ------------------------------------------------

# SMaPP/ACS/Pew
survey_comparison <- summary_us2016 %>%
  dplyr::select(var, category, percentage) %>%
  dplyr::left_join(summary_pew2018, by = c("var", "category")) %>%
  dplyr::left_join(summary_acs2016 %>%
                     dplyr::select(var, category, percentage), by = c("var", "category")) %>%
  dplyr::rename(
    smapp = percentage.x,
    pew = percentage.y,
    acs = percentage
  )

readr::write_csv(survey_comparison, "./results/06_survey_comparison/survey_comparison.csv")

# With/without Twitter
summary_ideology <- us2016_twitter %>%
  dplyr::mutate(
    have_twitter_data = factor(have_twitter_data, labels = c("no_twitter_data", "twitter_data"))
  ) %>%
  dplyr::count(have_twitter_data, pid7) %>%
  dplyr::mutate(pid7 = dplyr::case_when(
    as.character(round(pid7, 3)) == "0" ~ "Strong Democrat",
    as.character(round(pid7, 3)) == "0.167" ~ "Moderate Democrat",
    as.character(round(pid7, 3)) == "0.333" ~ "Weak Democrat",
    as.character(round(pid7, 3)) == "0.5" ~ "Independent",
    as.character(round(pid7, 3)) == "0.667" ~ "Weak Republican",
    as.character(round(pid7, 3)) == "0.833" ~ "Moderate Republican",
    as.character(round(pid7, 3)) == "1" ~ "Strong Republican"
  )) %>%
  tidyr::pivot_wider(names_from = have_twitter_data, values_from = n) %>%
  dplyr::mutate(
    twitter_data_perc = twitter_data/sum(twitter_data) * 100,
    no_twitter_data_perc = no_twitter_data/sum(no_twitter_data) * 100
  ) %>%
  dplyr::bind_rows(
    tibble::tibble(
      pid7 = "N",
      no_twitter_data = NA,
      twitter_data = NA,
      twitter_data_perc = sum(.$twitter_data),
      no_twitter_data_perc = sum(.$no_twitter_data)
    )
  ) %>%
  # Remove NAs
  dplyr::filter(!is.na(pid7)) %>%
  dplyr::select(-no_twitter_data, -twitter_data)

readr::write_csv(summary_ideology, "./results/06_survey_comparison/summary_ideology.csv")
