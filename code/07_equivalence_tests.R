#!/usr/bin/env Rscript
# coding: utf-8

#===============================================================================
#
#  Gregory Eady, 2021-02-13
#  Tom Paskhalis 2022-09-25
#
#  Estimate main models and calculate equivalence intervals
#
#===============================================================================

library("dplyr")
library("cowplot")
library("readr")

source("./code/helper_functions.R")
source("./code/ggplot_theme.R")

sessionInfo()


# Read In Data ------------------------------------------------------------

# Merged survey responses and exposure
us2016_twitter <- readRDS("./data/Survey_Data.rds")

us2016_recoded <- us2016_twitter %>%
  # remove respondents with no Twitter handles
  dplyr::filter(have_twitter_data == 1) %>%
  dplyr::mutate(
    income = as.numeric(income),
    education = factor(dplyr::case_when(
      education == "High school or below" ~ "High school or below",
      education %in% c("College degree", "Post-graduate degree") ~ "College+"
    ), levels = c("High school or below", "College+")),
    race = factor(dplyr::case_when(
      race == "White" ~ "White",
      race %in% c("Black", "Hispanic", "Other") ~ "Non-White"
    ), levels = c("White", "Non-White")),
    region = factor(region, levels = c("South", "Northeast", "Midwest", "West"))
  )


# Estimate Standardized OLS Models ----------------------------------------

##### ISSUE AND IDEOLOGICAL DISTANCE MODELS

# Ideology
# Immigration
# Muslim ban
# Building Wall
# Obamacare
# ACA
# Free trade
# Tariffs
# Use of Military Force
# Note: progresive income tax and taxing the rich are not included here because
#       the samples for these two questions were split into half-sized samples
#       in the YouGov survey by design

# Get standardized coefficients for models
get_distance_models_std <- function(dv, iv, data) {

  model_out <- list()
  for(the_dv in dv) {
    data[, the_dv] <- as.numeric(scale(data[, the_dv]))
    model_out[[the_dv]] <- lm(build_formula(dv = the_dv, iv = iv), data = data)
  }
  return(model_out)

}


# Issue Position ----------------------------------------------------------

controls <- c(
  "age",
  "gender",
  "education",
  "income",
  "race",
  "region",
  "internet_social_media_w1",
  "pid7"
)

issue_distance_variables <- c(
  "change_ideo_distance_clinton_versus_trump_w1w3",
  "change_immigration_distance_clinton_versus_trump_w1w3",
  "change_banmuslimsBAD_distance_clinton_versus_trump_w1w3",
  "change_buildwallBAD_distance_clinton_versus_trump_w1w3",
  "change_healthcare_distance_clinton_versus_trump_w1w3",
  "change_expandACA_distance_clinton_versus_trump_w1w3",
  "change_freetrade_distance_clinton_versus_trump_w1w3",
  "change_tarrifsBAD_distance_clinton_versus_trump_w1w3",
  "change_milforce_distance_clinton_versus_trump_w1w3"
)

issue_distance_log_exposure_russia_controls_std <- get_distance_models_std(
  dv = issue_distance_variables,
  iv = c("log(total_exposure_russia + 1)", controls),
  data = us2016_recoded)

issue_distance_binary_exposure_russia_controls_std <- get_distance_models_std(
  dv = issue_distance_variables,
  iv = c("total_exposure_russia_binary", controls),
  data = us2016_recoded)


# Ideological Distance (Polarization) -------------------------------------

candidate_distance_variables <- c(
  "ideo_clinton_trump_distance_w1w3",
  "immigration_clinton_trump_distance_w1w3",
  "banmuslimsBAD_clinton_trump_distance_w1w3",
  "buildwallBAD_clinton_trump_distance_w1w3",
  "healthcare_clinton_trump_distance_w1w3",
  "expandACA_clinton_trump_distance_w1w3",
  "freetrade_clinton_trump_distance_w1w3",
  "tarrifsBAD_clinton_trump_distance_w1w3",
  "milforce_clinton_trump_distance_w1w3"
)

candidate_distance_log_exposure_russia_controls_std <- get_distance_models_std(
  dv = candidate_distance_variables,
  iv = c("log(total_exposure_russia + 1)", controls),
  data = us2016_recoded)

candidate_distance_binary_exposure_russia_controls_std <- get_distance_models_std(
  dv = candidate_distance_variables,
  iv = c("total_exposure_russia_binary", controls),
  data = us2016_recoded)


# Calculate TOST ----------------------------------------------------------

# Must match up with names(issue_distance_log_exposure_russia_controls) and
# names(candidate_distance_log_exposure_russia_controls)
dv_labels <- c(
  "Political ideology\n(left-right scale)",
  "Immigration",
  "Ban on Muslims",
  "Building a wall",
  "Support for Obamacare",
  "Expanding the ACA",
  "Support for free trade",
  "Oppose Chinese tariffs",
  "Use of military force"
)

# ISSUE POSITIONS
# Use alpha = 0.1 to calculate 95% Two One-sided Test (TOST) intervals
G1A_log_exposure <- lapply(issue_distance_log_exposure_russia_controls_std,
                          get_coefs,
                          iv = "log(Exposure)", dv = "Label me", alpha = 0.1)
for(i in 1:length(G1A_log_exposure)) G1A_log_exposure[[i]]$dv <- dv_labels[i]

G1A_binary_exposure <- lapply(issue_distance_binary_exposure_russia_controls_std, get_coefs,
                          iv = "Exposure (binary)", dv = "Label me", alpha = 0.1)
for(i in 1:length(G1A_binary_exposure)) G1A_binary_exposure[[i]]$dv <- dv_labels[i]

G1A <- rbind(do.call(rbind, G1A_log_exposure),
            do.call(rbind, G1A_binary_exposure))

G1A$outcome <- "Issues"

# POLARIZATION
# Use alpha = 0.1 to calculate 95% Two One-sided Test (TOST) intervals
G1B_log_exposure <- lapply(candidate_distance_log_exposure_russia_controls_std, get_coefs,
                          iv = "log(Exposure)", dv = "Label me", alpha = 0.1)
for(i in 1:length(G1B_log_exposure)) G1B_log_exposure[[i]]$dv <- dv_labels[i]

G1B_binary_exposure <- lapply(candidate_distance_binary_exposure_russia_controls_std, get_coefs,
                          iv = "Exposure (binary)", dv = "Label me", alpha = 0.1)
for(i in 1:length(G1B_binary_exposure)) G1B_binary_exposure[[i]]$dv <- dv_labels[i]

G1B <- rbind(do.call(rbind, G1B_log_exposure),
             do.call(rbind, G1B_binary_exposure))

G1B$outcome <- "Polarization"

# Reorder labels and prep for graphing
G1 <- rbind(G1A, G1B)

G1$iv <- factor(G1$iv, levels = c("log(Exposure)",
                                  "Exposure (binary)"))

G1$dv <- factor(G1$dv, levels = c("Immigration",
                                  "Building a wall",
                                  "Support for Obamacare",
                                  "Expanding the ACA",
                                  "Ban on Muslims",
                                  "Support for free trade",
                                  "Oppose Chinese tariffs",
                                  "Use of military force",
                                  "Political ideology\n(left-right scale)"))

G1$model_number <- as.numeric(G1$dv)

# Average effect across all issue positions
mean(abs(G1$coef[G1$outcome == "Issues" &
         G1$treatment == "Treatment" &
         G1$iv == "Exposure (binary)"]))

# Average effect across all polarization measures
mean(abs(G1$coef[G1$outcome == "Polarization" &
         G1$treatment == "Treatment" &
         G1$iv == "Exposure (binary)"]))

readr::write_csv(G1, "./results/07_equivalence_tests/G1.csv")
