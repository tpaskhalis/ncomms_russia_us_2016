#!/usr/bin/env Rscript
# coding: utf-8

#===============================================================================
#
#  Gregory Eady, 2022-09-16
#  Tom Paskhalis, 2022-09-16
#
#  Estimate baseline models of exposure/effects of exposure
#
#===============================================================================

library("dplyr")
library("estimatr")
library("lfe")
library("lmtest")
library("MASS")
library("mvtnorm")
library("nnet")
library("sandwich")
library("tidyr")

source("./code/helper_functions.R")

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


# Models of Total Exposure (Binary) ---------------------------------------

# Specify control variables
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

controls_black <- c(
  "age",
  "gender",
  "education",
  "income",
  "race_other_hispanic",
  "race_black",
  "region",
  "internet_social_media_w1",
  "pid7"
)

controls_total_tweets <- c(
  "age",
  "gender",
  "education",
  "income",
  "race_other_hispanic",
  "race_black",
  "region",
  "internet_social_media_w1",
  "total_tweets_log",
  "pid7"
)

total_exposure_all_glm <- glm(
  build_formula(dv = "total_exposure_all_binary", iv = controls),
  family = binomial(),
  data = us2016_recoded
)

total_exposure_all_glm_black <- glm(
  build_formula(dv = "total_exposure_all_binary", iv = controls_black),
  family = binomial(),
  data = us2016_recoded
)

total_exposure_russia_glm <- glm(
  build_formula(dv = "total_exposure_russia_binary", iv = controls),
  family = binomial(),
  data = us2016_recoded
)

total_exposure_russia_glm_black <- glm(
  build_formula(dv = "total_exposure_russia_binary", iv = controls_black),
  family = binomial(),
  data = us2016_recoded
)

saveRDS(
  total_exposure_all_glm,
  "./results/05_regression_analysis/total_exposure_all_glm.rds"
)
saveRDS(
  total_exposure_all_glm_black,
  "./results/05_regression_analysis/total_exposure_all_glm_black.rds"
)
saveRDS(
  total_exposure_russia_glm,
  "./results/05_regression_analysis/total_exposure_russia_glm.rds"
)
saveRDS(
  total_exposure_russia_glm_black,
  "./results/05_regression_analysis/total_exposure_russia_glm_black.rds"
)


# Models of Total Exposure (Continuous) -----------------------------------

# RUSSIA
total_exposure_russia_log <- glm(total_exposure_russia_log ~ gender +
                                                 internet_social_media_w1 +
                                                 age +
                                                 income +
                                                 education +
                                                 pid7 +
                                                 race +
                                                 region,
                            data = us2016_recoded)

total_exposure_russia_log_black <- glm(total_exposure_russia_log ~ gender +
                                                 internet_social_media_w1 +
                                                 age +
                                                 income +
                                                 education +
                                                 pid7 +
                                                 race_other_hispanic +
                                                 race_black +
                                                 region,
                            data = us2016_recoded)

total_exposure_russia_log_total_tweets <- glm(total_exposure_russia_log ~ gender +
                                                 internet_social_media_w1 +
                                                 total_tweets_log +
                                                 age +
                                                 income +
                                                 education +
                                                 pid7 +
                                                 race_other_hispanic +
                                                 race_black +
                                                 region,
                            data = us2016_recoded)

total_exposure_russia_log_square <- glm(total_exposure_russia_log ~ gender +
                                                 internet_social_media_w1 +
                                                 age +
                                                 income +
                                                 education +
                                                 pid7 +
                                                 I(pid7^2) +
                                                 race +
                                                 region,
                            data = us2016_recoded)

saveRDS(
  total_exposure_russia_log,
  "./results/05_regression_analysis/total_exposure_russia_log.rds"
)
saveRDS(
  total_exposure_russia_log_total_tweets,
  "./results/05_regression_analysis/total_exposure_russia_log_total_tweets.rds"
)
saveRDS(
  total_exposure_russia_log_black,
  "./results/05_regression_analysis/total_exposure_russia_log_black.rds"
)
saveRDS(
  total_exposure_russia_log_square,
  "./results/05_regression_analysis/total_exposure_russia_log_square.rds"
)

# All campaigns
total_exposure_all_log <- glm(total_exposure_all_log ~ gender +
                            internet_social_media_w1 +
                            age +
                            income +
                            education +
                            pid7 +
                            race +
                            region,
                          data = us2016_recoded)

total_exposure_all_log_black <- glm(total_exposure_all_log ~ gender +
                            internet_social_media_w1 +
                            age +
                            income +
                            education +
                            pid7 +
                            race_other_hispanic +
                            race_black +
                            region,
                          data = us2016_recoded)

total_exposure_all_log_total_tweets <- glm(total_exposure_all_log ~ gender +
                            internet_social_media_w1 +
                            total_tweets_log +
                            age +
                            income +
                            education +
                            pid7 +
                            race_other_hispanic +
                            race_black +
                            region,
                          data = us2016_recoded)

total_exposure_all_log_square <- glm(total_exposure_all_log ~ gender +
                                   internet_social_media_w1 +
                                   age +
                                   income +
                                   education +
                                   pid7 +
                                   I(pid7^2) +
                                   race +
                                   region,
                                 data = us2016_recoded)

saveRDS(
  total_exposure_all_log,
  "./results/05_regression_analysis/total_exposure_all_log.rds"
)
saveRDS(
  total_exposure_all_log_total_tweets,
  "./results/05_regression_analysis/total_exposure_all_log_total_tweets.rds"
)
saveRDS(
  total_exposure_all_log_black,
  "./results/05_regression_analysis/total_exposure_all_log_black.rds"
)
saveRDS(
  total_exposure_all_log_square,
  "./results/05_regression_analysis/total_exposure_all_log_square.rds"
)


# Quasi-poisson
total_exposure_russia_poisson <- glm(total_exposure_russia ~ gender +
                                                 internet_social_media_w1 +
                                                 age +
                                                 income +
                                                 education +
                                                 pid7 +
                                                 race +
                                                 region,
                            family = "quasipoisson",
                            data = us2016_recoded)

total_exposure_russia_poisson_black <- glm(total_exposure_russia ~ gender +
                                                 internet_social_media_w1 +
                                                 age +
                                                 income +
                                                 education +
                                                 pid7 +
                                                 race_other_hispanic +
                                                 race_black +
                                                 region,
                            family = "quasipoisson",
                            data = us2016_recoded)

total_exposure_russia_poisson_total_tweets <- glm(total_exposure_russia ~ gender +
                                                 internet_social_media_w1 +
                                                 total_tweets_log +
                                                 age +
                                                 income +
                                                 education +
                                                 pid7 +
                                                 race_other_hispanic +
                                                 race_black +
                                                 region,
                            family = "quasipoisson",
                            data = us2016_recoded)

total_exposure_russia_poisson_square <- glm(total_exposure_russia ~ gender +
                                                 internet_social_media_w1 +
                                                 age +
                                                 income +
                                                 education +
                                                 pid7 +
                                                 I(pid7^2) +
                                                 race +
                                                 region,
                            family = "quasipoisson",
                            data = us2016_recoded)

saveRDS(
  total_exposure_russia_poisson,
  "./results/05_regression_analysis/total_exposure_russia_poisson.rds"
)
saveRDS(
  total_exposure_russia_poisson_black,
  "./results/05_regression_analysis/total_exposure_russia_poisson_black.rds"
)
saveRDS(
  total_exposure_russia_poisson_total_tweets,
  "./results/05_regression_analysis/total_exposure_russia_poisson_total_tweets.rds"
)
saveRDS(
  total_exposure_russia_poisson_square,
  "./results/05_regression_analysis/total_exposure_russia_poisson_square.rds"
)

# All campaigns
total_exposure_all_poisson <- glm(total_exposure_all ~ gender +
                            internet_social_media_w1 +
                            age +
                            income +
                            education +
                            pid7 +
                            race +
                            region,
                          family = "quasipoisson",
                          data = us2016_recoded)

total_exposure_all_poisson_black <- glm(total_exposure_all ~ gender +
                            internet_social_media_w1 +
                            age +
                            income +
                            education +
                            pid7 +
                            race_other_hispanic +
                            race_black +
                            region,
                          family = "quasipoisson",
                          data = us2016_recoded)

total_exposure_all_poisson_total_tweets <- glm(total_exposure_all ~ gender +
                            internet_social_media_w1 +
                            total_tweets_log +
                            age +
                            income +
                            education +
                            pid7 +
                            race_other_hispanic +
                            race_black +
                            region,
                          family = "quasipoisson",
                          data = us2016_recoded)

total_exposure_all_poisson_square <- glm(total_exposure_all ~ gender +
                                   internet_social_media_w1 +
                                   age +
                                   income +
                                   education +
                                   pid7 +
                                   I(pid7^2) +
                                   race +
                                   region,
                                 family = "quasipoisson",
                                 data = us2016_recoded)

saveRDS(
  total_exposure_all_poisson,
  "./results/05_regression_analysis/total_exposure_all_poisson.rds"
)
saveRDS(
  total_exposure_all_poisson_black,
  "./results/05_regression_analysis/total_exposure_all_poisson_black.rds"
)
saveRDS(
  total_exposure_all_poisson_total_tweets,
  "./results/05_regression_analysis/total_exposure_all_poisson_total_tweets.rds"
)
saveRDS(
  total_exposure_all_poisson_square,
  "./results/05_regression_analysis/total_exposure_all_poisson_square.rds"
)


# Simulate Predicted Exposure ---------------------------------------------

# Results for Figure 3C
#   Predicted exposure conditional on PID, hold all other variables
#   at their observed values

# Read-in for possible separate re-run
total_exposure_russia_log <- readRDS(
  "./results/05_regression_analysis/total_exposure_russia_log.rds"
)
total_exposure_russia_log_square <- readRDS(
  "./results/05_regression_analysis/total_exposure_russia_log_square.rds"
)
total_exposure_russia_poisson <- readRDS(
  "./results/05_regression_analysis/total_exposure_russia_poisson.rds"
)
total_exposure_russia_poisson_square <- readRDS(
  "./results/05_regression_analysis/total_exposure_russia_poisson_square.rds"
)

# Do we need PID squared? p = 0.09. PID as linear is highly significant.
lmtest::lrtest(total_exposure_russia_log, total_exposure_russia_log_square)$`Pr(>Chisq)`[2]

# Simulate coefficients from the fitted model and
# plot the relationship between party ID and exposure to Russian IRA tweets
n_sims <- 5000

# ~7 minutes to run
set.seed(1)
G3C1 <- get_sims(total_exposure_russia_log, n_sims)
G3C2 <- get_sims(total_exposure_russia_log_square, n_sims)

G3C3 <- get_sims(total_exposure_russia_poisson, n_sims)
G3C4 <- get_sims(total_exposure_russia_poisson_square, n_sims)

G3C1$model <- "Linear"
G3C2$model <- "Quadratic"
G3C3$model <- "Poisson Linear"
G3C4$model <- "Poisson Quadratic"

G3C <- rbind(G3C1, G3C2, G3C3, G3C4)
G3C$coef[G3C$model %in% c("Linear", "Quadratic")] <- 
  exp(G3C$coef[G3C$model %in% c("Linear", "Quadratic")]) - 1
G3C$lower95[G3C$model %in% c("Linear", "Quadratic")] <- 
  exp(G3C$lower95[G3C$model %in% c("Linear", "Quadratic")]) - 1
G3C$upper95[G3C$model %in% c("Linear", "Quadratic")] <- 
  exp(G3C$upper95[G3C$model %in% c("Linear", "Quadratic")]) - 1

readr::write_csv(G3C, "./results/05_regression_analysis/G3C.csv")


# Models of Vote Choice ---------------------------------------------------

# Simulate difference in outcome under observed vs under no treatment (exposure)
sim_lm <- function(the_model, vcov_matrix, the_data, treatment, n_sims = 10000) {
  
  outcome_variable <- all.vars(formula(the_model))[1]
  the_data <- subset(the_data, !is.na(the_data[, outcome_variable]))
  the_data_counterfact <- the_data
  the_data_counterfact[, treatment] <- 0
  
  out <- NA
  
  for(i in 1:n_sims) {
    
    model_copy <- the_model
    par_sim <- mvtnorm::rmvnorm(1, c(coef(the_model)), vcov_matrix)
    model_copy$coefficients <- par_sim
    prediction_obs <- predict(model_copy, newdata = the_data)
    prediction_counterfact <- predict(model_copy, newdata = the_data_counterfact)
    out[i] <- mean(prediction_obs - prediction_counterfact, na.rm = TRUE)
    
  }
  
  return(out)
}

Sims <- list()

# EXPOSURE TO TWEETS FROM _ANY_ FOREIGN INTERFERENCE CAMPAIGN
# Change in rank of Trump versus Clinton
vote_rank_total_all_lm <- lm(
  build_formula(dv = "change_rank_trump_over_clinton_w1w3",
                iv = "total_exposure_all_log"),
  data = us2016_recoded
)
vote_rank_total_all_lm_vcov <- sandwich::vcovHC(
  vote_rank_total_all_lm, 
  type = "HC0"
)
vote_rank_total_all_lm_robust <- lmtest::coeftest(
  vote_rank_total_all_lm, 
  vcov = vote_rank_total_all_lm_vcov
)

vote_rank_total_all_lm_controls <- lm(
  build_formula(dv = "change_rank_trump_over_clinton_w1w3",
                iv = c("total_exposure_all_log",
                       controls)),
  data = us2016_recoded
)
vote_rank_total_all_lm_controls_vcov <- sandwich::vcovHC(
  vote_rank_total_all_lm_controls, 
  type = "HC0"
)
vote_rank_total_all_lm_controls_robust <- lmtest::coeftest(
  vote_rank_total_all_lm_controls, 
  vcov = vote_rank_total_all_lm_controls_vcov
)

vote_rank_total_all_lm_controls_black <- lm(
  build_formula(dv = "change_rank_trump_over_clinton_w1w3",
                iv = c("total_exposure_all_log",
                       controls_black)),
  data = us2016_recoded
)
vote_rank_total_all_lm_controls_black_vcov <- sandwich::vcovHC(
  vote_rank_total_all_lm_controls_black, 
  type = "HC0"
)
vote_rank_total_all_lm_controls_black_robust <- lmtest::coeftest(
  vote_rank_total_all_lm_controls_black, 
  vcov = vote_rank_total_all_lm_controls_black_vcov
)

vote_rank_total_all_lm_controls_total_tweets <- lm(
  build_formula(dv = "change_rank_trump_over_clinton_w1w3",
                iv = c("total_exposure_all_log",
                       controls_total_tweets)),
  data = us2016_recoded
)
vote_rank_total_all_lm_controls_total_tweets_vcov <- sandwich::vcovHC(
  vote_rank_total_all_lm_controls_total_tweets, 
  type = "HC0"
)
vote_rank_total_all_lm_controls_total_tweets_robust <- lmtest::coeftest(
  vote_rank_total_all_lm_controls_total_tweets, 
  vcov = vote_rank_total_all_lm_controls_total_tweets_vcov
)

vote_rank_total_all_lm_rhs <- lm(
  build_formula(dv = "rank_trump_over_clinton_w1",
                iv = c("total_exposure_all_log", "rank_trump_over_clinton_w3")),
  data = us2016_recoded
)
vote_rank_total_all_lm_rhs_vcov <- sandwich::vcovHC(
  vote_rank_total_all_lm_rhs, 
  type = "HC0"
)
vote_rank_total_all_lm_rhs_robust <- lmtest::coeftest(
  vote_rank_total_all_lm_rhs, 
  vcov = vote_rank_total_all_lm_rhs_vcov
)

vote_rank_total_all_lm_controls_rhs <- lm(
  build_formula(dv = "rank_trump_over_clinton_w1",
                iv = c("total_exposure_all_log", "rank_trump_over_clinton_w3",
                       controls)),
  data = us2016_recoded
)
vote_rank_total_all_lm_controls_rhs_vcov <- sandwich::vcovHC(
  vote_rank_total_all_lm_controls_rhs, 
  type = "HC0"
)
vote_rank_total_all_lm_controls_rhs_robust <- lmtest::coeftest(
  vote_rank_total_all_lm_controls_rhs, 
  vcov = vote_rank_total_all_lm_controls_rhs_vcov
)

saveRDS(
  vote_rank_total_all_lm_robust,
  "./results/05_regression_analysis/vote_rank_total_all_lm.rds"
)
saveRDS(
  vote_rank_total_all_lm_controls_robust,
  "./results/05_regression_analysis/vote_rank_total_all_lm_controls.rds"
)
saveRDS(
  vote_rank_total_all_lm_controls_black_robust,
  "./results/05_regression_analysis/vote_rank_total_all_lm_controls_black.rds"
)
saveRDS(
  vote_rank_total_all_lm_controls_robust,
  "./results/05_regression_analysis/vote_rank_total_all_lm_controls.rds"
)
saveRDS(
  vote_rank_total_all_lm_rhs_robust,
  "./results/05_regression_analysis/vote_rank_total_all_lm_rhs.rds"
)
saveRDS(
  vote_rank_total_all_lm_controls_rhs_robust,
  "./results/05_regression_analysis/vote_rank_total_all_lm_controls_rhs.rds"
)

Sims[["vote_rank_total_all_lm"]] <- sim_lm(
  vote_rank_total_all_lm,
  vote_rank_total_all_lm_vcov,
  us2016_recoded,
  "total_exposure_all_log"
)
Sims[["vote_rank_total_all_lm_controls"]] <- sim_lm(
  vote_rank_total_all_lm_controls,
  vote_rank_total_all_lm_controls_vcov,
  us2016_recoded,
  "total_exposure_all_log"
)


# Change in voting for Trump versus Clinton relative to ranking
vote_choice_total_all_lm <- lm(
  build_formula(dv = "voted_trump_versus_clinton",
                iv = c("total_exposure_all_log")),
  data = us2016_recoded
)
vote_choice_total_all_lm_vcov <- sandwich::vcovHC(
  vote_choice_total_all_lm, 
  type = "HC0"
)
vote_choice_total_all_lm_robust <- lmtest::coeftest(
  vote_choice_total_all_lm, 
  vcov = vote_choice_total_all_lm_vcov
)

vote_choice_total_all_lm_controls <- lm(
  build_formula(dv = "voted_trump_versus_clinton",
                iv = c("total_exposure_all_log",
                       controls)),
  data = us2016_recoded
)
vote_choice_total_all_lm_controls_vcov <- sandwich::vcovHC(
  vote_choice_total_all_lm_controls, 
  type = "HC0"
)
vote_choice_total_all_lm_controls_robust <- lmtest::coeftest(
  vote_choice_total_all_lm_controls, 
  vcov = vote_choice_total_all_lm_controls_vcov
)

vote_choice_total_all_lm_controls_black <- lm(
  build_formula(dv = "voted_trump_versus_clinton",
                iv = c("total_exposure_all_log",
                       controls_black)),
  data = us2016_recoded
)
vote_choice_total_all_lm_controls_black_vcov <- sandwich::vcovHC(
  vote_choice_total_all_lm_controls_black, 
  type = "HC0"
)
vote_choice_total_all_lm_controls_black_robust <- lmtest::coeftest(
  vote_choice_total_all_lm_controls_black, 
  vcov = vote_choice_total_all_lm_controls_black_vcov
)

vote_choice_total_all_lm_controls_total_tweets <- lm(
  build_formula(dv = "voted_trump_versus_clinton",
                iv = c("total_exposure_all_log",
                       controls_total_tweets)),
  data = us2016_recoded
)
vote_choice_total_all_lm_controls_total_tweets_vcov <- sandwich::vcovHC(
  vote_choice_total_all_lm_controls_total_tweets, 
  type = "HC0"
)
vote_choice_total_all_lm_controls_total_tweets_robust <- lmtest::coeftest(
  vote_choice_total_all_lm_controls_total_tweets, 
  vcov = vote_choice_total_all_lm_controls_total_tweets_vcov
)

vote_choice_total_all_lm_rhs <- lm(
  build_formula(dv = "voted_trump_not_clinton",
                iv = c("total_exposure_all_log", "rank_trump_over_clinton_w1")),
  data = us2016_recoded
)
vote_choice_total_all_lm_rhs_vcov <- sandwich::vcovHC(
  vote_choice_total_all_lm_rhs, 
  type = "HC0"
)
vote_choice_total_all_lm_rhs_robust <- lmtest::coeftest(
  vote_choice_total_all_lm_rhs, 
  vcov = vote_choice_total_all_lm_rhs_vcov
)

vote_choice_total_all_lm_controls_rhs <- lm(
  build_formula(dv = "voted_trump_not_clinton",
                iv = c("total_exposure_all_log", "rank_trump_over_clinton_w1",
                       controls)),
  data = us2016_recoded
)
vote_choice_total_all_lm_controls_rhs_vcov <- sandwich::vcovHC(
  vote_choice_total_all_lm_controls_rhs, 
  type = "HC0"
)
vote_choice_total_all_lm_controls_rhs_robust <- lmtest::coeftest(
  vote_choice_total_all_lm_controls_rhs, 
  vcov = vote_choice_total_all_lm_controls_rhs_vcov
)

saveRDS(
  vote_choice_total_all_lm_robust,
  "./results/05_regression_analysis/vote_choice_total_all_lm.rds"
)
saveRDS(
  vote_choice_total_all_lm_controls_robust,
  "./results/05_regression_analysis/vote_choice_total_all_lm_controls.rds"
)
saveRDS(
  vote_choice_total_all_lm_controls_black_robust,
  "./results/05_regression_analysis/vote_choice_total_all_lm_controls_black.rds"
)
saveRDS(
  vote_choice_total_all_lm_controls_total_tweets_robust,
  "./results/05_regression_analysis/vote_choice_total_all_lm_controls_total_tweets.rds"
)
saveRDS(
  vote_choice_total_all_lm_rhs_robust,
  "./results/05_regression_analysis/vote_choice_total_all_lm_rhs.rds"
)
saveRDS(
  vote_choice_total_all_lm_controls_rhs_robust,
  "./results/05_regression_analysis/vote_choice_total_all_lm_controls_rhs.rds"
)

Sims[["vote_choice_total_all_lm"]] <- sim_lm(
  vote_choice_total_all_lm,
  vote_choice_total_all_lm_vcov,
  us2016_recoded,
  "total_exposure_all_log"
)
Sims[["vote_choice_total_all_lm_controls"]] <- sim_lm(
  vote_choice_total_all_lm_controls,
  vote_choice_total_all_lm_controls_vcov,
  us2016_recoded,
  "total_exposure_all_log"
)


# Change in voting away from Trump to {Clinton, 3rd party, or not voting},
# or in votting away from Clinton to {Trump, 3rd party, or not voting}
vote_choice_benefit_total_all_lm <- lm(
  build_formula(dv = "voted_trump_versus_clinton_benefit",
                iv = c("total_exposure_all_log")),
  data = us2016_recoded
)
vote_choice_benefit_total_all_lm_vcov <- sandwich::vcovHC(
  vote_choice_benefit_total_all_lm, type = "HC0"
)
vote_choice_benefit_total_all_lm_robust <- lmtest::coeftest(
  vote_choice_benefit_total_all_lm,
  vcov = vote_choice_benefit_total_all_lm_vcov
)

vote_choice_benefit_total_all_lm_controls <- lm(
  build_formula(dv = "voted_trump_versus_clinton_benefit",
                iv = c("total_exposure_all_log", controls)),
  data = us2016_recoded
)
vote_choice_benefit_total_all_lm_controls_vcov <- sandwich::vcovHC(
  vote_choice_benefit_total_all_lm_controls,
  type = "HC0"
)
vote_choice_benefit_total_all_lm_controls_robust <- lmtest::coeftest(
  vote_choice_benefit_total_all_lm_controls,
  vcov = vote_choice_benefit_total_all_lm_controls_vcov
)

vote_choice_benefit_total_all_lm_controls_black <- lm(
  build_formula(dv = "voted_trump_versus_clinton_benefit",
                iv = c("total_exposure_all_log", controls_black)),
  data = us2016_recoded
)
vote_choice_benefit_total_all_lm_controls_black_vcov <- sandwich::vcovHC(
  vote_choice_benefit_total_all_lm_controls_black,
  type = "HC0"
)
vote_choice_benefit_total_all_lm_controls_black_robust <- lmtest::coeftest(
  vote_choice_benefit_total_all_lm_controls_black,
  vcov = vote_choice_benefit_total_all_lm_controls_black_vcov
)

vote_choice_benefit_total_all_lm_controls_total_tweets <- lm(
  build_formula(dv = "voted_trump_versus_clinton_benefit",
                iv = c("total_exposure_all_log", controls_total_tweets)),
  data = us2016_recoded
)
vote_choice_benefit_total_all_lm_controls_total_tweets_vcov <- sandwich::vcovHC(
  vote_choice_benefit_total_all_lm_controls_total_tweets, 
  type = "HC0"
)
vote_choice_benefit_total_all_lm_controls_total_tweets_robust <- lmtest::coeftest(
  vote_choice_benefit_total_all_lm_controls_total_tweets,
  vcov = vote_choice_benefit_total_all_lm_controls_total_tweets_vcov
)

vote_choice_benefit_total_all_lm_rhs <- lm(
  build_formula(dv = "voted_trump_not_clinton_benefit",
                iv = c("total_exposure_all_log", "rank_trump_over_clinton_w1")),
  data = us2016_recoded
)
vote_choice_benefit_total_all_lm_rhs_vcov <- sandwich::vcovHC(
  vote_choice_benefit_total_all_lm_rhs,
  type = "HC0"
)
vote_choice_benefit_total_all_lm_rhs_robust <- lmtest::coeftest(
  vote_choice_benefit_total_all_lm_rhs,
  vcov = vote_choice_benefit_total_all_lm_rhs_vcov
)

vote_choice_benefit_total_all_lm_controls_rhs <- lm(
  build_formula(dv = "voted_trump_not_clinton_benefit",
                iv = c("total_exposure_all_log", controls, "rank_trump_over_clinton_w1")),
  data = us2016_recoded
)
vote_choice_benefit_total_all_lm_controls_rhs_vcov <- sandwich::vcovHC(
  vote_choice_benefit_total_all_lm_controls_rhs, 
  type = "HC0"
)
vote_choice_benefit_total_all_lm_controls_rhs_robust <- lmtest::coeftest(
  vote_choice_benefit_total_all_lm_controls_rhs, 
  vcov = vote_choice_benefit_total_all_lm_controls_rhs_vcov
)

saveRDS(
  vote_choice_benefit_total_all_lm_robust, 
  "./results/05_regression_analysis/vote_choice_benefit_total_all_lm.rds"
)
saveRDS(
  vote_choice_benefit_total_all_lm_controls_robust, 
  "./results/05_regression_analysis/vote_choice_benefit_total_all_lm_controls.rds"
)
saveRDS(
  vote_choice_benefit_total_all_lm_controls_black_robust, 
  "./results/05_regression_analysis/vote_choice_benefit_total_all_lm_controls_black.rds"
)
saveRDS(
  vote_choice_benefit_total_all_lm_controls_total_tweets_robust, 
  "./results/05_regression_analysis/vote_choice_benefit_total_all_lm_controls_total_tweets.rds"
)
saveRDS(
  vote_choice_benefit_total_all_lm_rhs_robust, 
  "./results/05_regression_analysis/vote_choice_benefit_total_all_lm_rhs.rds"
)
saveRDS(
  vote_choice_benefit_total_all_lm_controls_rhs_robust, 
  "./results/05_regression_analysis/vote_choice_benefit_total_all_lm_controls_rhs.rds"
)

Sims[["vote_choice_benefit_total_all_lm"]] <- sim_lm(
  vote_choice_benefit_total_all_lm,
  vote_choice_benefit_total_all_lm_vcov,
  us2016_recoded,
  "total_exposure_all_log"
)
Sims[["vote_choice_benefit_total_all_lm_controls"]] <- sim_lm(
  vote_choice_benefit_total_all_lm_controls,
  vote_choice_benefit_total_all_lm_controls_vcov,
  us2016_recoded,
  "total_exposure_all_log"
)


# EXPOSURE TO TWEETS FROM THE IRA

# Change in rank of Trump versus Clinton
vote_rank_total_russia_count_lm <- lm(
  build_formula(dv = "change_rank_trump_over_clinton_w1w3",
                iv = c("total_exposure_russia_log")),
  data = us2016_recoded
)
vote_rank_total_russia_count_lm_vcov <- sandwich::vcovHC(
  vote_rank_total_russia_count_lm, 
  type = "HC0"
)
vote_rank_total_russia_count_lm_robust <- lmtest::coeftest(
  vote_rank_total_russia_count_lm, 
  vcov = vote_rank_total_russia_count_lm_vcov
)

vote_rank_total_russia_count_lm_controls <- lm(
  build_formula(dv = "change_rank_trump_over_clinton_w1w3",
                iv = c("total_exposure_russia_log",
                       controls)),
  data = us2016_recoded
)
vote_rank_total_russia_count_lm_controls_vcov <- sandwich::vcovHC(
  vote_rank_total_russia_count_lm_controls, 
  type = "HC0"
)
vote_rank_total_russia_count_lm_controls_robust <- lmtest::coeftest(
  vote_rank_total_russia_count_lm_controls, 
  vcov = vote_rank_total_russia_count_lm_controls_vcov
)

vote_rank_total_russia_count_lm_controls_black <- lm(
  build_formula(dv = "change_rank_trump_over_clinton_w1w3",
                iv = c("total_exposure_russia_log",
                       controls_black)),
  data = us2016_recoded
)
vote_rank_total_russia_count_lm_controls_black_vcov <- sandwich::vcovHC(
  vote_rank_total_russia_count_lm_controls_black, 
  type = "HC0"
)
vote_rank_total_russia_count_lm_controls_black_robust <- lmtest::coeftest(
  vote_rank_total_russia_count_lm_controls_black, 
  vcov = vote_rank_total_russia_count_lm_controls_black_vcov
)

vote_rank_total_russia_count_lm_controls_total_tweets <- lm(
  build_formula(dv = "change_rank_trump_over_clinton_w1w3",
                iv = c("total_exposure_russia_log",
                       controls_total_tweets)),
  data = us2016_recoded
)
vote_rank_total_russia_count_lm_controls_total_tweets_vcov <- sandwich::vcovHC(
  vote_rank_total_russia_count_lm_controls_total_tweets, 
  type = "HC0"
)
vote_rank_total_russia_count_lm_controls_total_tweets_robust <- lmtest::coeftest(
  vote_rank_total_russia_count_lm_controls_total_tweets, 
  vcov = vote_rank_total_russia_count_lm_controls_total_tweets_vcov
)

# Change in rank of Trump versus Clinton (log0 + binary)
vote_rank_total_russia_count_log0_lm <- lm(
  build_formula(dv = "change_rank_trump_over_clinton_w1w3",
                iv = c("total_exposure_russia_log0", "total_exposure_russia_binary")),
  data = us2016_recoded
)
vote_rank_total_russia_count_log0_lm_vcov <- sandwich::vcovHC(
  vote_rank_total_russia_count_log0_lm, 
  type = "HC0"
)
vote_rank_total_russia_count_log0_lm_robust <- lmtest::coeftest(
  vote_rank_total_russia_count_log0_lm, 
  vcov = vote_rank_total_russia_count_log0_lm_vcov
)

vote_rank_total_russia_count_log0_lm_controls <- lm(
  build_formula(dv = "change_rank_trump_over_clinton_w1w3",
                iv = c("total_exposure_russia_log0", "total_exposure_russia_binary",
                       controls)),
  data = us2016_recoded
)
vote_rank_total_russia_count_log0_lm_controls_vcov <- sandwich::vcovHC(
  vote_rank_total_russia_count_log0_lm_controls, 
  type = "HC0"
)
vote_rank_total_russia_count_log0_lm_controls_robust <- lmtest::coeftest(
  vote_rank_total_russia_count_log0_lm_controls, 
  vcov = vote_rank_total_russia_count_log0_lm_controls_vcov
)

# Change in rank of Trump versus Clinton
vote_rank_total_russia_count_lm_rhs <- lm(
  build_formula(dv = "rank_trump_over_clinton_w3",
                iv = c("total_exposure_russia_log", "rank_trump_over_clinton_w1")),
  data = us2016_recoded
)
vote_rank_total_russia_count_lm_rhs_vcov <- sandwich::vcovHC(
  vote_rank_total_russia_count_lm_rhs, 
  type = "HC0"
)
vote_rank_total_russia_count_lm_rhs_robust <- lmtest::coeftest(
  vote_rank_total_russia_count_lm_rhs, 
  vcov = vote_rank_total_russia_count_lm_rhs_vcov
)

vote_rank_total_russia_count_lm_controls_rhs <- lm(
  build_formula(dv = "rank_trump_over_clinton_w3",
                iv = c("total_exposure_russia_log",
                       controls, "rank_trump_over_clinton_w1")),
  data = us2016_recoded
)
vote_rank_total_russia_count_lm_controls_rhs_vcov <- sandwich::vcovHC(
  vote_rank_total_russia_count_lm_controls_rhs, 
  type = "HC0"
)
vote_rank_total_russia_count_lm_controls_rhs_robust <- lmtest::coeftest(
  vote_rank_total_russia_count_lm_controls_rhs, 
  vcov = vote_rank_total_russia_count_lm_controls_rhs_vcov
)

saveRDS(
  vote_rank_total_russia_count_lm_robust, 
  "./results/05_regression_analysis/vote_rank_total_russia_count_lm.rds"
)
saveRDS(
  vote_rank_total_russia_count_lm_controls_robust, 
  "./results/05_regression_analysis/vote_rank_total_russia_count_lm_controls.rds"
)
saveRDS(
  vote_rank_total_russia_count_lm_controls_black_robust, 
  "./results/05_regression_analysis/vote_rank_total_russia_count_lm_controls_black.rds"
)
saveRDS(
  vote_rank_total_russia_count_lm_controls_total_tweets_robust, 
  "./results/05_regression_analysis/vote_rank_total_russia_count_lm_controls_total_tweets.rds"
)
saveRDS(
  vote_rank_total_russia_count_log0_lm_robust, 
  "./results/05_regression_analysis/vote_rank_total_russia_count_log0_lm.rds"
)
saveRDS(
  vote_rank_total_russia_count_log0_lm_controls_robust, 
  "./results/05_regression_analysis/vote_rank_total_russia_count_log0_lm_controls.rds"
)
saveRDS(
  vote_rank_total_russia_count_lm_rhs_robust, 
  "./results/05_regression_analysis/vote_rank_total_russia_count_lm_rhs.rds"
)
saveRDS(
  vote_rank_total_russia_count_lm_controls_rhs_robust, 
  "./results/05_regression_analysis/vote_rank_total_russia_count_lm_controls_rhs.rds"
)

Sims[["vote_rank_total_russia_count_lm"]] <- sim_lm(
  vote_rank_total_russia_count_lm,
  vote_rank_total_russia_count_lm_vcov,
  us2016_recoded,
  "total_exposure_russia_log"
)
Sims[["vote_rank_total_russia_count_lm_controls"]] <- sim_lm(
  vote_rank_total_russia_count_lm_controls,
  vote_rank_total_russia_count_lm_controls_vcov,
  us2016_recoded,
  "total_exposure_russia_log"
)
Sims[["vote_rank_total_russia_count_log0_lm"]] <- sim_lm(
  vote_rank_total_russia_count_log0_lm,
  vote_rank_total_russia_count_log0_lm_vcov,
  us2016_recoded,
  c("total_exposure_russia_log", "total_exposure_russia_binary")
)
Sims[["vote_rank_total_russia_count_log0_lm_controls"]] <- sim_lm(
  vote_rank_total_russia_count_log0_lm_controls,
  vote_rank_total_russia_count_log0_lm_controls_vcov,
  us2016_recoded,
  c("total_exposure_russia_log", "total_exposure_russia_binary")
)


vote_rank_total_russia_binary_lm <- lm(
  build_formula(dv = "change_rank_trump_over_clinton_w1w3",
                iv = c("total_exposure_russia_binary")),
  data = us2016_recoded
)
vote_rank_total_russia_binary_lm_vcov <- sandwich::vcovHC(
  vote_rank_total_russia_binary_lm, 
  type = "HC0"
)
vote_rank_total_russia_binary_lm_robust <- lmtest::coeftest(
  vote_rank_total_russia_binary_lm, 
  vcov = vote_rank_total_russia_binary_lm_vcov
)

vote_rank_total_russia_binary_lm_controls <- lm(
  build_formula(dv = "change_rank_trump_over_clinton_w1w3",
                iv = c("total_exposure_russia_binary",
                       controls)),
  data = us2016_recoded
)
vote_rank_total_russia_binary_lm_controls_vcov <- sandwich::vcovHC(
  vote_rank_total_russia_binary_lm_controls, 
  type = "HC0"
)
vote_rank_total_russia_binary_lm_controls_robust <- lmtest::coeftest(
  vote_rank_total_russia_binary_lm_controls, 
  vcov = vote_rank_total_russia_binary_lm_controls_vcov
)

vote_rank_total_russia_binary_lm_controls_black <- lm(
  build_formula(dv = "change_rank_trump_over_clinton_w1w3",
                iv = c("total_exposure_russia_binary",
                       controls_black)),
  data = us2016_recoded
)
vote_rank_total_russia_binary_lm_controls_black_vcov <- sandwich::vcovHC(
  vote_rank_total_russia_binary_lm_controls_black, 
  type = "HC0"
)
vote_rank_total_russia_binary_lm_controls_black_robust <- lmtest::coeftest(
  vote_rank_total_russia_binary_lm_controls_black, 
  vcov = vote_rank_total_russia_binary_lm_controls_black_vcov
)

vote_rank_total_russia_binary_lm_controls_total_tweets <- lm(
  build_formula(dv = "change_rank_trump_over_clinton_w1w3",
                iv = c("total_exposure_russia_binary",
                       controls_total_tweets)),
  data = us2016_recoded
)
vote_rank_total_russia_binary_lm_controls_total_tweets_vcov <- sandwich::vcovHC(
  vote_rank_total_russia_binary_lm_controls_total_tweets, 
  type = "HC0"
)
vote_rank_total_russia_binary_lm_controls_total_tweets_robust <- lmtest::coeftest(
  vote_rank_total_russia_binary_lm_controls_total_tweets, 
  vcov = vote_rank_total_russia_binary_lm_controls_total_tweets_vcov
)

vote_rank_total_russia_binary_lm_rhs <- lm(
  build_formula(dv = "rank_trump_over_clinton_w3",
                iv = c("total_exposure_russia_binary", "rank_trump_over_clinton_w1")),
  data = us2016_recoded
)
vote_rank_total_russia_binary_lm_rhs_vcov <- sandwich::vcovHC(
  vote_rank_total_russia_binary_lm_rhs, 
  type = "HC0"
)
vote_rank_total_russia_binary_lm_rhs_robust <- lmtest::coeftest(
  vote_rank_total_russia_binary_lm_rhs, 
  vcov = vote_rank_total_russia_binary_lm_rhs_vcov
)

vote_rank_total_russia_binary_lm_controls_rhs <- lm(
  build_formula(dv = "rank_trump_over_clinton_w3",
                iv = c("total_exposure_russia_binary",
                       controls, "rank_trump_over_clinton_w1")),
  data = us2016_recoded
)
vote_rank_total_russia_binary_lm_controls_rhs_vcov <- sandwich::vcovHC(
  vote_rank_total_russia_binary_lm_controls_rhs, 
  type = "HC0"
)
vote_rank_total_russia_binary_lm_controls_rhs_robust <- lmtest::coeftest(
  vote_rank_total_russia_binary_lm_controls_rhs, 
  vcov = vote_rank_total_russia_binary_lm_controls_rhs_vcov
)

saveRDS(
  vote_rank_total_russia_binary_lm_robust, 
  "./results/05_regression_analysis/vote_rank_total_russia_binary_lm.rds"
)
saveRDS(
  vote_rank_total_russia_binary_lm_controls_robust, 
  "./results/05_regression_analysis/vote_rank_total_russia_binary_lm_controls.rds"
)
saveRDS(
  vote_rank_total_russia_binary_lm_controls_black_robust, 
  "./results/05_regression_analysis/vote_rank_total_russia_binary_lm_controls_black.rds"
)
saveRDS(
  vote_rank_total_russia_binary_lm_controls_total_tweets_robust, 
  "./results/05_regression_analysis/vote_rank_total_russia_binary_lm_controls_total_tweets.rds"
)
saveRDS(
  vote_rank_total_russia_binary_lm_rhs_robust, 
  "./results/05_regression_analysis/vote_rank_total_russia_binary_lm_rhs.rds"
)
saveRDS(
  vote_rank_total_russia_binary_lm_controls_rhs_robust, 
  "./results/05_regression_analysis/vote_rank_total_russia_binary_lm_controls_rhs.rds"
)

Sims[["vote_rank_total_russia_binary_lm"]] <- sim_lm(
  vote_rank_total_russia_binary_lm,
  vote_rank_total_russia_binary_lm_vcov,
  us2016_recoded,
  "total_exposure_russia_binary"
)
Sims[["vote_rank_total_russia_binary_lm_controls"]] <- sim_lm(
  vote_rank_total_russia_binary_lm_controls,
  vote_rank_total_russia_binary_lm_controls_vcov,
  us2016_recoded,
  "total_exposure_russia_binary"
)


vote_rank_total_russia_follow_lm <- lm(
  build_formula(dv = "change_rank_trump_over_clinton_w1w3",
                iv = c("follow_troll_russia_binary")),
  data = us2016_recoded
)
vote_rank_total_russia_follow_lm_vcov <- sandwich::vcovHC(
  vote_rank_total_russia_follow_lm, 
  type = "HC0"
)
vote_rank_total_russia_follow_lm_robust <- lmtest::coeftest(
  vote_rank_total_russia_follow_lm,
  vcov = vote_rank_total_russia_follow_lm_vcov
)

vote_rank_total_russia_follow_lm_controls <- lm(
  build_formula(dv = "change_rank_trump_over_clinton_w1w3",
                iv = c("follow_troll_russia_binary",
                       controls)),
  data = us2016_recoded
)
vote_rank_total_russia_follow_lm_controls_vcov <- sandwich::vcovHC(
  vote_rank_total_russia_follow_lm_controls, 
  type = "HC0"
)
vote_rank_total_russia_follow_lm_controls_robust <- lmtest::coeftest(
  vote_rank_total_russia_follow_lm_controls, 
  vcov = vote_rank_total_russia_follow_lm_controls_vcov
)

vote_rank_total_russia_follow_lm_controls_black <- lm(
  build_formula(dv = "change_rank_trump_over_clinton_w1w3",
                iv = c("follow_troll_russia_binary",
                       controls_black)),
  data = us2016_recoded
)
vote_rank_total_russia_follow_lm_controls_black_vcov <- sandwich::vcovHC(
  vote_rank_total_russia_follow_lm_controls_black, 
  type = "HC0"
)
vote_rank_total_russia_follow_lm_controls_black_robust <- lmtest::coeftest(
  vote_rank_total_russia_follow_lm_controls_black, 
  vcov = vote_rank_total_russia_follow_lm_controls_black_vcov
)

vote_rank_total_russia_follow_lm_controls_total_tweets <- lm(
  build_formula(dv = "change_rank_trump_over_clinton_w1w3",
                iv = c("follow_troll_russia_binary",
                       controls_total_tweets)),
  data = us2016_recoded
)
vote_rank_total_russia_follow_lm_controls_total_tweets_vcov <- sandwich::vcovHC(
  vote_rank_total_russia_follow_lm_controls_total_tweets, 
  type = "HC0"
)
vote_rank_total_russia_follow_lm_controls_total_tweets_robust <- lmtest::coeftest(
  vote_rank_total_russia_follow_lm_controls_total_tweets, 
  vcov = vote_rank_total_russia_follow_lm_controls_total_tweets_vcov
)

vote_rank_total_russia_follow_lm_rhs <- lm(
  build_formula(dv = "rank_trump_over_clinton_w3",
                iv = c("follow_troll_russia_binary", "rank_trump_over_clinton_w1")),
  data = us2016_recoded
)
vote_rank_total_russia_follow_lm_rhs_vcov <- sandwich::vcovHC(
  vote_rank_total_russia_follow_lm_rhs, 
  type = "HC0"
)
vote_rank_total_russia_follow_lm_rhs_robust <- lmtest::coeftest(
  vote_rank_total_russia_follow_lm_rhs, 
  vcov = vote_rank_total_russia_follow_lm_rhs_vcov
)

vote_rank_total_russia_follow_lm_controls_rhs <- lm(
  build_formula(dv = "rank_trump_over_clinton_w3",
                iv = c("follow_troll_russia_binary",
                       controls, "rank_trump_over_clinton_w1")),
  data = us2016_recoded
)
vote_rank_total_russia_follow_lm_controls_rhs_vcov <- sandwich::vcovHC(
  vote_rank_total_russia_follow_lm_controls_rhs, 
  type = "HC0"
)
vote_rank_total_russia_follow_lm_controls_rhs_robust <- lmtest::coeftest(
  vote_rank_total_russia_follow_lm_controls_rhs, 
  vcov = vote_rank_total_russia_follow_lm_controls_rhs_vcov
)

saveRDS(
  vote_rank_total_russia_follow_lm_robust,
  "./results/05_regression_analysis/vote_rank_total_russia_follow_lm.rds"
)
saveRDS(
  vote_rank_total_russia_follow_lm_controls_robust, 
  "./results/05_regression_analysis/vote_rank_total_russia_follow_lm_controls.rds"
)
saveRDS(
  vote_rank_total_russia_follow_lm_controls_black_robust, 
  "./results/05_regression_analysis/vote_rank_total_russia_follow_lm_controls_black.rds"
)
saveRDS(
  vote_rank_total_russia_follow_lm_controls_total_tweets_robust, 
  "./results/05_regression_analysis/vote_rank_total_russia_follow_lm_controls_total_tweets.rds"
)
saveRDS(
  vote_rank_total_russia_follow_lm_rhs_robust, 
  "./results/05_regression_analysis/vote_rank_total_russia_follow_lm_rhs.rds"
)
saveRDS(
  vote_rank_total_russia_follow_lm_controls_rhs_robust, 
  "./results/05_regression_analysis/vote_rank_total_russia_follow_lm_controls_rhs.rds"
)

Sims[["vote_rank_total_russia_follow_lm"]] <- sim_lm(
  vote_rank_total_russia_follow_lm,
  vote_rank_total_russia_follow_lm_vcov,
  us2016_recoded,
  "follow_troll_russia_binary"
)
Sims[["vote_rank_total_russia_follow_lm_controls"]] <- sim_lm(
  vote_rank_total_russia_follow_lm_controls,
  vote_rank_total_russia_follow_lm_controls_vcov,
  us2016_recoded,
  "follow_troll_russia_binary"
)


# Change in voting for Trump versus Clinton relative to ranking
vote_choice_total_russia_count_lm <- lm(
  build_formula(dv = "voted_trump_versus_clinton",
                iv = c("total_exposure_russia_log")),
  data = us2016_recoded
)
vote_choice_total_russia_count_lm_vcov <- sandwich::vcovHC(
  vote_choice_total_russia_count_lm, 
  type = "HC0"
)
vote_choice_total_russia_count_lm_robust <- lmtest::coeftest(
  vote_choice_total_russia_count_lm, 
  vcov = vote_choice_total_russia_count_lm_vcov
)

vote_choice_total_russia_count_lm_controls <- lm(
  build_formula(dv = "voted_trump_versus_clinton",
                iv = c("total_exposure_russia_log",
                       controls)),
  data = us2016_recoded
)
vote_choice_total_russia_count_lm_controls_vcov <- sandwich::vcovHC(
  vote_choice_total_russia_count_lm_controls,
  type = "HC0"
)
vote_choice_total_russia_count_lm_controls_robust <- lmtest::coeftest(
  vote_choice_total_russia_count_lm_controls,
  vcov = vote_choice_total_russia_count_lm_controls_vcov
)

vote_choice_total_russia_count_lm_controls_black <- lm(
  build_formula(dv = "voted_trump_versus_clinton",
                iv = c("total_exposure_russia_log",
                       controls_black)),
  data = us2016_recoded
)
vote_choice_total_russia_count_lm_controls_black_vcov <- sandwich::vcovHC(
  vote_choice_total_russia_count_lm_controls_black, 
  type = "HC0"
)
vote_choice_total_russia_count_lm_controls_black_robust <- lmtest::coeftest(
  vote_choice_total_russia_count_lm_controls_black, 
  vcov = vote_choice_total_russia_count_lm_controls_black_vcov
)

vote_choice_total_russia_count_lm_controls_total_tweets <- lm(
  build_formula(dv = "voted_trump_versus_clinton",
                iv = c("total_exposure_russia_log",
                       controls_total_tweets)),
  data = us2016_recoded
)
vote_choice_total_russia_count_lm_controls_total_tweets_vcov <- sandwich::vcovHC(
  vote_choice_total_russia_count_lm_controls_total_tweets, 
  type = "HC0"
)
vote_choice_total_russia_count_lm_controls_total_tweets_robust <- lmtest::coeftest(
  vote_choice_total_russia_count_lm_controls_total_tweets, 
  vcov = vote_choice_total_russia_count_lm_controls_total_tweets_vcov
)

vote_choice_total_russia_count_log0_lm <- lm(
  build_formula(dv = "voted_trump_versus_clinton",
                iv = c("total_exposure_russia_log0", "total_exposure_russia_binary")),
  data = us2016_recoded
)
vote_choice_total_russia_count_log0_lm_vcov <- sandwich::vcovHC(
  vote_choice_total_russia_count_log0_lm, 
  type = "HC0"
)
vote_choice_total_russia_count_log0_lm_robust <- lmtest::coeftest(
  vote_choice_total_russia_count_log0_lm, 
  vcov = vote_choice_total_russia_count_log0_lm_vcov
)

vote_choice_total_russia_count_log0_lm_controls <- lm(
  build_formula(dv = "voted_trump_versus_clinton",
                iv = c("total_exposure_russia_log0", "total_exposure_russia_binary",
                       controls)),
  data = us2016_recoded
)
vote_choice_total_russia_count_log0_lm_controls_vcov <- sandwich::vcovHC(
  vote_choice_total_russia_count_log0_lm_controls, 
  type = "HC0"
)
vote_choice_total_russia_count_log0_lm_controls_robust <- lmtest::coeftest(
  vote_choice_total_russia_count_log0_lm_controls, 
  vcov = vote_choice_total_russia_count_log0_lm_controls_vcov
)

vote_choice_total_russia_count_lm_rhs <- lm(
  build_formula(dv = "voted_trump_not_clinton",
                iv = c("total_exposure_russia_log", "rank_trump_over_clinton_w1")),
  data = us2016_recoded
)
vote_choice_total_russia_count_lm_rhs_vcov <- sandwich::vcovHC(
  vote_choice_total_russia_count_lm_rhs, 
  type = "HC0"
)
vote_choice_total_russia_count_lm_rhs_robust <- lmtest::coeftest(
  vote_choice_total_russia_count_lm_rhs, 
  vcov = vote_choice_total_russia_count_lm_rhs_vcov
)

vote_choice_total_russia_count_lm_controls_rhs <- lm(
  build_formula(dv = "voted_trump_not_clinton",
                iv = c("total_exposure_russia_log",
                       controls, "rank_trump_over_clinton_w1")),
  data = us2016_recoded
)
vote_choice_total_russia_count_lm_controls_rhs_vcov <- sandwich::vcovHC(
  vote_choice_total_russia_count_lm_controls_rhs, 
  type = "HC0"
)
vote_choice_total_russia_count_lm_controls_rhs_robust <- lmtest::coeftest(
  vote_choice_total_russia_count_lm_controls_rhs, 
  vcov = vote_choice_total_russia_count_lm_controls_rhs_vcov
)

saveRDS(
  vote_choice_total_russia_count_lm_robust, 
  "./results/05_regression_analysis/vote_choice_total_russia_count_lm.rds"
)
saveRDS(
  vote_choice_total_russia_count_lm_controls_robust, 
  "./results/05_regression_analysis/vote_choice_total_russia_count_lm_controls.rds"
)
saveRDS(
  vote_choice_total_russia_count_lm_controls_black_robust, 
  "./results/05_regression_analysis/vote_choice_total_russia_count_lm_controls_black.rds"
)
saveRDS(
  vote_choice_total_russia_count_lm_controls_total_tweets_robust, 
  "./results/05_regression_analysis/vote_choice_total_russia_count_lm_controls_total_tweets.rds"
)
saveRDS(
  vote_choice_total_russia_count_log0_lm_robust, 
  "./results/05_regression_analysis/vote_choice_total_russia_count_log0_lm.rds"
)
saveRDS(
  vote_choice_total_russia_count_log0_lm_controls_robust, 
  "./results/05_regression_analysis/vote_choice_total_russia_count_log0_lm_controls.rds"
)
saveRDS(
  vote_choice_total_russia_count_lm_rhs_robust, 
  "./results/05_regression_analysis/vote_choice_total_russia_count_lm_rhs.rds"
)
saveRDS(
  vote_choice_total_russia_count_lm_controls_rhs_robust, 
  "./results/05_regression_analysis/vote_choice_total_russia_count_lm_controls_rhs.rds"
)

Sims[["vote_choice_total_russia_count_lm"]] <- sim_lm(
  vote_choice_total_russia_count_lm,
  vote_choice_total_russia_count_lm_vcov,
  us2016_recoded,
  "total_exposure_russia_log"
)
Sims[["vote_choice_total_russia_count_lm_controls"]] <- sim_lm(
  vote_choice_total_russia_count_lm_controls,
  vote_choice_total_russia_count_lm_controls_vcov,
  us2016_recoded,
  "total_exposure_russia_log"
)
Sims[["vote_choice_total_russia_count_log0_lm"]] <- sim_lm(
  vote_choice_total_russia_count_log0_lm,
  vote_choice_total_russia_count_log0_lm_vcov,
  us2016_recoded,
  c("total_exposure_russia_log", "total_exposure_russia_binary")
)
Sims[["vote_choice_total_russia_count_log0_lm_controls"]] <- sim_lm(
  vote_choice_total_russia_count_log0_lm_controls,
  vote_choice_total_russia_count_log0_lm_controls_vcov,
  us2016_recoded,
  c("total_exposure_russia_log", "total_exposure_russia_binary")
)


vote_choice_total_russia_binary_lm <- lm(
  build_formula(dv = "voted_trump_versus_clinton",
                iv = c("total_exposure_russia_binary")),
  data = us2016_recoded
)
vote_choice_total_russia_binary_lm_vcov <- sandwich::vcovHC(
  vote_choice_total_russia_binary_lm, 
  type = "HC0"
)
vote_choice_total_russia_binary_lm_robust <- lmtest::coeftest(
  vote_choice_total_russia_binary_lm, 
  vcov = vote_choice_total_russia_binary_lm_vcov
)

vote_choice_total_russia_binary_lm_controls <- lm(
  build_formula(dv = "voted_trump_versus_clinton",
                iv = c("total_exposure_russia_binary",
                       controls)),
  data = us2016_recoded
)
vote_choice_total_russia_binary_lm_controls_vcov <- sandwich::vcovHC(
  vote_choice_total_russia_binary_lm_controls, 
  type = "HC0"
)
vote_choice_total_russia_binary_lm_controls_robust <- lmtest::coeftest(
  vote_choice_total_russia_binary_lm_controls, 
  vcov = vote_choice_total_russia_binary_lm_controls_vcov
)

vote_choice_total_russia_binary_lm_controls_black <- lm(
  build_formula(dv = "voted_trump_versus_clinton",
                iv = c("total_exposure_russia_binary",
                       controls_black)),
  data = us2016_recoded
)
vote_choice_total_russia_binary_lm_controls_black_vcov <- sandwich::vcovHC(
  vote_choice_total_russia_binary_lm_controls_black, 
  type = "HC0"
)
vote_choice_total_russia_binary_lm_controls_black_robust <- lmtest::coeftest(
  vote_choice_total_russia_binary_lm_controls_black, 
  vcov = vote_choice_total_russia_binary_lm_controls_black_vcov
)

vote_choice_total_russia_binary_lm_controls_total_tweets <- lm(
  build_formula(dv = "voted_trump_versus_clinton",
                iv = c("total_exposure_russia_binary",
                       controls_total_tweets)),
  data = us2016_recoded
)
vote_choice_total_russia_binary_lm_controls_total_tweets_vcov <- sandwich::vcovHC(
  vote_choice_total_russia_binary_lm_controls_total_tweets, 
  type = "HC0"
)
vote_choice_total_russia_binary_lm_controls_total_tweets_robust <- lmtest::coeftest(
  vote_choice_total_russia_binary_lm_controls_total_tweets, 
  vcov = vote_choice_total_russia_binary_lm_controls_total_tweets_vcov
)

vote_choice_total_russia_binary_lm_rhs <- lm(
  build_formula(dv = "voted_trump_not_clinton",
                iv = c("total_exposure_russia_binary", "rank_trump_over_clinton_w1")),
  data = us2016_recoded
)
vote_choice_total_russia_binary_lm_rhs_vcov <- sandwich::vcovHC(
  vote_choice_total_russia_binary_lm_rhs, 
  type = "HC0"
)
vote_choice_total_russia_binary_lm_rhs_robust <- lmtest::coeftest(
  vote_choice_total_russia_binary_lm_rhs,
  vcov = vote_choice_total_russia_binary_lm_rhs_vcov
)

vote_choice_total_russia_binary_lm_controls_rhs <- lm(
  build_formula(dv = "voted_trump_not_clinton",
                iv = c("total_exposure_russia_binary",
                       controls, "rank_trump_over_clinton_w1")),
  data = us2016_recoded
)
vote_choice_total_russia_binary_lm_controls_rhs_vcov <- sandwich::vcovHC(
  vote_choice_total_russia_binary_lm_controls_rhs, 
  type = "HC0"
)
vote_choice_total_russia_binary_lm_controls_rhs_robust <- lmtest::coeftest(
  vote_choice_total_russia_binary_lm_controls_rhs, 
  vcov = vote_choice_total_russia_binary_lm_controls_rhs_vcov
)

saveRDS(
  vote_choice_total_russia_binary_lm_robust, 
  "./results/05_regression_analysis/vote_choice_total_russia_binary_lm.rds"
)
saveRDS(
  vote_choice_total_russia_binary_lm_controls_robust, 
  "./results/05_regression_analysis/vote_choice_total_russia_binary_lm_controls.rds"
)
saveRDS(
  vote_choice_total_russia_binary_lm_controls_black_robust, 
  "./results/05_regression_analysis/vote_choice_total_russia_binary_lm_controls_black.rds"
)
saveRDS(
  vote_choice_total_russia_binary_lm_controls_total_tweets_robust, 
  "./results/05_regression_analysis/vote_choice_total_russia_binary_lm_controls_total_tweets.rds"
)
saveRDS(
  vote_choice_total_russia_binary_lm_rhs_robust, 
  "./results/05_regression_analysis/vote_choice_total_russia_binary_lm_rhs.rds"
)
saveRDS(
  vote_choice_total_russia_binary_lm_controls_rhs_robust, 
  "./results/05_regression_analysis/vote_choice_total_russia_binary_lm_controls_rhs.rds"
)

Sims[["vote_choice_total_russia_binary_lm"]] <- sim_lm(
  vote_choice_total_russia_binary_lm,
  vote_choice_total_russia_binary_lm_vcov,
  us2016_recoded,
  "total_exposure_russia_binary"
)
Sims[["vote_choice_total_russia_binary_lm_controls"]] <- sim_lm(
  vote_choice_total_russia_binary_lm_controls,
  vote_choice_total_russia_binary_lm_controls_vcov,
  us2016_recoded,
  "total_exposure_russia_binary"
)


vote_choice_total_russia_follow_lm <- lm(
  build_formula(dv = "voted_trump_versus_clinton",
                iv = c("follow_troll_russia_binary")),
  data = us2016_recoded
)
vote_choice_total_russia_follow_lm_vcov <- sandwich::vcovHC(
  vote_choice_total_russia_follow_lm, 
  type = "HC0"
)
vote_choice_total_russia_follow_lm_robust <- lmtest::coeftest(
  vote_choice_total_russia_follow_lm, 
  vcov = vote_choice_total_russia_follow_lm_vcov
)

vote_choice_total_russia_follow_lm_controls <- lm(
  build_formula(dv = "voted_trump_versus_clinton",
                iv = c("follow_troll_russia_binary",
                       controls)),
  data = us2016_recoded
)
vote_choice_total_russia_follow_lm_controls_vcov <- sandwich::vcovHC(
  vote_choice_total_russia_follow_lm_controls, 
  type = "HC0"
)
vote_choice_total_russia_follow_lm_controls_robust <- lmtest::coeftest(
  vote_choice_total_russia_follow_lm_controls, 
  vcov = vote_choice_total_russia_follow_lm_controls_vcov
)

vote_choice_total_russia_follow_lm_controls_black <- lm(
  build_formula(dv = "voted_trump_versus_clinton",
                iv = c("follow_troll_russia_binary",
                       controls_black)),
  data = us2016_recoded
)
vote_choice_total_russia_follow_lm_controls_black_vcov <- sandwich::vcovHC(
  vote_choice_total_russia_follow_lm_controls_black, 
  type = "HC0"
)
vote_choice_total_russia_follow_lm_controls_black_robust <- lmtest::coeftest(
  vote_choice_total_russia_follow_lm_controls_black, 
  vcov = vote_choice_total_russia_follow_lm_controls_black_vcov
)

vote_choice_total_russia_follow_lm_controls_total_tweets <- lm(
  build_formula(dv = "voted_trump_versus_clinton",
                iv = c("follow_troll_russia_binary",
                       controls_total_tweets)),
  data = us2016_recoded
)
vote_choice_total_russia_follow_lm_controls_total_tweets_vcov <- sandwich::vcovHC(
  vote_choice_total_russia_follow_lm_controls_total_tweets, 
  type = "HC0"
)
vote_choice_total_russia_follow_lm_controls_total_tweets_robust <- lmtest::coeftest(
  vote_choice_total_russia_follow_lm_controls_total_tweets, 
  vcov = vote_choice_total_russia_follow_lm_controls_total_tweets_vcov
)

vote_choice_total_russia_follow_lm_rhs <- lm(
  build_formula(dv = "voted_trump_not_clinton",
                iv = c("follow_troll_russia_binary", "rank_trump_over_clinton_w1")),
  data = us2016_recoded
)
vote_choice_total_russia_follow_lm_rhs_vcov <- sandwich::vcovHC(
  vote_choice_total_russia_follow_lm_rhs, 
  type = "HC0"
)
vote_choice_total_russia_follow_lm_rhs_robust <- lmtest::coeftest(
  vote_choice_total_russia_follow_lm_rhs, 
  vcov = vote_choice_total_russia_follow_lm_rhs_vcov
)

vote_choice_total_russia_follow_lm_controls_rhs <- lm(
  build_formula(dv = "voted_trump_not_clinton",
                iv = c("follow_troll_russia_binary",
                       controls, "rank_trump_over_clinton_w1")),
  data = us2016_recoded
)
vote_choice_total_russia_follow_lm_controls_rhs_vcov <- sandwich::vcovHC(
  vote_choice_total_russia_follow_lm_controls_rhs, 
  type = "HC0"
)
vote_choice_total_russia_follow_lm_controls_rhs_robust <- lmtest::coeftest(
  vote_choice_total_russia_follow_lm_controls_rhs, 
  vcov = vote_choice_total_russia_follow_lm_controls_rhs_vcov
)

saveRDS(
  vote_choice_total_russia_follow_lm_robust, 
  "./results/05_regression_analysis/vote_choice_total_russia_follow_lm.rds"
)
saveRDS(
  vote_choice_total_russia_follow_lm_controls_robust, 
  "./results/05_regression_analysis/vote_choice_total_russia_follow_lm_controls.rds"
)
saveRDS(
  vote_choice_total_russia_follow_lm_controls_black_robust, 
  "./results/05_regression_analysis/vote_choice_total_russia_follow_lm_controls_black.rds"
)
saveRDS(
  vote_choice_total_russia_follow_lm_controls_total_tweets_robust, 
  "./results/05_regression_analysis/vote_choice_total_russia_follow_lm_controls_total_tweets.rds"
)
saveRDS(
  vote_choice_total_russia_follow_lm_rhs_robust, 
  "./results/05_regression_analysis/vote_choice_total_russia_follow_lm_rhs.rds"
)
saveRDS(
  vote_choice_total_russia_follow_lm_controls_rhs_robust, 
  "./results/05_regression_analysis/vote_choice_total_russia_follow_lm_controls_rhs.rds"
)

Sims[["vote_choice_total_russia_follow_lm"]] <- sim_lm(
  vote_choice_total_russia_follow_lm,
  vote_choice_total_russia_follow_lm_vcov,
  us2016_recoded,
  "follow_troll_russia_binary"
)
Sims[["vote_choice_total_russia_follow_lm_controls"]] <- sim_lm(
  vote_choice_total_russia_follow_lm_controls,
  vote_choice_total_russia_follow_lm_controls_vcov,
  us2016_recoded,
  "follow_troll_russia_binary"
)


# Change in voting away from Trump to {Clinton, 3rd party, or not voting},
# or in votting away from Clinton to {Trump, 3rd party, or not voting}
vote_choice_benefit_total_russia_count_lm <- lm(
  build_formula(dv = "voted_trump_versus_clinton_benefit",
                iv = c("total_exposure_russia_log")),
  data = us2016_recoded
)
vote_choice_benefit_total_russia_count_lm_vcov <- sandwich::vcovHC(
  vote_choice_benefit_total_russia_count_lm, 
  type = "HC0"
)
vote_choice_benefit_total_russia_count_lm_robust <- lmtest::coeftest(
  vote_choice_benefit_total_russia_count_lm, 
  vcov = vote_choice_benefit_total_russia_count_lm_vcov
)

vote_choice_benefit_total_russia_count_lm_controls <- lm(
  build_formula(dv = "voted_trump_versus_clinton_benefit",
                iv = c("total_exposure_russia_log",
                       controls)),
  data = us2016_recoded
)
vote_choice_benefit_total_russia_count_lm_controls_vcov <- sandwich::vcovHC(
  vote_choice_benefit_total_russia_count_lm_controls, 
  type = "HC0"
)
vote_choice_benefit_total_russia_count_lm_controls_robust <- lmtest::coeftest(
  vote_choice_benefit_total_russia_count_lm_controls, 
  vcov = vote_choice_benefit_total_russia_count_lm_controls_vcov
)

vote_choice_benefit_total_russia_count_lm_controls_black <- lm(
  build_formula(dv = "voted_trump_versus_clinton_benefit",
                iv = c("total_exposure_russia_log",
                       controls_black)),
  data = us2016_recoded
)
vote_choice_benefit_total_russia_count_lm_controls_black_vcov <- sandwich::vcovHC(
  vote_choice_benefit_total_russia_count_lm_controls_black, 
  type = "HC0"
)
vote_choice_benefit_total_russia_count_lm_controls_black_robust <- lmtest::coeftest(
  vote_choice_benefit_total_russia_count_lm_controls_black, 
  vcov = vote_choice_benefit_total_russia_count_lm_controls_black_vcov
)

vote_choice_benefit_total_russia_count_lm_controls_total_tweets <- lm(
  build_formula(dv = "voted_trump_versus_clinton_benefit",
                iv = c("total_exposure_russia_log",
                       controls_total_tweets)),
  data = us2016_recoded
)
vote_choice_benefit_total_russia_count_lm_controls_total_tweets_vcov <- sandwich::vcovHC(
  vote_choice_benefit_total_russia_count_lm_controls_total_tweets, 
  type = "HC0"
)
vote_choice_benefit_total_russia_count_lm_controls_total_tweets_robust <- lmtest::coeftest(
  vote_choice_benefit_total_russia_count_lm_controls_total_tweets, 
  vcov = vote_choice_benefit_total_russia_count_lm_controls_total_tweets_vcov
)

vote_choice_benefit_total_russia_count_log0_lm <- lm(
  build_formula(dv = "voted_trump_versus_clinton_benefit",
                iv = c("total_exposure_russia_log0",  "total_exposure_russia_binary")),
  data = us2016_recoded
)
vote_choice_benefit_total_russia_count_log0_lm_vcov <- sandwich::vcovHC(
  vote_choice_benefit_total_russia_count_log0_lm, 
  type = "HC0"
)
vote_choice_benefit_total_russia_count_log0_lm_robust <- lmtest::coeftest(
  vote_choice_benefit_total_russia_count_log0_lm, 
  vcov = vote_choice_benefit_total_russia_count_log0_lm_vcov
)

vote_choice_benefit_total_russia_count_log0_lm_controls <- lm(
  build_formula(dv = "voted_trump_versus_clinton_benefit",
                iv = c("total_exposure_russia_log0", "total_exposure_russia_binary",
                       controls)),
  data = us2016_recoded
)
vote_choice_benefit_total_russia_count_log0_lm_controls_vcov <- sandwich::vcovHC(
  vote_choice_benefit_total_russia_count_log0_lm_controls, 
  type = "HC0"
)
vote_choice_benefit_total_russia_count_log0_lm_controls_robust <- lmtest::coeftest(
  vote_choice_benefit_total_russia_count_log0_lm_controls, 
  vcov = vote_choice_benefit_total_russia_count_log0_lm_controls_vcov
)

vote_choice_benefit_total_russia_count_lm_rhs <- lm(
  build_formula(dv = "voted_trump_not_clinton_benefit",
                iv = c("total_exposure_russia_log", "rank_trump_over_clinton_w1")),
  data = us2016_recoded
)
vote_choice_benefit_total_russia_count_lm_rhs_vcov <- sandwich::vcovHC(
  vote_choice_benefit_total_russia_count_lm_rhs, 
  type = "HC0"
)
vote_choice_benefit_total_russia_count_lm_rhs_robust <- lmtest::coeftest(
  vote_choice_benefit_total_russia_count_lm_rhs, 
  vcov = vote_choice_benefit_total_russia_count_lm_rhs_vcov
)

vote_choice_benefit_total_russia_count_lm_controls_rhs <- lm(
  build_formula(dv = "voted_trump_not_clinton_benefit",
                iv = c("total_exposure_russia_log",
                       controls, "rank_trump_over_clinton_w1")),
  data = us2016_recoded
)
vote_choice_benefit_total_russia_count_lm_controls_rhs_vcov <- sandwich::vcovHC(
  vote_choice_benefit_total_russia_count_lm_controls_rhs, 
  type = "HC0"
)
vote_choice_benefit_total_russia_count_lm_controls_rhs_robust <- lmtest::coeftest(
  vote_choice_benefit_total_russia_count_lm_controls_rhs, 
  vcov = vote_choice_benefit_total_russia_count_lm_controls_rhs_vcov
)

saveRDS(
  vote_choice_benefit_total_russia_count_lm_robust, 
  "./results/05_regression_analysis/vote_choice_benefit_total_russia_count_lm.rds"
)
saveRDS(
  vote_choice_benefit_total_russia_count_lm_controls_robust, 
  "./results/05_regression_analysis/vote_choice_benefit_total_russia_count_lm_controls.rds"
)
saveRDS(
  vote_choice_benefit_total_russia_count_lm_controls_black_robust, 
  "./results/05_regression_analysis/vote_choice_benefit_total_russia_count_lm_controls_black.rds"
)
saveRDS(
  vote_choice_benefit_total_russia_count_lm_controls_total_tweets_robust, 
  "./results/05_regression_analysis/vote_choice_benefit_total_russia_count_lm_controls_total_tweets.rds"
)
saveRDS(
  vote_choice_benefit_total_russia_count_log0_lm_robust, 
  "./results/05_regression_analysis/vote_choice_benefit_total_russia_count_log0_lm.rds"
)
saveRDS(
  vote_choice_benefit_total_russia_count_log0_lm_controls_robust, 
  "./results/05_regression_analysis/vote_choice_benefit_total_russia_count_log0_lm_controls.rds"
)
saveRDS(
  vote_choice_benefit_total_russia_count_lm_rhs_robust, 
  "./results/05_regression_analysis/vote_choice_benefit_total_russia_count_lm_rhs.rds"
)
saveRDS(
  vote_choice_benefit_total_russia_count_lm_controls_rhs_robust, 
  "./results/05_regression_analysis/vote_choice_benefit_total_russia_count_lm_controls_rhs.rds"
)

Sims[["vote_choice_benefit_total_russia_count_lm"]] <- sim_lm(
  vote_choice_benefit_total_russia_count_lm,
  vote_choice_benefit_total_russia_count_lm_vcov,
  us2016_recoded,
  "total_exposure_russia_log"
)
Sims[["vote_choice_benefit_total_russia_count_lm_controls"]] <- sim_lm(
  vote_choice_benefit_total_russia_count_lm_controls,
  vote_choice_benefit_total_russia_count_lm_controls_vcov,
  us2016_recoded,
  "total_exposure_russia_log"
)
Sims[["vote_choice_benefit_total_russia_count_log0_lm"]] <- sim_lm(
  vote_choice_benefit_total_russia_count_log0_lm,
  vote_choice_benefit_total_russia_count_log0_lm_vcov,
  us2016_recoded,
  c("total_exposure_russia_log", "total_exposure_russia_binary")
)
Sims[["vote_choice_benefit_total_russia_count_log0_lm_controls"]] <- sim_lm(
  vote_choice_benefit_total_russia_count_log0_lm_controls,
  vote_choice_benefit_total_russia_count_log0_lm_controls_vcov,
  us2016_recoded,
  c("total_exposure_russia_log", "total_exposure_russia_binary")
)


vote_choice_benefit_total_russia_binary_lm <- lm(
  build_formula(dv = "voted_trump_versus_clinton_benefit",
                iv = c("total_exposure_russia_binary")),
  data = us2016_recoded
)
vote_choice_benefit_total_russia_binary_lm_vcov <- sandwich::vcovHC(
  vote_choice_benefit_total_russia_binary_lm, 
  type = "HC0"
)
vote_choice_benefit_total_russia_binary_lm_robust <- lmtest::coeftest(
  vote_choice_benefit_total_russia_binary_lm, 
  vcov = vote_choice_benefit_total_russia_binary_lm_vcov
)

vote_choice_benefit_total_russia_binary_lm_controls <- lm(
  build_formula(dv = "voted_trump_versus_clinton_benefit",
                iv = c("total_exposure_russia_binary",
                       controls)),
  data = us2016_recoded
)
vote_choice_benefit_total_russia_binary_lm_controls_vcov <- sandwich::vcovHC(
  vote_choice_benefit_total_russia_binary_lm_controls, 
  type = "HC0"
)
vote_choice_benefit_total_russia_binary_lm_controls_robust <- lmtest::coeftest(
  vote_choice_benefit_total_russia_binary_lm_controls, 
  vcov = vote_choice_benefit_total_russia_binary_lm_controls_vcov
)

vote_choice_benefit_total_russia_binary_lm_controls_black <- lm(
  build_formula(dv = "voted_trump_versus_clinton_benefit",
                iv = c("total_exposure_russia_binary",
                       controls_black)),
  data = us2016_recoded
)
vote_choice_benefit_total_russia_binary_lm_controls_black_vcov <- sandwich::vcovHC(
  vote_choice_benefit_total_russia_binary_lm_controls_black, 
  type = "HC0"
)
vote_choice_benefit_total_russia_binary_lm_controls_black_robust <- lmtest::coeftest(
  vote_choice_benefit_total_russia_binary_lm_controls_black, 
  vcov = vote_choice_benefit_total_russia_binary_lm_controls_black_vcov
)

vote_choice_benefit_total_russia_binary_lm_controls_total_tweets <- lm(
  build_formula(dv = "voted_trump_versus_clinton_benefit",
                iv = c("total_exposure_russia_binary",
                       controls_total_tweets)),
  data = us2016_recoded
)
vote_choice_benefit_total_russia_binary_lm_controls_total_tweets_vcov <- sandwich::vcovHC(
  vote_choice_benefit_total_russia_binary_lm_controls_total_tweets, 
  type = "HC0"
)
vote_choice_benefit_total_russia_binary_lm_controls_total_tweets_robust <- lmtest::coeftest(
  vote_choice_benefit_total_russia_binary_lm_controls_total_tweets, 
  vcov = vote_choice_benefit_total_russia_binary_lm_controls_total_tweets_vcov
)

vote_choice_benefit_total_russia_binary_lm_rhs <- lm(
  build_formula(dv = "voted_trump_not_clinton_benefit",
                iv = c("total_exposure_russia_binary", "rank_trump_over_clinton_w1")),
  data = us2016_recoded
)
vote_choice_benefit_total_russia_binary_lm_rhs_vcov <- sandwich::vcovHC(
  vote_choice_benefit_total_russia_binary_lm_rhs,
  type = "HC0"
)
vote_choice_benefit_total_russia_binary_lm_rhs_robust <- lmtest::coeftest(
  vote_choice_benefit_total_russia_binary_lm_rhs,
  vcov = vote_choice_benefit_total_russia_binary_lm_rhs_vcov
)

vote_choice_benefit_total_russia_binary_lm_controls_rhs <- lm(
  build_formula(dv = "voted_trump_not_clinton_benefit",
                iv = c("total_exposure_russia_binary",
                       controls, "rank_trump_over_clinton_w1")),
  data = us2016_recoded
)
vote_choice_benefit_total_russia_binary_lm_controls_rhs_vcov <- sandwich::vcovHC(
  vote_choice_benefit_total_russia_binary_lm_controls_rhs, 
  type = "HC0"
)
vote_choice_benefit_total_russia_binary_lm_controls_rhs_robust <- lmtest::coeftest(
  vote_choice_benefit_total_russia_binary_lm_controls_rhs, 
  vcov = vote_choice_benefit_total_russia_binary_lm_controls_rhs_vcov
)

saveRDS(
  vote_choice_benefit_total_russia_binary_lm_robust, 
  "./results/05_regression_analysis/vote_choice_benefit_total_russia_binary_lm.rds"
)
saveRDS(
  vote_choice_benefit_total_russia_binary_lm_controls_robust, 
  "./results/05_regression_analysis/vote_choice_benefit_total_russia_binary_lm_controls.rds"
)
saveRDS(
  vote_choice_benefit_total_russia_binary_lm_controls_black_robust, 
  "./results/05_regression_analysis/vote_choice_benefit_total_russia_binary_lm_controls_black.rds"
)
saveRDS(
  vote_choice_benefit_total_russia_binary_lm_controls_total_tweets_robust,
  "./results/05_regression_analysis/vote_choice_benefit_total_russia_binary_lm_controls_total_tweets.rds"
)
saveRDS(
  vote_choice_benefit_total_russia_binary_lm_rhs_robust, 
  "./results/05_regression_analysis/vote_choice_benefit_total_russia_binary_lm_rhs.rds"
)
saveRDS(
  vote_choice_benefit_total_russia_binary_lm_controls_rhs_robust, 
  "./results/05_regression_analysis/vote_choice_benefit_total_russia_binary_lm_controls_rhs.rds"
)

Sims[["vote_choice_benefit_total_russia_binary_lm"]] <- sim_lm(
  vote_choice_benefit_total_russia_binary_lm,
  vote_choice_benefit_total_russia_binary_lm_vcov,
  us2016_recoded,
  "total_exposure_russia_binary"
)
Sims[["vote_choice_benefit_total_russia_binary_lm_controls"]] <- sim_lm(
  vote_choice_benefit_total_russia_binary_lm_controls,
  vote_choice_benefit_total_russia_binary_lm_controls_vcov,
  us2016_recoded,
  "total_exposure_russia_binary"
)


vote_choice_benefit_total_russia_follow_lm <- lm(
  build_formula(dv = "voted_trump_versus_clinton_benefit",
                iv = c("follow_troll_russia_binary")),
  data = us2016_recoded
)
vote_choice_benefit_total_russia_follow_lm_vcov <- sandwich::vcovHC(
  vote_choice_benefit_total_russia_follow_lm, 
  type = "HC0"
)
vote_choice_benefit_total_russia_follow_lm_robust <- lmtest::coeftest(
  vote_choice_benefit_total_russia_follow_lm, 
  vcov = vote_choice_benefit_total_russia_follow_lm_vcov
)

vote_choice_benefit_total_russia_follow_lm_controls <- lm(
  build_formula(dv = "voted_trump_versus_clinton_benefit",
                iv = c("follow_troll_russia_binary",
                       controls)),
  data = us2016_recoded
)
vote_choice_benefit_total_russia_follow_lm_controls_vcov <- sandwich::vcovHC(
  vote_choice_benefit_total_russia_follow_lm_controls, 
  type = "HC0"
)
vote_choice_benefit_total_russia_follow_lm_controls_robust <- lmtest::coeftest(
  vote_choice_benefit_total_russia_follow_lm_controls, 
  vcov = vote_choice_benefit_total_russia_follow_lm_controls_vcov
)

vote_choice_benefit_total_russia_follow_lm_controls_black <- lm(
  build_formula(dv = "voted_trump_versus_clinton_benefit",
                iv = c("follow_troll_russia_binary",
                       controls_black)),
  data = us2016_recoded
)
vote_choice_benefit_total_russia_follow_lm_controls_black_vcov <- sandwich::vcovHC(
  vote_choice_benefit_total_russia_follow_lm_controls_black,
  type = "HC0"
)
vote_choice_benefit_total_russia_follow_lm_controls_black_robust <- lmtest::coeftest(
  vote_choice_benefit_total_russia_follow_lm_controls_black, 
  vcov = vote_choice_benefit_total_russia_follow_lm_controls_black_vcov
)

vote_choice_benefit_total_russia_follow_lm_controls_total_tweets <- lm(
  build_formula(dv = "voted_trump_versus_clinton_benefit",
                iv = c("follow_troll_russia_binary",
                       controls_total_tweets)),
  data = us2016_recoded
)
vote_choice_benefit_total_russia_follow_lm_controls_total_tweets_vcov <- sandwich::vcovHC(
  vote_choice_benefit_total_russia_follow_lm_controls_total_tweets, 
  type = "HC0"
)
vote_choice_benefit_total_russia_follow_lm_controls_total_tweets_robust <- lmtest::coeftest(
  vote_choice_benefit_total_russia_follow_lm_controls_total_tweets, 
  vcov = vote_choice_benefit_total_russia_follow_lm_controls_total_tweets_vcov
)

vote_choice_benefit_total_russia_follow_lm_rhs <- lm(
  build_formula(dv = "voted_trump_not_clinton_benefit",
                iv = c("follow_troll_russia_binary", "rank_trump_over_clinton_w1")),
  data = us2016_recoded
)
vote_choice_benefit_total_russia_follow_lm_rhs_vcov <- sandwich::vcovHC(
  vote_choice_benefit_total_russia_follow_lm_rhs, 
  type = "HC0"
)
vote_choice_benefit_total_russia_follow_lm_rhs_robust <- lmtest::coeftest(
  vote_choice_benefit_total_russia_follow_lm_rhs, 
  vcov = vote_choice_benefit_total_russia_follow_lm_rhs_vcov
)

vote_choice_benefit_total_russia_follow_lm_controls_rhs <- lm(
  build_formula(dv = "voted_trump_not_clinton_benefit",
                iv = c("follow_troll_russia_binary",
                       controls, "rank_trump_over_clinton_w1")),
  data = us2016_recoded
)
vote_choice_benefit_total_russia_follow_lm_controls_rhs_vcov <- sandwich::vcovHC(
  vote_choice_benefit_total_russia_follow_lm_controls_rhs, 
  type = "HC0"
)
vote_choice_benefit_total_russia_follow_lm_controls_rhs_robust <- lmtest::coeftest(
  vote_choice_benefit_total_russia_follow_lm_controls_rhs, 
  vcov = vote_choice_benefit_total_russia_follow_lm_controls_rhs_vcov
)

saveRDS(
  vote_choice_benefit_total_russia_follow_lm_robust, 
  "./results/05_regression_analysis/vote_choice_benefit_total_russia_follow_lm.rds"
)
saveRDS(
  vote_choice_benefit_total_russia_follow_lm_controls_robust, 
  "./results/05_regression_analysis/vote_choice_benefit_total_russia_follow_lm_controls.rds"
)
saveRDS(
  vote_choice_benefit_total_russia_follow_lm_controls_black_robust, 
  "./results/05_regression_analysis/vote_choice_benefit_total_russia_follow_lm_controls_black.rds"
)
saveRDS(
  vote_choice_benefit_total_russia_follow_lm_controls_total_tweets_robust, 
  "./results/05_regression_analysis/vote_choice_benefit_total_russia_follow_lm_controls_total_tweets.rds"
)
saveRDS(
  vote_choice_benefit_total_russia_follow_lm_rhs_robust, 
  "./results/05_regression_analysis/vote_choice_benefit_total_russia_follow_lm_rhs.rds"
)
saveRDS(
  vote_choice_benefit_total_russia_follow_lm_controls_rhs_robust, 
  "./results/05_regression_analysis/vote_choice_benefit_total_russia_follow_lm_controls_rhs.rds"
)

Sims[["vote_choice_benefit_total_russia_follow_lm"]] <- sim_lm(
  vote_choice_benefit_total_russia_follow_lm,
  vote_choice_benefit_total_russia_follow_lm_vcov,
  us2016_recoded,
  "follow_troll_russia_binary"
)
Sims[["vote_choice_benefit_total_russia_follow_lm_controls"]] <- sim_lm(
  vote_choice_benefit_total_russia_follow_lm_controls,
  vote_choice_benefit_total_russia_follow_lm_controls_vcov,
  us2016_recoded,
  "follow_troll_russia_binary"
)

S <- dplyr::bind_cols(Sims) %>%
     tidyr::pivot_longer(cols = 1:length(Sims)) %>%
     dplyr::arrange(name)

saveRDS(S, "./results/05_regression_analysis/Vote_Models_Simulations.rds")


# Models of Issue Position and Ideological Distance -----------------------

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

get_distance_models <- function(dv, iv, data, rhs = FALSE) {

  model_out <- list()
  for(the_dv in dv) {
    
    if(rhs == TRUE) {

      the_dv <- gsub("_w1w3", "_w3", the_dv)
      the_dv <- gsub("change_", "", the_dv)
      the_iv <- c(iv, gsub("_w3", "_w1", the_dv))

    } else {

      the_iv <- iv

    }

    new_model <- lm(build_formula(dv = the_dv,
                                  iv = the_iv),
                                  data = data)
    new_model_vcov <- sandwich::vcovHC(new_model, type = "HC0")
    new_model_robust <- lmtest::coeftest(new_model, vcov = new_model_vcov)

    model_out[[the_dv]] <- new_model_robust

  }

  return(model_out)

}

issue_distance_variables <- c("change_ideo_distance_clinton_versus_trump_w1w3",
                              "change_immigration_distance_clinton_versus_trump_w1w3",
                              "change_banmuslimsBAD_distance_clinton_versus_trump_w1w3",
                              "change_buildwallBAD_distance_clinton_versus_trump_w1w3",
                              "change_healthcare_distance_clinton_versus_trump_w1w3",
                              "change_expandACA_distance_clinton_versus_trump_w1w3",
                              "change_freetrade_distance_clinton_versus_trump_w1w3",
                              "change_tarrifsBAD_distance_clinton_versus_trump_w1w3",
                              "change_milforce_distance_clinton_versus_trump_w1w3")

issue_distance_log_exposure_russia <- get_distance_models(
  dv = issue_distance_variables,
  iv = "total_exposure_russia_log",
  data = us2016_recoded
)
issue_distance_log_exposure_russia_controls <- get_distance_models(
  dv = issue_distance_variables,
  iv = c("total_exposure_russia_log", controls),
  data = us2016_recoded
)
issue_distance_log_exposure_russia_controls_black <- get_distance_models(
  dv = issue_distance_variables,
  iv = c("total_exposure_russia_log", controls_black),
  data = us2016_recoded
)
issue_distance_log_exposure_russia_controls_total_tweets <- get_distance_models(
  dv = issue_distance_variables,
  iv = c("total_exposure_russia_log", controls_total_tweets),
  data = us2016_recoded
)
issue_distance_log_exposure_russia_rhs <- get_distance_models(
  dv = issue_distance_variables,
  iv = "total_exposure_russia_log",
  rhs = TRUE,
  data = us2016_recoded
)
issue_distance_log_exposure_russia_controls_rhs <- get_distance_models(
  dv = issue_distance_variables,
  iv = c("total_exposure_russia_log", controls),
  rhs = TRUE,
  data = us2016_recoded
)

issue_distance_log0_exposure_russia <- get_distance_models(
  dv = issue_distance_variables,
  iv = c("total_exposure_russia_log0", "total_exposure_russia_binary"),
  data = us2016_recoded
)
issue_distance_log0_exposure_russia_controls <- get_distance_models(
  dv = issue_distance_variables,
  iv = c("total_exposure_russia_log0", "total_exposure_russia_binary", controls),
  data = us2016_recoded
)
issue_distance_log0_exposure_russia_rhs <- get_distance_models(
  dv = issue_distance_variables,
  iv = c("total_exposure_russia_log0", "total_exposure_russia_binary"),
  rhs = TRUE,
  data = us2016_recoded
)
issue_distance_log0_exposure_russia_controls_rhs <- get_distance_models(
  dv = issue_distance_variables,
  iv = c("total_exposure_russia_log0", "total_exposure_russia_binary", controls),
  rhs = TRUE,
  data = us2016_recoded
)

issue_distance_binary_exposure_russia <- get_distance_models(
  dv = issue_distance_variables,
  iv = "total_exposure_russia_binary",
  data = us2016_recoded
)
issue_distance_binary_exposure_russia_controls <- get_distance_models(
  dv = issue_distance_variables,
  iv = c("total_exposure_russia_binary", controls),
  data = us2016_recoded
)
issue_distance_binary_exposure_russia_controls_black <- get_distance_models(
  dv = issue_distance_variables,
  iv = c("total_exposure_russia_binary", controls_black),
  data = us2016_recoded
)
issue_distance_binary_exposure_russia_controls_total_tweets <- get_distance_models(
  dv = issue_distance_variables,
  iv = c("total_exposure_russia_binary", controls_total_tweets),
  data = us2016_recoded
)
issue_distance_binary_exposure_russia_rhs <- get_distance_models(
  dv = issue_distance_variables,
  iv = "total_exposure_russia_binary",
  rhs = TRUE,
  data = us2016_recoded
)
issue_distance_binary_exposure_russia_controls_rhs <- get_distance_models(
  dv = issue_distance_variables,
  iv = c("total_exposure_russia_binary", controls),
  rhs = TRUE,
  data = us2016_recoded
)

issue_distance_follow_troll_russia <- get_distance_models(
  dv = issue_distance_variables,
  iv = "follow_troll_russia_binary",
  data = us2016_recoded
)
issue_distance_follow_troll_russia_controls <- get_distance_models(
  dv = issue_distance_variables,
  iv = c("follow_troll_russia_binary", controls),
  data = us2016_recoded
)
issue_distance_follow_troll_russia_controls_black <- get_distance_models(
  dv = issue_distance_variables,
  iv = c("follow_troll_russia_binary", controls_black),
  data = us2016_recoded
)
issue_distance_follow_troll_russia_controls_total_tweets <- get_distance_models(
  dv = issue_distance_variables,
  iv = c("follow_troll_russia_binary", controls_total_tweets),
  data = us2016_recoded
)
issue_distance_follow_troll_russia_rhs <- get_distance_models(
  dv = issue_distance_variables,
  iv = "follow_troll_russia_binary",
  rhs = TRUE,
  data = us2016_recoded
)
issue_distance_follow_troll_russia_controls_rhs <- get_distance_models(
  dv = issue_distance_variables,
  iv = c("follow_troll_russia_binary", controls),
  rhs = TRUE,
  data = us2016_recoded
)

saveRDS(
  issue_distance_log_exposure_russia,
  "./results/05_regression_analysis/issue_distance_log_exposure_russia.rds"
)
saveRDS(
  issue_distance_log_exposure_russia_controls,
  "./results/05_regression_analysis/issue_distance_log_exposure_russia_controls.rds"
)
saveRDS(
  issue_distance_log_exposure_russia_controls_black,
  "./results/05_regression_analysis/issue_distance_log_exposure_russia_controls_black.rds"
)
saveRDS(
  issue_distance_log_exposure_russia_controls_total_tweets,
  "./results/05_regression_analysis/issue_distance_log_exposure_russia_controls_total_tweets.rds"
)
saveRDS(
  issue_distance_log_exposure_russia_rhs,
  "./results/05_regression_analysis/issue_distance_log_exposure_russia_rhs.rds"
)
saveRDS(
  issue_distance_log_exposure_russia_controls_rhs,
  "./results/05_regression_analysis/issue_distance_log_exposure_russia_controls_rhs.rds"
)

saveRDS(
  issue_distance_log0_exposure_russia,
  "./results/05_regression_analysis/issue_distance_log0_exposure_russia.rds"
)
saveRDS(
  issue_distance_log0_exposure_russia_controls,
  "./results/05_regression_analysis/issue_distance_log0_exposure_russia_controls.rds"
)
saveRDS(
  issue_distance_log0_exposure_russia_rhs,
  "./results/05_regression_analysis/issue_distance_log0_exposure_russia_rhs.rds"
)
saveRDS(
  issue_distance_log0_exposure_russia_controls_rhs,
  "./results/05_regression_analysis/issue_distance_log0_exposure_russia_controls_rhs.rds"
)

saveRDS(
  issue_distance_binary_exposure_russia,
  "./results/05_regression_analysis/issue_distance_binary_exposure_russia.rds"
)
saveRDS(
  issue_distance_binary_exposure_russia_controls,
  "./results/05_regression_analysis/issue_distance_binary_exposure_russia_controls.rds"
)
saveRDS(
  issue_distance_binary_exposure_russia_controls_black,
  "./results/05_regression_analysis/issue_distance_binary_exposure_russia_controls_black.rds"
)
saveRDS(
  issue_distance_binary_exposure_russia_controls_total_tweets,
  "./results/05_regression_analysis/issue_distance_binary_exposure_russia_controls_total_tweets.rds"
)
saveRDS(
  issue_distance_binary_exposure_russia_rhs,
  "./results/05_regression_analysis/issue_distance_binary_exposure_russia_rhs.rds"
)
saveRDS(
  issue_distance_binary_exposure_russia_controls_rhs,
  "./results/05_regression_analysis/issue_distance_binary_exposure_russia_controls_rhs.rds"
)

saveRDS(
  issue_distance_follow_troll_russia,
  "./results/05_regression_analysis/issue_distance_follow_troll_russia.rds"
)
saveRDS(
  issue_distance_follow_troll_russia_controls,
  "./results/05_regression_analysis/issue_distance_follow_troll_russia_controls.rds"
)
saveRDS(
  issue_distance_follow_troll_russia_controls_black,
  "./results/05_regression_analysis/issue_distance_follow_troll_russia_controls_black.rds"
)
saveRDS(
  issue_distance_follow_troll_russia_controls_total_tweets,
  "./results/05_regression_analysis/issue_distance_follow_troll_russia_controls_total_tweets.rds"
)
saveRDS(
  issue_distance_follow_troll_russia_rhs,
  "./results/05_regression_analysis/issue_distance_follow_troll_russia_rhs.rds"
)
saveRDS(
  issue_distance_follow_troll_russia_controls_rhs,
  "./results/05_regression_analysis/issue_distance_follow_troll_russia_controls_rhs.rds"
)


candidate_distance_variables <- c("ideo_clinton_trump_distance_w1w3",
                                  "immigration_clinton_trump_distance_w1w3",
                                  "banmuslimsBAD_clinton_trump_distance_w1w3",
                                  "buildwallBAD_clinton_trump_distance_w1w3",
                                  "healthcare_clinton_trump_distance_w1w3",
                                  "expandACA_clinton_trump_distance_w1w3",
                                  "freetrade_clinton_trump_distance_w1w3",
                                  "tarrifsBAD_clinton_trump_distance_w1w3",
                                  "milforce_clinton_trump_distance_w1w3")

candidate_distance_log_exposure_russia <- get_distance_models(
  dv = candidate_distance_variables,
  iv = "total_exposure_russia_log",
  data = us2016_recoded
)
candidate_distance_log_exposure_russia_controls <- get_distance_models(
  dv = candidate_distance_variables,
  iv = c("total_exposure_russia_log", controls),
  data = us2016_recoded
)
candidate_distance_log_exposure_russia_controls_black <- get_distance_models(
  dv = candidate_distance_variables,
  iv = c("total_exposure_russia_log", controls_black),
  data = us2016_recoded
)
candidate_distance_log_exposure_russia_controls_total_tweets <- get_distance_models(
  dv = candidate_distance_variables,
  iv = c("total_exposure_russia_log", controls_total_tweets),
  data = us2016_recoded
)
candidate_distance_log_exposure_russia_rhs <- get_distance_models(
  dv = candidate_distance_variables,
  iv = "total_exposure_russia_log",
  rhs = TRUE,
  data = us2016_recoded
)
candidate_distance_log_exposure_russia_controls_rhs <- get_distance_models(
  dv = candidate_distance_variables,
  iv = c("total_exposure_russia_log", controls),
  rhs = TRUE,
  data = us2016_recoded
)

candidate_distance_log0_exposure_russia <- get_distance_models(
  dv = candidate_distance_variables,
  iv = c("total_exposure_russia_log", "total_exposure_russia_binary"),
  data = us2016_recoded
)
candidate_distance_log0_exposure_russia_controls <- get_distance_models(
  dv = candidate_distance_variables,
  iv = c("total_exposure_russia_log", "total_exposure_russia_binary", controls),
  data = us2016_recoded
)
candidate_distance_log0_exposure_russia_rhs <- get_distance_models(
  dv = candidate_distance_variables,
  iv = c("total_exposure_russia_log", "total_exposure_russia_binary"),
  rhs = TRUE,
  data = us2016_recoded
)
candidate_distance_log0_exposure_russia_controls_rhs <- get_distance_models(
  dv = candidate_distance_variables,
  iv = c("total_exposure_russia_log", "total_exposure_russia_binary", controls),
  rhs = TRUE,
  data = us2016_recoded
)

candidate_distance_binary_exposure_russia <- get_distance_models(
  dv = candidate_distance_variables,
  iv = "total_exposure_russia_binary",
  data = us2016_recoded
)
candidate_distance_binary_exposure_russia_controls <- get_distance_models(
  dv = candidate_distance_variables,
  iv = c("total_exposure_russia_binary", controls),
  data = us2016_recoded
)
candidate_distance_binary_exposure_russia_controls_black <- get_distance_models(
  dv = candidate_distance_variables,
  iv = c("total_exposure_russia_binary", controls_black),
  data = us2016_recoded
)
candidate_distance_binary_exposure_russia_controls_total_tweets <- get_distance_models(
  dv = candidate_distance_variables,
  iv = c("total_exposure_russia_binary", controls_total_tweets),
  data = us2016_recoded
)
candidate_distance_binary_exposure_russia_rhs <- get_distance_models(
  dv = candidate_distance_variables,
  iv = "total_exposure_russia_binary",
  rhs = TRUE,
  data = us2016_recoded
)
candidate_distance_binary_exposure_russia_controls_rhs <- get_distance_models(
  dv = candidate_distance_variables,
  iv = c("total_exposure_russia_binary", controls),
  rhs = TRUE,
  data = us2016_recoded
)

candidate_distance_follow_troll_russia <- get_distance_models(
  dv = candidate_distance_variables,
  iv = "follow_troll_russia_binary",
  data = us2016_recoded
)
candidate_distance_follow_troll_russia_controls <- get_distance_models(
  dv = candidate_distance_variables,
  iv = c("follow_troll_russia_binary", controls),
  data = us2016_recoded
)
candidate_distance_follow_troll_russia_controls_black <- get_distance_models(
  dv = candidate_distance_variables,
  iv = c("follow_troll_russia_binary", controls_black),
  data = us2016_recoded
)
candidate_distance_follow_troll_russia_controls_total_tweets <- get_distance_models(
  dv = candidate_distance_variables,
  iv = c("follow_troll_russia_binary", controls_total_tweets),
  data = us2016_recoded
)
candidate_distance_follow_troll_russia_rhs <- get_distance_models(
  dv = candidate_distance_variables,
  iv = "follow_troll_russia_binary",
  rhs = TRUE,
  data = us2016_recoded
)
candidate_distance_follow_troll_russia_controls_rhs <- get_distance_models(
  dv = candidate_distance_variables,
  iv = c("follow_troll_russia_binary", controls),
  rhs = TRUE,
  data = us2016_recoded
)


saveRDS(
  candidate_distance_log_exposure_russia,
  "./results/05_regression_analysis/candidate_distance_log_exposure_russia.rds"
)
saveRDS(
  candidate_distance_log_exposure_russia_controls,
  "./results/05_regression_analysis/candidate_distance_log_exposure_russia_controls.rds"
)
saveRDS(
  candidate_distance_log_exposure_russia_controls_black,
  "./results/05_regression_analysis/candidate_distance_log_exposure_russia_controls_black.rds"
)
saveRDS(
  candidate_distance_log_exposure_russia_controls_total_tweets,
  "./results/05_regression_analysis/candidate_distance_log_exposure_russia_controls_total_tweets.rds"
)
saveRDS(
  candidate_distance_log_exposure_russia_rhs,
  "./results/05_regression_analysis/candidate_distance_log_exposure_russia_rhs.rds"
)
saveRDS(
  candidate_distance_log_exposure_russia_controls_rhs,
  "./results/05_regression_analysis/candidate_distance_log_exposure_russia_controls_rhs.rds"
)

saveRDS(
  candidate_distance_log0_exposure_russia,
  "./results/05_regression_analysis/candidate_distance_log0_exposure_russia.rds"
)
saveRDS(
  candidate_distance_log0_exposure_russia_controls,
  "./results/05_regression_analysis/candidate_distance_log0_exposure_russia_controls.rds"
)
saveRDS(
  candidate_distance_log0_exposure_russia_rhs,
  "./results/05_regression_analysis/candidate_distance_log0_exposure_russia_rhs.rds"
)
saveRDS(
  candidate_distance_log0_exposure_russia_controls_rhs,
  "./results/05_regression_analysis/candidate_distance_log0_exposure_russia_controls_rhs.rds"
)

saveRDS(
  candidate_distance_binary_exposure_russia,
  "./results/05_regression_analysis/candidate_distance_binary_exposure_russia.rds"
)
saveRDS(
  candidate_distance_binary_exposure_russia_controls,
  "./results/05_regression_analysis/candidate_distance_binary_exposure_russia_controls.rds"
)
saveRDS(
  candidate_distance_binary_exposure_russia_controls_black,
  "./results/05_regression_analysis/candidate_distance_binary_exposure_russia_controls_black.rds"
)
saveRDS(
  candidate_distance_binary_exposure_russia_controls_total_tweets,
  "./results/05_regression_analysis/candidate_distance_binary_exposure_russia_controls_total_tweets.rds"
)
saveRDS(
  candidate_distance_binary_exposure_russia_rhs,
  "./results/05_regression_analysis/candidate_distance_binary_exposure_russia_rhs.rds"
)
saveRDS(
  candidate_distance_binary_exposure_russia_controls_rhs,
  "./results/05_regression_analysis/candidate_distance_binary_exposure_russia_controls_rhs.rds"
)

saveRDS(
  candidate_distance_follow_troll_russia,
  "./results/05_regression_analysis/candidate_distance_follow_troll_russia.rds"
)
saveRDS(
  candidate_distance_follow_troll_russia_controls,
  "./results/05_regression_analysis/candidate_distance_follow_troll_russia_controls.rds"
)
saveRDS(
  candidate_distance_follow_troll_russia_controls_black,
  "./results/05_regression_analysis/candidate_distance_follow_troll_russia_controls_black.rds"
)
saveRDS(
  candidate_distance_follow_troll_russia_controls_total_tweets,
  "./results/05_regression_analysis/candidate_distance_follow_troll_russia_controls_total_tweets.rds"
)
saveRDS(
  candidate_distance_follow_troll_russia_rhs,
  "./results/05_regression_analysis/candidate_distance_follow_troll_russia_rhs.rds"
)
saveRDS(
  candidate_distance_follow_troll_russia_controls_rhs,
  "./results/05_regression_analysis/candidate_distance_follow_troll_russia_controls_rhs.rds"
)
