#!/usr/bin/env Rscript
# coding: utf-8

#===============================================================================
# 
#  Tom Paskhalis, 2022-09-14
#  Gregory Eady, 2022-09-14
#
#  collection of functions for regression analysis and simulation
#
#===============================================================================


# Regression Analysis -----------------------------------------------------

# Build formula from character vectors of LHS (DV) and RHS (IV)
build_formula <- function(dv, iv) {
  specification <- paste(dv, paste(iv, collapse = " + "), sep = " ~ ")
  
  as.formula(specification)
}

get_coefs <- function(fitted_model, dv, iv, multiple_comparisons = NULL, alpha = 0.05) {
  
  # Adjust CIs for multiple comparisons
  if (!is.null(multiple_comparisons)) {
    if (!is.numeric(multiple_comparisons)) {
      stop("Number of comparisons must be numeric.")
    } else {
      if (length(multiple_comparisons) > 1) {
        stop("Number of comparisons must be a single integer.")
      } else {
        # Apply Bonferroni correction
        lower95 = confint(fitted_model, level = 1 - (alpha/multiple_comparisons))[, 1]
        upper95 = confint(fitted_model, level = 1 - (alpha/multiple_comparisons))[, 2]
      }
    }
  } else {
    lower95 = confint(fitted_model, level = 1 - alpha)[, 1]
    upper95 = confint(fitted_model, level = 1 - alpha)[, 2]
  }
  
  O <- data.frame(iv = iv,
                  variable = names(coef(fitted_model)),
                  coef = coef(fitted_model),
                  p = ifelse(
                    is(fitted_model, "coeftest"),
                    fitted_model[, 4], # coeftest
                    summary(fitted_model)$coefficients[, 4] # lmtest
                  ),
                  lower95 = lower95,
                  upper95 = upper95)
  
  rownames(O) <- 1:nrow(O)
  
  O$treatment <- ifelse(grepl("exposure|follow", O$variable),
                        "Treatment", "Controls")
  
  O$variable[O$variable == "(Intercept)"] <- "Constant"
  O$variable[O$variable == "genderFemale"] <- "Female"
  O$variable[O$variable == "internet_social_media_w1"] <- "Social media use"
  O$variable[O$variable == "age"] <- "Age"
  O$variable[O$variable == "income"] <- "Income"
  O$variable[O$variable == "educationCollege+"] <- "College degree"
  O$variable[O$variable == "pid7"] <- "Party ID"
  O$variable[O$variable == "raceNon-White"] <- "Non-white"
  O$variable[O$variable == "regionNortheast"] <- "Region: Northeast"
  O$variable[O$variable == "regionMidwest"] <- "Region: Midwest"
  O$variable[O$variable == "regionWest"] <- "Region: West"
  O$variable[grep("_w1", O$variable)] <- "Wave 1 response"
  O$variable[grep("exposure|follow", O$variable)] <- dv
  
  return(O)
  
}

get_sims <- function(fitted_model, n_sims) {
  
  coefs_sim <- rmvnorm(n_sims,
                       fitted_model$coefficients,
                       vcov(fitted_model))
  prediction_data <- fitted_model$data
  prediction_data <- prediction_data[, -which(names(prediction_data) == "pid7")]
  
  sims <- list()
  coef <- lower95 <- upper95 <- numeric()
  pid7 <- seq(0, 1, by = 0.05)
  
  for(i in 1:length(pid7)) {
    sims[[i]] <- t(sapply(1:n_sims, function(x) {
      fitted_model$coefficients <- coefs_sim[x, ]
      predict(fitted_model,
              type = "response",
              newdata = data.frame(prediction_data, pid7 = pid7[i]),
              na.action = "na.pass") }))
    coef[i] <- mean(rowMeans(sims[[i]], na.rm = TRUE))
    lower95[i] <- quantile(rowMeans(sims[[i]], na.rm = TRUE), 0.05)
    upper95[i] <- quantile(rowMeans(sims[[i]], na.rm = TRUE), 0.95)
    
  }
  
  return(data.frame(pid7 = pid7,
                    coef = coef,
                    lower95 = lower95,
                    upper95 = upper95))
  
}
