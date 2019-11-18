devtools::load_all()
library(tidyverse)

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

fit <- bayesPF(y|k ~ x1 + age + gender, data = Sample_Data_Binomial,
               link = "logit", chains = 4, iter = 3500, warmup = 2000,
               thin = 4, cores = 4)

fit_nofactor <- bayesPF(y|k ~ x1, data = Sample_Data_Binomial,
                        link = "logit", chains = 4, iter = 3500, warmup = 2000,
                        thin = 4, cores = 4)

fit_noslope <- bayesPF(y|k ~ 1 + age + gender, Sample_Data_Binomial,
                       link = "logit", chains = 4, iter = 3500, warmup = 2000,
                       thin = 4, cores = 4)

fit_nointercept <- bayesPF(y|k ~ 0 + x1, Sample_Data_Binomial,
                           link = "logit", chains = 4, iter = 3500, warmup = 2000,
                           thin = 4, cores = 4)


extractPSS <- function(fit, method = c("Posterior", "Classic"),
                       level = 0.95, CI = NULL) {
  metadata <- fit[["f2stan"]][["metadata"]]
  has_intercept <- fit[["f2stan"]][["has_intercept"]]
  nvs <- metadata[["vars"]][["numeric"]]

  factorSamples <- fit[["factorSamples"]]

  # If there is no intercept, then return 0
  if (!has_intercept) {
    warning("Any model without an intercept will have a fixed PSS, at F(0), where 'F' is the link function specified in the model.")
    return(0)
  }

  # If there is/are no slope term(s), then return NULL
  if (length(nvs) == 0) {
    warning("The model must have at least 1 numeric predictor in order to calculate the PSS. This specific function requires exactly 1 numeric predictor.")
    return(NULL)
  }

  # If there are more than one slope terms, then throw an error with a message
  # saying that a multiple-slope method is implemented by a different function
  if (length(nvs) > 1) {
    stop("This function must take exactly 1 numeric predictor to calculate the PSS. Your model has ", length(nvs), ": ", nvs)
  }

  # Finally, if there is exactly one slope term, then calculate the PSS
    # If  method == 'Posterior', then return median of PSS
    # Else if method == 'Classic', then return (1-alpha)% credible interval
    # and mean
  if (!is.null(factorSamples)) {
    pss_dist <- purrr::map2(factorSamples[[1]],
                            factorSamples[[2]],
                            ~ -.x / .y)
  } else {
    intercept_coef <- metadata[["coefs"]][[1]][1]
    slope_coef <- metadata[["coefs"]][[2]][1]
    pss_dist <- -fit[["samples"]][[intercept_coef]] / fit[["samples"]][[slope_coef]]
  }
  pss_dist
}

extractPSS(fit)
extractPSS(fit_nofactor)

extractPSS(fit_noslope)
extractPSS(fit_nointercept)
