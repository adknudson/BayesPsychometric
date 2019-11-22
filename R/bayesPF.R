#' Fits a psychometric function
#' @param formula A formula that one would pass to `glm` or similar
#' @param data A data.frame or list
#' @param link A link function (either "logit" or "probit")
#' @param ... Other parameters to be passed to `rstan::stan()`.
#' @param sample Whether to sample from the model or just build the model
#'   without sampling
#' @param return_f2stan Return the data and parameters generated internally by
#'   `f2stan()`. Useful for debugging.
#' @param return_stan_fit Return the full model created by `rstan::stan()`.
#'   Useful for debugging or using other `rstan` functions.
#' @export
#' @examples
#' # Load the data
#' data_binomial  <- Sample_Data_Binomial
#' data_bernoulli <- Sample_Data_Bernoulli
#'
#' # Fit a full model using binomial response data. By default, it will only
#' # return the posterior samples for the coefficients
#' fit1.1 <- bayesPF(y|k ~ x1 + age + gender, data_binomial, "logit",
#'                   chains = 2, cores = 2, iter = 8000, warmup = 2000)
#'
#' str(fit1.1)
#'
#' # Fit a full model using binary response data. To get the fitted Stan
#' # model, specify `return_stan_fit = TRUE`. To get diagnostic information
#' # such as the Stan code and transformed data, specify `return_f2stan = TRUE`
#' fit1.2 <- bayesPF(y ~ x1 + age + gender, data_bernoulli, "probit",
#'                   return_f2stan = TRUE, return_stan_fit = TRUE)
#' str(fit1.2)
#' extract(fit1.2$fit)
bayesPF <- function(formula, data, link, adaptive_pooling = FALSE,
                    chains = 1, iter = 2000, warmup = 1000, thin = 1,
                    cores = 1, ..., sample = TRUE) {

  concat <- function(...) {
    paste(..., collapse = "", sep = "")
  }

  # Build the model from the formula -----------------------------------------
  # NOTE: this function modifies the data so it's important to reassign `data`
  f_ls <- f2stan(formula, data, link, adaptive_pooling)

  # Extract objects from f_ls
  stan_code <- f_ls[["stan_code"]]
  data <- f_ls[["data"]]
  metadata <- f_ls[["metadata"]]
  coef_list <- metadata[["coefs"]]
  has_intercept <- f_ls[["has_intercept"]]
  nvs <- metadata[["vars"]][["numeric"]]
  fvs <- metadata[["vars"]][["factor"]]
  has_numeric <- length(nvs) > 0
  has_factor  <- length(fvs) > 0

  # Prepare arguments for Stan -----------------------------------------------
  # Number of cores -------------------
  if (cores < 0) cores <- 1
  options(mc.cores = min(as.integer(cores), parallel::detectCores()))
  rstan::rstan_options(auto_write = TRUE)

  # Parameter inits -------------------
  inits <- list()
  # Intercept inits
  if (has_intercept) {
    inits[["a"]] <- "random"
    # Factor intercepts
    if (has_factor) {
      for (fv in fvs) {
        inits[[concat("a_", fv)]] <- rep("random", data[[concat("N_", fv)]])
      }
    }
  }

  # Slope inits
  if (has_numeric) {
    for (nv in nvs) {
      inits[[concat("b", nv)]] <- "random"
      # Factor slope terms
      if (has_factor) {
        for (fv in fvs) {
          inits[[concat("b", nv, "_", fv)]] <- rep("random", data[[concat("N_", fv)]])
        } # for fv
      } # if fv
    } # for nv
  } # if nv

  # Adaptive pooling inits
  if(adaptive_pooling && has_factor) {
    for (coefs in coef_list) {
      for (coef in coefs[-1]) {
        inits[[concat("sd_", coef)]] <- 1
      }
    }
  } # if adaptive pooling

  inits <- replicate(chains, inits, simplify = FALSE)

  # Warmup and iterations -------------
  if (is.null(warmup)) warmup <- iter %/% 2
  if (warmup > iter) {
    warning("'warmup' must not be greater than 'iter'. Assuming that the user meant for 'iter' to be the number of post-warmup draws. Setting iter = iter + warmup.")
    iter <- iter + warmup
  }

  # Sending all arguments to stan --------------------------------------------
  if (sample) {
    message("The model is now compiling. This may take a minute.")
    fit <- rstan::stan(
      model_name = "BayesPF",
      model_code = stan_code,
      data       = data,
      init       = inits,
      iter       = iter,
      warmup     = warmup,
      chains     = chains,
      thin       = thin, ...)
  } else {
    message("If you don't want to return samples, you may also want to consider the function f2stan() to generate the Stan code.")
  }

  # Return the results
  list(stanfit = fit, f_ls = f_ls)
}
