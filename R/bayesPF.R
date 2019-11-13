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
                    cores = 1, ...,
                    sample = TRUE,
                    return_stan_fit = FALSE) {

  concat <- function(...) {
    paste(..., collapse = "", sep = "")
  }

  # Initialize an empty return list
  ret_list <- list()

  # Build the model from the formula -----------------------------------------
  # NOTE: this function modifies the data so it's important to reassign `data`
  fstan <- f2stan(formula, data, link, adaptive_pooling)
  ret_list[["f2stan"]] <- fstan

  # Extract objects from fstan
  model_code <- fstan[["StanCode"]]
  data <- fstan[["data"]]
  metadata <- fstan[["metadata"]]
  coef_list <- metadata$coefs
  has_intercept <- fstan[["has_intercept"]]
  nvs <- metadata[["vars"]][["numeric"]]
  fvs <- metadata[["vars"]][["factor"]]

  # Prepare arguments for Stan -----------------------------------------------
  # Number of cores -------------------
  if (cores < 0) cores <- 1
  options(mc.cores = min(as.integer(cores), parallel::detectCores()))
  rstan::rstan_options(auto_write = TRUE)

  # Parameter inits -------------------
  inits <- list()
  # Intercept inits
  if (has_intercept) {
    inits[["a0"]] <- 0
    # Factor intercepts
    if (length(fvs) > 0) {
      for (fv in fvs) {
        inits[[concat("a_", fv)]] <- rep(0, data[[concat("N_", fv)]])
      }
    }
  }

  # Slope inits
  if (length(nvs) > 0) {
    for (nv in nvs) {
      inits[[concat("b", nv)]] <- 0
      # Factor slope terms
      if (length(fvs) > 0) {
        for (fv in fvs) {
          inits[[concat("b", nv, "_", fv)]] <- rep(0, data[[concat("N_", fv)]])
        } # for fv
      } # if fv
    } # for nv
  } # if nv

  # Adaptive pooling inits
  if(adaptive_pooling && length(fvs) > 0) {
    for (coefs in coef_list) {
      for (coef in coefs[-1]) {
        inits[[concat("sd_", coef)]] <- 1
      }
    }
  } # if adaptive pooling

  inits <- replicate(chains, inits, simplify = FALSE)
  ret_list[["inits"]] <- inits

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
      model_code = model_code,
      data       = data,
      init       = inits,
      iter       = iter,
      warmup     = warmup,
      chains     = chains,
      thin       = thin, ...)

    # Extract and process samples ----------------------------------------------
    samples <- rstan::extract(fit)
    samples <- .processSamples(samples, data, metadata, has_intercept)
    ret_list[["samples"]] <- samples

    if (return_stan_fit) ret_list[["fit"]] <- fit
  }

  # Return the results
  ret_list
}
