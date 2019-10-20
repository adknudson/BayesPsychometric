#' Fits a psychometric function
#' @param formula A formula that one would pass to `glm` or similar.
#' @param data A data.frame or list
#' @param link A link function such as "logit" or "probit".
#' @export
bayesPF <- function(formula, data, link,
                    chains = 1, iter = 2000, warmup = NULL, thin = 1,
                    sample = TRUE, cores = 1, seed = NULL, ...) {

  require(rstan)
  concat <- function(...) {
    paste(..., collapse = "", sep = "")
  }

  # Build the model from the formula -----------------------------------------
  fstan <- f2stan(formula, data, link)
  model_code <- fstan[["StanCode"]]
  data <- fstan[["data"]]
  data_classes <- fstan[["data_classes"]]
  fls <- fstan[["fls"]]

  # Prepare arguments for Stan -----------------------------------------------
  # Number of cores -------------------
  if (cores < 0) {
    cores <- 1
  }
  options(mc.cores = min(as.integer(cores), parallel::detectCores()))
  rstan_options(auto_write = TRUE)

  # Parameter inits -------------------
  inits <- list()
  # Intercept inits
  if (fls[["include_intercept"]]) {
    inits[["a0"]] <- 0
    # Factor intercepts
    if (length(data_classes[["factor_vars"]]) > 0) {
      for (fv in data_classes[["factor_vars"]]) {
        inits[[concat("a_", fv)]] <- rep(0, data[[concat("N_", fv)]])
      }
    }
  }
  # Slope inits
  if (length(data_classes[["numeric_vars"]]) > 0) {
    for (nv in data_classes[["numeric_vars"]]) {
      inits[[concat("b", nv)]] <- 0
      # Factor slope terms
      if (length(data_classes[["factor_vars"]]) > 0) {
        for (fv in data_classes[["factor_vars"]]) {
          inits[[concat("b", nv, "_", fv)]] <- rep(0, data[[concat("N_", fv)]])
        } # for fv
      } # if fv
    } # for nv
  } # if nv
  inits <- replicate(chains, inits, simplify = FALSE)

  # Warmup and iterations -------------
  if (is.null(warmup)) warmup <- iter %/% 2
  if (warmup > iter) {
    warning("'warmup' must not be greater than 'iter'. Assuming that the user meant for 'iter' to be the number of post-warmup draws. Setting iter = iter + warmup.")
    iter <- iter + warmup
  }

  # Sending all arguments to stan --------------------------------------------
  stan(
    model_name = "BayesPF",
    model_code = model_code,
    data       = data,
    init       = inits,
    iter       = iter,
    warmup     = warmup,
    chains     = chains,
    thin       = thin,
    seed       = seed, ...)
}
