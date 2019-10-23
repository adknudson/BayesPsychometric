#' Fits a psychometric function
#' @param formula A formula that one would pass to `glm` or similar.
#' @param data A data.frame or list
#' @param link A link function such as "logit" or "probit".
#' @export
#' @examples
#' # Build an arbitrary data set
#' n <- 300
#' x1 <- runif(n, -1, 1)
#' x2 <- sample((-n%/%2):(n%/%2), n, TRUE)
#' a <- 0.6
#' b1 <- -4.2
#' b2 <- -0.005
#' p <- 1 / (1 + exp(-(a + b1*x1 + b2*x2)))
#' y <- rbinom(n, size = 1, prob = p)
#' size <- sample(3:5, n, TRUE)
#' y2 <- rbinom(n, size = size, prob = p)
#' gender <- factor(sample(c("male", "female"), n, TRUE))
#' age <- factor(sample(c("<25", "25-50", ">50"), n, TRUE),
#'               levels = c("<25", "25-50", ">50"), ordered = TRUE)
#' dat_bern <- data.frame(y = y, x1 = x1, x2 = x2,
#'                        gender = gender, age = age)
#' dat_binom <- data.frame(y = y2, k = size, prop = y2 / size, x1 = x1, x2 = x2,
#'                         gender = gender, age = age)
#'
#' fit <- bayesPF(y|k ~ x1 + x2 + age, dat_binom, "logit",
#'                chains = 2, cores = 2, iter = 8000, warmup = 2000)
#' fit.samples <- extract(fit)
#' hist(fit.samples$bx2, breaks = 50)
bayesPF <- function(formula, data, link,
                    chains = 1, iter = 2000, warmup = 1000, thin = 1,
                    sample = TRUE, cores = 1, ...) {

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
  fit <- stan(
    model_name = "BayesPF",
    model_code = model_code,
    data       = data,
    init       = inits,
    iter       = iter,
    warmup     = warmup,
    chains     = chains,
    thin       = thin, ...)

  # Extract and process samples ----------------------------------------------
  samples <- extract(fit)
  samples <- .processSamples(samples, data,
                             data_classes, fls[["include_intercept"]])

  list(fit = fit, samples = samples)
}
