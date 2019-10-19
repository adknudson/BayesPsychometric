#' Fits a psychometric function
#' @param formula A formula that one would pass to `glm` or similar.
#' @param data A data.frame or list
#' @param link A link function such as "logit" or "probit".
#' @export
bayesPF <- function(formula, data, link, ...) {

  stan_model <- f2stan(formula, data, link)

  rstan::stan(model_code = stan_model, ...)

}
