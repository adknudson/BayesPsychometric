#' Fits a psychometric function
#' @param formula A formula that one would pass to `glm` or similar.
#' @param data A data.frame or list
#' @param link A link function such as "logit" or "probit".
#' @export
bayesPF <- function(formula, data, link, ...) {

  if (class(formula) == "list") {
    # assume that we are given an flist intended for rethinking::map2stan
    flist <- formula
  } else {
    # Create an flist from the formula given the data and link function
    flist <- f2flist(formula, data, link)
  }

  data <- .factorLevelsToIntegers(formula, data)

  # Fit the model using map2stan
  rethinking::map2stan(flist, data, ...)
}
