#' Computes the logistic of a number
#'
#' The function is defined by \deqn{\frac{1}{1 + \exp(-x)}}{1 / ( 1 + exp(-x) )}
#' This function is the inverse of the [logit()] function.
#'
#' @param x A real number, including ±Inf
#' @return The logistic of \code{x}, a real number in the range \code{[0, 1]}
#' @export
logistic <- function(x) {
  1 / (1 + exp(-x))
}

#' Computes the log-odds of a number
#'
#' The function is defined by \deqn{\text{ln}\left(\frac{p}{1-p}\right)}{ln( p / ( 1 - p ) )}
#' This function has an inverse, the [logistic()] function.
#'
#' @param p A real number in the range \code{[0, 1]}
#' @return The log-odds of \code{p}, a real number symmetric around 0
#' @export
logit <- function(p) {
  log(p / (1 - p))
}

