#' Computes the logistic of a number
#'
#' The function is defined by \deqn{\frac{1}{1 + \exp(-x)}}{1 / ( 1 + exp(-x) )}
#' @family logistic equations
#' @seealso \code{\link{logit}}
#'
#' @param x A real number, including ±Inf
#' @return The logistic of \code{x}, a real number in the range \code{[0, 1]}
#' @export
logistic <- function(x) {
  1 / (1 + exp(-x))
}

#' Computes the log-odds of a number
#'
#' The function is defined by
#' \deqn{\text{ln}\left(\frac{p}{1-p}\right)}{ln( p / ( 1 - p ) )}
#' @family logistic equations
#' @seealso \code{\link{logistic}}
#'
#' @param p A real number in the range \code{[0, 1]}
#' @return The log-odds of \code{p}, a real number symmetric around 0
#' @export
logit <- function(p) {
  log(p / (1 - p))
}


#' Get the link function from a fitted model
#'
#' @param fit A model fit as returned by \code{\link{bayesPF}}
#' @return The link and inverse link functions
#' @export
getLink <- function(fit) {
  link <- fit[["f2stan"]][["link"]]
  link_functions[[link]]
}
