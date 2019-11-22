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


#' Calculate the posterior samples for the PSS
#'
#' @return The posterior samples for the Point of Subjective Simultaneity
extract_PSS_samples <- function(fit) {
  metadata <- fit[["f2stan"]][["metadata"]]
  has_intercept <- fit[["f2stan"]][["has_intercept"]]
  nvs <- metadata[["vars"]][["numeric"]]

  factorSamples <- fit[["factorSamples"]]

  # If there is no intercept, then return 0
  if (!has_intercept) {
    warning("Any model without an intercept will have a fixed PSS at F(0.5), where 'F' is the link function specified in the model.")

    f <- getLink(fit)

    return(f[["link"]](0.5))
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
  if (!is.null(factorSamples)) {
    pss_dist <- purrr::map2(factorSamples[[1]],
                            factorSamples[[2]],
                            ~ -.x / .y)
  } else {
    intercept_coef <- metadata[["coefs"]][[1]][1]
    slope_coef     <- metadata[["coefs"]][[2]][1]

    intercept <- fit[["samples"]][[intercept_coef]]
    slope     <- fit[["samples"]][[slope_coef]]

    pss_dist <- -intercept / slope
  }
  pss_dist
}
