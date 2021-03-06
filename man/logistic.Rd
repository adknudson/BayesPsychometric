% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{logistic}
\alias{logistic}
\title{Computes the logistic of a number}
\usage{
logistic(x)
}
\arguments{
\item{x}{A real number, including ±Inf}
}
\value{
The logistic of \code{x}, a real number in the range \code{[0, 1]}
}
\description{
The function is defined by \deqn{\frac{1}{1 + \exp(-x)}}{1 / ( 1 + exp(-x) )}
}
\seealso{
\code{\link{logit}}

Other logistic equations: \code{\link{logit}}
}
\concept{logistic equations}
