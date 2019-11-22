#' Convert a typical formula to a formula list that map2stan can understand
#'
#' @param formula A formula that you would pass to `glm` or similar.
#' @param data A data frame of observations.
#' @param link A link function (either "logit" or "probit")
#' @param adaptive_pooling Logical (FALSE by default) Specifies whether
#'   adaptive pooling should be used when fitting the model
#' @return A model and data that can be used by `rstan::stan` as well as
#'   information about the model and the data.
#' @export
f2stan <- function(formula,
                   data,
                   link = c("logit", "probit"),
                   adaptive_pooling = FALSE) {

  f_ls <- process_formula(formula)
  f_ls[["adaptive_pooling"]] <- adaptive_pooling
  has_intercept <- f_ls[["has_intercept"]]

  # Cannot specify a model with adaptive pooling and without an intercept
  # Truth table. P = adaptive pooling, Q = has intercept
  # P  Q  Output || !P  Q  !P||Q
  # T  T  T      || F   T  T
  # T  F  F      || F   F  F
  # F  T  T      || T   T  T
  # F  F  T      || T   F  T
  assertthat::assert_that(
    !adaptive_pooling || has_intercept,
    msg = paste("If adaptive pooling is specified,",
                "then you must specify a model with an intercept and factors.")
  )

  metadata <- get_metadata(data, f_ls)
  has_factor <- length(metadata[["vars"]][["factor"]]) > 0

  # Cannot specify a model with adaptive pooling and without factor variables. See the
  # truth table above because it is the same for factors.
  assertthat::assert_that(
    !adaptive_pooling || has_factor,
    msg = paste("If adaptive pooling is specified,",
                "then you must specify a model with an intercept and factors.")
  )

  data <- process_data(data, metadata)

  stan_code <- make_stan(metadata, f_ls, link)

  # Return the model ---------------------------------------------------------
  list(formula = formula,
       link = link,
       has_intercept = has_intercept,
       adaptive_pooling = adaptive_pooling,
       metadata = metadata,
       data = data,
       stan_code = stan_code)
}
