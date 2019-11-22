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

  metadata <- get_metadata(data, f_ls)

  data <- process_data(data, metadata)

  stan_code <- make_stan(metadata, f_ls, link)

  # Return the model ---------------------------------------------------------
  list(formula = formula,
       link = link,
       has_intercept = f_ls[["has_intercept"]],
       adaptive_pooling = adaptive_pooling,
       metadata = metadata,
       data = data,
       stan_code = stan_code)
}
