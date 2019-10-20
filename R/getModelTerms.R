.getModelTerms <- function(fls, data_classes){

  factor_vars <- data_classes[["factor_vars"]]
  numeric_vars <- data_classes[["numeric_vars"]]

  factor_coefs <- NULL
  for (nv in numeric_vars) {
    if (length(factor_vars) == 0) {
      factor_coefs <- c(factor_coefs, paste0("b", nv))
    } else {
      factor_coefs <- c(
        factor_coefs,
        paste0("b", nv),
        paste0("b", nv, "_", factor_vars)
      )
    }
  }

  intercepts <- NULL
  if (fls[["include_intercept"]]) {
    if (length(factor_vars) == 0) {
      intercepts <- c(intercepts, "a0")
    } else {
      intercepts <- c(intercepts,
                      "a0",
                      paste0("a_", factor_vars))
    }
  }

  list(factor_coefs=factor_coefs,
       intercepts=intercepts)
}
