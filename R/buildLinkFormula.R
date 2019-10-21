.buildLinkFormula <- function(fls, data_classes, link) {

  # Assertions ---------------------------------------------------------------
  assertthat::assert_that(link %in% c("logit", "probit"),
                          msg = "'Link' function must be either 'logit' or 'probit'.")

  factor_vars <- data_classes[["factor_vars"]]
  numeric_vars <- data_classes[["numeric_vars"]]

  m_prob <- "theta[i]"
  m_lm <- .buildLinearModel(data_classes, fls[["include_intercept"]])

  # Probit link uses the inverse CDF of the normal distribution, Phi
  if (link == "probit") {
    m_lm <- paste0("Phi(", m_lm, ")")
  }

  paste0(m_prob, " = ", m_lm)
}
