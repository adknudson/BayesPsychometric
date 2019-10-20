.buildLinkFormula <- function(fls, data_classes, link) {

  factor_vars <- data_classes[["factor_vars"]]
  numeric_vars <- data_classes[["numeric_vars"]]

  m_prob <- "theta[i]"
  m_lm <- .buildLinearModel(data_classes, fls[["include_intercept"]])

  if (link == "probit") {
    inv_link <- "Phi"
    m_lm <- paste0(inv_link, "(", m_lm, ")")
  } else if (link == "logit") {

  } else {
    stop("Link function not recognized. Must be either 'logit' or 'probit'.")
  }

  paste0(m_prob, " = ", m_lm)

}
