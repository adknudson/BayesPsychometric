.buildPriorFormula <- function(m_terms) {

  lapply(c(m_terms[["intercepts"]], m_terms[["factor_coefs"]]), function(var) {
    paste0(var, " ~ dnorm(0, 32)")
  })
}
