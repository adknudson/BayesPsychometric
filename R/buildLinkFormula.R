# TODO Implement a method that starts with the average slope coefficient for
# each predictor first. I.e. for x1, x2, ... create bx1, bx2, ...
# Then add the factor coefficients bx1_F1[F1], bx1_F2[F2], ...
# bx2_F1[F1], bx2_F2[F2], ...
.buildLinkFormula <- function(fls, data_classes, link) {

  factor_vars <- data_classes[["factor_vars"]]
  numeric_vars <- data_classes[["numeric_vars"]]

  m_link <- paste0(link, "(theta)")

  # Build a string for the slope terms for each predictor
  m_slope <- NULL
  if (length(numeric_vars) != 0) {
    for (nv in numeric_vars) {
      # create the factor coefficients:
      # bvar1_factor1[factor1] + bvar1_factor2[factor2] + ...
      if (length(factor_vars) == 0) {
        factor_coefs <- paste0("b", nv)
      } else {
        factor_coefs <- paste0("b", nv, "_",
                               factor_vars, "[", factor_vars, "]",
                               collapse = " + ")
        factor_coefs <- paste0("b", nv, " + ", factor_coefs)
      }
      factor_coefs <- paste0("(", factor_coefs, ") * ", nv)

      m_slope <- c(m_slope, factor_coefs)
    }
    m_slope <- paste0(m_slope, collapse = " + ")
  }

  # Build up a string of the intercept terms. Also store a list of terms
  if (fls[["include_intercept"]]) {
    if (length(factor_vars) == 0) {
      m_intercept <- "a0"
    } else {
      m_intercept <- paste0(
        "a_", factor_vars, "[", factor_vars, "]", collapse = " + "
      )
      m_intercept <- paste0("a0 + ", m_intercept)
    }
    if (is.null(m_slope)) {
      m_lm <- paste0(m_link, " <- ", m_intercept)
    } else {
      m_lm <- paste0(m_link, " <- ", m_intercept, " + ", m_slope)
    }
  } else {
    m_lm <- paste0(m_link, " <- ", m_slope)
  }

  m_lm

}
