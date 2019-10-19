# TODO Implement a method that starts with the average slope coefficient for
# each predictor first. I.e. for x1, x2, ... create bx1, bx2, ...
# Then add the factor coefficients bx1_F1[F1], bx1_F2[F2], ...
# bx2_F1[F1], bx2_F2[F2], ...
.buildLinkFormula <- function(fls, data_classes, link) {

  factor_vars <- data_classes[["factor_vars"]]
  numeric_vars <- data_classes[["numeric_vars"]]

  m_link <- paste0(link, "(theta)")
  m_lm <- .buildLinearModel(data_classes, fls[["include_intercept"]])

  paste0(m_link, " <- ", m_lm)

}
