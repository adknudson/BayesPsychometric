if (!has_numeric && !has_factor && !has_intercept) {
  # y ~ 0
  stop("You must specify a non-null model to fit.")

} else if (has_numeric && !has_factor && !has_intercept ) {
  # y ~ b1*x1 + b2*x2 + ...

} else if (!has_numeric && has_factor && !has_intercept) {
  # y ~ k1 + k2 + ...

} else if (!has_numeric && !has_factor && has_intercept) {
  # y ~ 1

} else if (has_numeric && has_factor && !has_intercept) {
  # y ~ (bx1 + bx1_k1)*x1 + (bx2 + bx2_k1)*x2 + ...

} else if (has_numeric && !has_factor && has_intercept) {
  # y ~ a + b1*x1 + b2*x2 + ...

} else if (!has_numeric && has_factor && has_intercept) {
  # y ~ a + k1 + k2 + ...

} else {
  # y ~ (a + k) + (bx1 + bx1_k1)*x1 + (bx2 + bx2_k1)*x2 + ...

}
