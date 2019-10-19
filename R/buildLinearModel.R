.buildLinearModel <- function(data_classes, intercept) {

  nv <- data_classes[["numeric_vars"]]
  fv <- data_classes[["factor_vars"]]

  has_numeric <- length(nv) > 0
  has_factor <- length(fv) > 0

  has_intercept <- intercept

  if (!has_numeric && !has_factor && !has_intercept) {
    # y ~ 0
    stop("ERROR: You cannot specify an empty model")
  } else if (has_numeric && !has_factor && !has_intercept ) {
    # y ~ x
    bxi <- paste0("b", nv)
    bxi <- paste0(bxi, " * ", nv, collapse = " + ")
    return(bxi)
  } else if (!has_numeric && has_factor && !has_intercept) {
    # y ~ k
    print("WARNING: Specifying to have no intercept and only factor variables is a contradiction. The intercept will be included and estimates will be aggregated at the end.")
    return(
      paste0("a0 + ",
             paste0("a[", fv, "[i]]", collapse = " + "))
    )
  } else if (!has_numeric && !has_factor && has_intercept) {
    # y ~ 1
    return("a0")
  } else if (has_numeric && has_factor && !has_intercept) {
    # y ~ (b + k)*x
    slope <- paste0(
      sapply(seq_along(nv), function(i) {
        inner <- paste0("b", nv[i], "[", fv, "[i]]", collapse = " + ")
        inner <- paste0("b", nv[i], " + ", inner)
        paste0("(", inner, ") * ", nv[i])
      }),
      collapse = " + ")
    return(slope)
  } else if (has_numeric && !has_factor && has_intercept) {
    # y ~ a + b*x
    bxi <- paste0("b", nv)
    bxi <- paste0(bxi, " * ", nv, collapse = " + ")
    abxi <- paste0("a0", " + ", bxi)
    return(abxi)
  } else if (!has_numeric && has_factor && has_intercept) {
    # y ~ a + k
    return(
      paste0("a0 + ",
             paste0("a[", fv, "[i]]", collapse = " + "))
    )
  } else {
    # y ~ (a + k) + (b + k)*x
    slope <- paste0(
      sapply(seq_along(nv), function(i) {
        inner <- paste0("b", nv[i], "[", fv, "[i]]", collapse = " + ")
        inner <- paste0("b", nv[i], " + ", inner)
        paste0("(", inner, ") * ", nv[i])
      }),
      collapse = " + ")
    a0 <- paste0("a0 + ", paste0("a[", fv, "[i]]", collapse = " + "))
    return(paste0(a0, " + ", slope))
  }
}
