.buildLinearModel <- function(data_classes, has_intercept) {

  nv <- data_classes[["numeric_vars"]]
  fv <- data_classes[["factor_vars"]]

  has_numeric <- length(nv) > 0
  has_factor <- length(fv) > 0

  if (!has_numeric && !has_factor && !has_intercept) {

    # y ~ 0
    stop("ERROR: You cannot specify an empty model")

  } else if (has_numeric && !has_factor && !has_intercept ) {

    # y ~ b1*x1 + b2*x2 + ...
    # creates: bx1, bx2, ...
    m <- paste0("b", nv)
    # creates: bx1*x1 + bx2*x2 + ...
    m <- paste0(m, " * ", nv, "[i]", collapse = " + ")
    return(m)

  } else if (!has_numeric && has_factor && !has_intercept) {

    # y ~ k1 + k2 + ...
    cat("WARNING: Specifying a model with no intercept but also specifying only factor variables is a contradiction. The intercept will be included and estimates will be aggregated at the end.\n")
    m <- paste0("a0 + ", paste0("a_", fv, "[", fv, "[i]]", collapse = " + "))
    return(m)

  } else if (!has_numeric && !has_factor && has_intercept) {

    # y ~ 1
    return("a0")

  } else if (has_numeric && has_factor && !has_intercept) {

    # y ~ (bx1 + bx1_k1)*x1 + (bx2 + bx2_k1)*x2 + ...
    # Builds inner slope (bx1 + bx1_k1 + ...) then the outer product
    m <- paste0(
      sapply(seq_along(nv), function(i) {
        # creates: bxi_k1 + bxi_k2 + ...
        inner <- paste0("b", nv[i], "_", fv, "[", fv, "[i]]", collapse = " + ")
        # creates: bxi + bxi_k1 + bxi_k2 + ...
        inner <- paste0("b", nv[i], " + ", inner)
        # creates: (bxi + bxi_k1 + bxi_k2 + ...)*xi
        paste0("(", inner, ") * ", nv[i], "[i]")
      }),
      collapse = " + ")
    return(m)

  } else if (has_numeric && !has_factor && has_intercept) {

    # y ~ a + b1*x1 + b2*x2 + ...
    # creates: bx1, bx2, ...
    m <- paste0("b", nv)
    # creates: bx1*x1 + bx2*x2 + ...
    m <- paste0(m, " * ", nv, "[i]", collapse = " + ")
    m <- paste0("a0", " + ", m)
    return(m)

  } else if (!has_numeric && has_factor && has_intercept) {

    # y ~ a + k1 + k2 + ...
    m <- paste0("a0 + ", paste0("a_", fv, "[", fv, "[i]]", collapse = " + "))
    return(m)

  } else {

    # y ~ (a + k) + (bx1 + bx1_k1)*x1 + (bx2 + bx2_k1)*x2 + ...
    m <- paste0(
      sapply(seq_along(nv), function(i) {
        inner <- paste0("b", nv[i], "_", fv, "[", fv, "[i]]", collapse = " + ")
        inner <- paste0("b", nv[i], " + ", inner)
        paste0("(", inner, ") * ", nv[i], "[i]")
      }),
      collapse = " + ")
    a0 <- paste0("a0 + ", paste0("a_", fv, "[", fv, "[i]]", collapse = " + "))
    return(paste0(a0, " + ", m))
  }

}
