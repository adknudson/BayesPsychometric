buildLink <- function(data_classes, intercept) {
  has_numeric <- length(data_classes[["x"]]) > 0
  has_factor <- length(data_classes[["k"]]) > 0
  has_intercept <- as.logical(intercept)
  # CASE 0: NULL Model
  if (!has_numeric && !has_factor && !has_intercept) {
    print("ERROR: You cannot specify an empty model")
  } else if (has_numeric && !has_factor && !has_intercept ) {
    bxi <- paste0("b", data_classes[["x"]])
    bxi <- paste0(bxi, " * ", data_classes[["x"]], collapse = " + ")
    return(bxi)
  } else if (!has_numeric && has_factor && !has_intercept) {
    print("WARNING: Specifying to have no intercept and only factor variables is a contradiction. The intercept will be included and estimates will be aggregated at the end.")
    return(
      paste0("a0 + ",
             paste0("a[", data_classes[["k"]], "[i]]", collapse = " + "))
    )
  } else if (!has_numeric && !has_factor && has_intercept) {
    return("a0")
  } else if (has_numeric && has_factor && !has_intercept ) {
    slope <- paste0(
      sapply(seq_along(data_classes[["x"]]), function(i) {
        inner <- paste0("b", data_classes[["x"]][i], "[", data_classes[["k"]], "[i]]", collapse = " + ")
        inner <- paste0("b", data_classes[["x"]][i], " + ", inner)
        paste0("(", inner, ") * ", data_classes[["x"]][i])
      }),
      collapse = " + ")
    return(slope)
  } else if (has_numeric && !has_factor && has_intercept) {
    bxi <- paste0("b", data_classes[["x"]])
    bxi <- paste0(bxi, " * ", data_classes[["x"]], collapse = " + ")
    abxi <- paste0("a0", " + ", bxi)
    return(abxi)
  } else if (!has_numeric && has_factor && has_intercept) {
    return(
      paste0("a0 + ",
             paste0("a[", data_classes[["k"]], "[i]]", collapse = " + "))
    )
  } else {
    slope <- paste0(
      sapply(seq_along(data_classes[["x"]]), function(i) {
        inner <- paste0("b", data_classes[["x"]][i], "[", data_classes[["k"]], "[i]]", collapse = " + ")
        inner <- paste0("b", data_classes[["x"]][i], " + ", inner)
        paste0("(", inner, ") * ", data_classes[["x"]][i])
      }),
      collapse = " + ")
    a0 <- paste0("a0 + ", paste0("a[", data_classes[["k"]], "[i]]", collapse = " + "))
    return(paste0(a0, " + ", slope))
  }
}


# Null
dc <- list(x = character(0),  k = character(0))
buildLink(dc, 0)
buildLink(dc, 1)

# 1 Numeric
dc_x <- list(x = "x1", k = character(0))
buildLink(dc_x, 0)
buildLink(dc_x, 1)

# 1 Factor
dc_k <- list(x = character(0), k = "k1")
buildLink(dc_k, 0)
buildLink(dc_k, 1)

# Multiple numeric
dc_mx <- list(x = c("x1", "x2"), k = character(0))
buildLink(dc_mx, 0)
buildLink(dc_mx, 1)

# Multiple factors
dc_mk <- list(x = character(0), k = c("k1", "k2", "k3"))
buildLink(dc_mk, 0)
buildLink(dc_mk, 1)

# 1 Numeric, Multiple Factors
dc_xmk <- list(x = "x1", k = c("k1", "k2", "k3"))
buildLink(dc_xmk, 0)
buildLink(dc_xmk, 1)

# Multiple Numeric, 1 Factor
dc_mxk <- list(x = c("x1", "x2"), k = "k1")
buildLink(dc_mxk, 0)
buildLink(dc_mxk, 1)

# Multiple Numeric, Multiple Factors
dc_mxmk <- list(x = c("x1", "x2"), k = c("k1", "k2", "k3"))
buildLink(dc_mxmk, 0)
buildLink(dc_mxmk, 1)

dc_big <- list(x = paste0("x", 1:5), k = paste0("k", 1:10))
cat(buildLink(dc_big, 0))
cat(buildLink(dc_big, 1))
