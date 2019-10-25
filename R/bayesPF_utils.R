.processSamples <- function(samples, data, metadata, has_intercept) {

  nvs <- metadata[["vars"]][["numeric"]]
  fvs <- metadata[["vars"]][["factor"]]

  has_numeric <- length(nvs) > 0
  has_factor  <- length(fvs) > 0

  if (!has_numeric && !has_factor && !has_intercept) {

    # y ~ 0
    stop("How did you even get to this point!? I'm not mad. I'm just impressed.")

  } else if (has_numeric && !has_factor && !has_intercept ) {

    # y ~ bx1*x1 + bx2*x2 + ...
    for (nv in nvs) {
      # bxi* = bxi / scale(xi)
      s <- attr(data[[nv]], "scaled:scale")
      samples[[paste0("b", nv)]] <- samples[[paste0("b", nv)]] / s
    }
    return(samples)


  } else if (!has_numeric && has_factor && !has_intercept) {

    # y ~ k1 + k2 + ...
    # nothing to do
    return(samples)

  } else if (!has_numeric && !has_factor && has_intercept) {

    # y ~ 1
    # nothing to do
    return(samples)

  } else if (has_numeric && has_factor && !has_intercept) {

    # y ~ (bx1 + bx1_k1)*x1 + (bx2 + bx2_k1)*x2 + ...
    for (nv in nvs) {
      # bxi* = bxi / scale(xi)
      s <- attr(data[[nv]], "scaled:scale")
      samples[[paste0("b", nv)]] <- samples[[paste0("b", nv)]] / s
      for (fv in fvs) {
        # bxi_kj* = bxi_kj / scale(xi)
        bxi_kj <- paste0("b", nv, "_", fv)
        samples[[bxi_kj]] <- samples[[bxi_kj]] / s
      }
    }
    return(samples)

  } else if (has_numeric && !has_factor && has_intercept) {

    # y ~ a + b1*x1 + b2*x2 + ...
    nv_scale  <- unlist(lapply(data[nvs], attr, which = "scaled:scale"))
    nv_center <- unlist(lapply(data[nvs], attr, which = "scaled:center"))

    # Process a0
    bxi       <- paste0("b", nvs)
    bxi_sum <- Reduce(`+`, lapply(1:length(bxi), function(i) {
      samples[[bxi[i]]] * nv_center[i] / nv_scale[i]
    }))
    samples[["a0"]] <- samples[["a0"]] - bxi_sum

    # Process bxi
    for (nv in nvs) {
      # bxi* = bxi / scale(xi)
      s <- attr(data[[nv]], "scaled:scale")
      samples[[paste0("b", nv)]] <- samples[[paste0("b", nv)]] / s
    }
    return(samples)

  } else if (!has_numeric && has_factor && has_intercept) {

    # y ~ a + k1 + k2 + ...
    # nothing to do
    return(samples)

  } else {

    # y ~ (a + k) + (bx1 + bx1_k1)*x1 + (bx2 + bx2_k1)*x2 + ...
    nv_scale  <- unlist(lapply(data[nvs], attr, which = "scaled:scale"))
    nv_center <- unlist(lapply(data[nvs], attr, which = "scaled:center"))

    # Process a0
    bxi       <- paste0("b", nvs)
    bxi_sum <- Reduce(`+`, lapply(1:length(bxi), function(i) {
      samples[[bxi[i]]] * nv_center[i] / nv_scale[i]
    }))
    samples[["a0"]] <- samples[["a0"]] - bxi_sum

    # Process a_kj
    for (fv in fvs) {
      bxi_kj <- paste0("b", nvs, "_", fv)
      bxi_kj_sum <- Reduce(`+`, lapply(1:length(bxi_kj), function(i) {
        samples[[bxi_kj[i]]] * nv_center[i] / nv_scale[i]
      }))
      samples[[paste0("a_", fv)]] <- samples[[paste0("a_", fv)]] - bxi_kj_sum
    }

    # Process bxi
    for (nv in nvs) {
      # bxi* = bxi / scale(xi)
      s <- attr(data[[nv]], "scaled:scale")
      samples[[paste0("b", nv)]] <- samples[[paste0("b", nv)]] / s
    }

    # Process bxi_kj
    for (nv in nvs) {
      for (fv in fvs) {
        bxi_kj <- paste0("b", nv, "_", fv)
        samples[[bxi_kj]] <- samples[[bxi_kj]] / nv_scale[nv]
      }
    }

    return(samples)
  }

}
