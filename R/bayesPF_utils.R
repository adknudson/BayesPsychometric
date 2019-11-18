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


.extractFactorSamples <- function(fit) {

  fstan   <- fit[["f2stan"]]
  samples <- fit[["samples"]]
  data    <- fstan[["data"]]

  nvs <- fstan[["metadata"]][["vars"]][["numeric"]]
  fvs <- fstan[["metadata"]][["vars"]][["factor"]]
  assertthat::assert_that(
    length(fvs) > 0,
    msg = "There must be factors in the model for this function to work."
  )

  has_intercept <- fstan[["has_intercept"]]
  has_numeric   <- length(nvs) > 0
  has_factor    <- length(fvs) > 0

  # get number of factor levels
  N_levels <- data[paste0("N_", fvs)]
  N_levels_seq <- purrr::map(N_levels, ~seq(1, .x, 1))
  names(N_levels_seq) <- fvs

  # get names of factor levels
  levels_fct <- data[paste0("levels_", fvs)]

  # Expand all combinations
  expand_levels_num <- do.call(expand.grid, N_levels_seq)
  expand_levels_fct <- do.call(expand.grid, levels_fct)
  # expand.grid preserves factors levels.
  # We just want the character representation
  expand_levels_fct <- apply(expand_levels_fct, 2, as.character)

  # Initialize the return list
  factorSamples <- list()

  # Cases
  # We need to retain the names of expand_levels_num which get dropped when the
  # number of factors is 1. Fix this by setting drop=FALSE
  if (has_numeric && has_factor && !has_intercept) {
    # y ~ (bx1 + bx1_k1)*x1 + (bx2 + bx2_k1)*x2 + ...

    for (nv in nvs) {
      tmp <- list()
      for (i in seq_len(nrow(expand_levels_num))) {
        int_list <- as.list(expand_levels_num[i, , drop=FALSE])
        fct_list <- as.list(expand_levels_fct[i, , drop=FALSE])

        names(int_list) <- paste0("b", nv, "_", names(int_list))
        smp <- samples[names(int_list)]
        new_name <- paste0(expand_levels_fct[i,], collapse = " ")
        fct_sum <- Reduce(`+`, purrr::map2(smp, int_list, ~ .x[, .y]))
        tmp[[new_name]] <- fct_sum + samples[[paste0("b", nv)]]
      }
      factorSamples[[nv]] <- tmp
    } # for nv

  } else if (!has_numeric && has_factor) {
    # y ~ a + k1 + k2 + ...
    tmp <- list()
    for (i in seq_len(nrow(expand_levels_num))) {
      int_list <- as.list(expand_levels_num[i, , drop=FALSE])
      fct_list <- as.list(expand_levels_fct[i, , drop=FALSE])

      names(int_list) <- paste0("a_", names(int_list))
      smp <- samples[names(int_list)]
      new_name <- paste0(expand_levels_fct[i,], collapse = " ")
      fct_sum <- Reduce(`+`, purrr::map2(smp, int_list, ~ .x[, .y]))
      tmp[[new_name]] <- fct_sum + samples[["a0"]]
    }
    factorSamples[["a"]] <- tmp

  } else { # (has_numeric && has_factor && has_intercept)
    # y ~ (a + k) + (bx1 + bx1_k1)*x1 + (bx2 + bx2_k1)*x2 + ...
    tmp <- list()
    for (i in seq_len(nrow(expand_levels_num))) {
      int_list <- as.list(expand_levels_num[i, , drop=FALSE])
      fct_list <- as.list(expand_levels_fct[i, , drop=FALSE])

      names(int_list) <- paste0("a_", names(int_list))
      smp <- samples[names(int_list)]
      new_name <- paste0(expand_levels_fct[i,], collapse = " ")
      fct_sum <- Reduce(`+`, purrr::map2(smp, int_list, ~ .x[, .y]))
      tmp[[new_name]] <- fct_sum + samples[["a0"]]
    }
    factorSamples[["a"]] <- tmp

    for (nv in nvs) {
      tmp <- list()
      for (i in seq_len(nrow(expand_levels_num))) {
        int_list <- as.list(expand_levels_num[i, , drop=FALSE])
        fct_list <- as.list(expand_levels_fct[i, , drop=FALSE])

        names(int_list) <- paste0("b", nv, "_", names(int_list))
        smp <- samples[names(int_list)]
        new_name <- paste0(expand_levels_fct[i,], collapse = " ")
        fct_sum <- Reduce(`+`, purrr::map2(smp, int_list, ~ .x[, .y]))
        tmp[[new_name]] <- fct_sum + samples[[paste0("b", nv)]]
      }
      factorSamples[[nv]] <- tmp
    }
  }

  factorSamples
}
