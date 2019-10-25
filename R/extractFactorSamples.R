#' Returns coefficient samples for mixed categories
#'
#' Takes in a bayesPF fit object and returns the coefficient estimates for each
#'   combination of factors.
#'
#' @param fit An object returned from `bayesPF()`.
#' @export
#' @importFrom purrr map2
extractFactorSamples <- function(fit) {

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
  if (has_numeric && has_factor && !has_intercept) {
    # y ~ (bx1 + bx1_k1)*x1 + (bx2 + bx2_k1)*x2 + ...

    for (nv in nvs) {
      tmp <- list()
      for (i in seq_len(nrow(expand_levels_num))) {
        int_list <- as.list(expand_levels_num[i, ])
        fct_list <- as.list(expand_levels_fct[i, ])

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
      int_list <- as.list(expand_levels_num[i, ])
      fct_list <- as.list(expand_levels_fct[i, ])

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
      int_list <- as.list(expand_levels_num[i, ])
      fct_list <- as.list(expand_levels_fct[i, ])

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
        int_list <- as.list(expand_levels_num[i, ])
        fct_list <- as.list(expand_levels_fct[i, ])

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
