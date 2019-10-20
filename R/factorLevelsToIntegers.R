.factorLevelsToIntegers <- function(formula, data) {
  # Extract features from formula --------------------------------------------
  fls <- .extractFromFormula(formula)
  vars <- fls[["vars"]]
  LHS <- fls[["LHS"]]
  if (grepl(pattern = "\\|", LHS)) {
    data_mode <- "binomial"
    LHS <- trimws(strsplit(LHS, split = "\\|")[[1]])
    fls[["LHS"]] <- LHS
  } else {
    data_mode <- "bernoulli"
  }

  # Get the class of each column in the model --------------------------------
  data_classes <- .getDataClasses(fls, data)

  data[fls[["factor_vars"]]] <- lapply(data[fls[["factor_vars"]]], as.integer)
  data
}
