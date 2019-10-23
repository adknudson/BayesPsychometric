.extractFromFormula <- function(formula) {
  fstr  <- as.character(formula)
  fterm <- terms(formula)
  fvars <- attr(fterm, "term.labels")
  fint  <- as.logical(attr(fterm, "intercept"))

  LHS   <- fstr[2]
  RHS   <- fstr[3]

  if (grepl(pattern = "\\|", LHS)) {
    data_mode <- "binomial"
    LHS <- trimws(strsplit(LHS, split = "\\|")[[1]])
  } else {
    data_mode <- "bernoulli"
  }

  # Assertions
  assertthat::assert_that(length(LHS) %in% c(1,2),
                          msg = paste("The number of arguments on the left hand side of the equation must be 1 (for bernoulli data) or 2 (for binomial data). The number of args on the LHS is", length(LHS)))
  assertthat::assert_that(all(attr(fterm, "order") == 1),
                          msg = "The order of all variables must be 1 (linear).")
  # Maybe disallow users from specifying a model with no intercept?
  if (!fint) {
    warning("Specifying a model with no intercept implicitly constrains the y-intercept to be 0.5 (0 on the log-odds scale).")
  }

  list(vars = fvars,
       LHS = LHS,
       RHS = RHS,
       include_intercept = fint,
       data_mode = data_mode)
}
