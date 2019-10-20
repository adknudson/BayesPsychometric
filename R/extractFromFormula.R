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

  list(vars = fvars,
       LHS = LHS,
       RHS = RHS,
       include_intercept = fint,
       data_mode = data_mode)
}
