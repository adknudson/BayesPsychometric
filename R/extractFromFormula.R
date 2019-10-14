.extractFromFormula <- function(formula) {
  fstr  <- as.character(formula)
  fterm <- terms(formula)
  fvars <- attr(fterm, "term.labels")
  fint  <- as.logical(attr(fterm, "intercept"))

  LHS   <- fstr[2]
  RHS   <- fstr[3]

  list(vars = fvars,
       LHS = LHS,
       RHS = RHS,
       include_intercept = fint)
}
