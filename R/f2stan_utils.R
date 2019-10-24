##############################################################################
#
# Utility functions for f2stan. These are not meant to be called directly by
# the user, so are not exported in the namespace. For ease of mainentance,
# these functions are listed in the order that they get called within the main
# function. A short description is provided for each function.
#
##############################################################################

#============================================================================= .extractFromFormula
# Takes in a formula such as
#
#     y|k ~ x1 + x2 + factor1 + factor 2
#
# and extracts helpful information like terms, response, intercept, etc.
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


#============================================================================= .getDataClasses
# Which variables are numeric? factor? character? predictor? response? etc.
.getDataClasses <- function(fls, data) {

  # Get the class of each column ---------------------------------------------
  # Ordered factors have two classes (ordered, factor) so get just the first
  data_classes <- unlist(lapply(data, function(col) class(col)[1]))
  data_names <- names(data)

  # Split up the classes by part of formula ----------------------------------
  response <- data_classes[fls[["LHS"]]]
  predictors <- data_classes[fls[["vars"]]]

  factor_vars <- subset(fls[["vars"]],
                        data_classes[fls[["vars"]]] %in% c("factor", "ordered"))
  numeric_vars <- subset(fls[["vars"]],
                         data_classes[fls[["vars"]]] %in% c("numeric", "integer"))
  character_vars <- subset(fls[["vars"]],
                           data_classes[fls[["vars"]]] == "character")

  # Assertions ---------------------------------------------------------------
  assertthat::assert_that(assertthat::has_name(data, c(fls[["LHS"]], fls[["vars"]])),
                          msg = "One or more of the variables in the formula is not a variable in the data.")
  assertthat::assert_that(all(response == "integer"),
                          msg = paste("The response variable(s) must be integers.", paste(names(response), "is type", response, collapse = "; ")))
  assertthat::assert_that(all(data[fls[["LHS"]]] >= 0),
                          msg = "The response variables must all be non-negative integers.")
  assertthat::assert_that(!("character" %in% predictors),
                          msg = "The predictor variables must not be of type 'character'. Please make sure that they are either of type 'integer', 'numeric', or 'factor'.")
  if (fls[["data_mode"]] == "binomial") {
    successes <- data[[fls[["LHS"]][1]]]
    size <- data[[fls[["LHS"]][2]]]
    assertthat::assert_that(all(successes <= size),
                            msg = "In a binomial distribution, the number of successes must be less than or equal to the size of the draw. Please validate your response variables.")
  }
  # I can probably implicitly handle this case internally, but my gut says
  # that is better to complain to the user so that they are aware of the
  # implications. I.e. the internal factor level may not match the external
  # data.
  if (length(factor_vars) > 0) {
    for (fv in factor_vars) {
      assertthat::assert_that(
        length(levels(data[[fv]])) == length(unique(data[[fv]])),
        msg = paste("The number of levels in", fv, "do not match the number of unique values. This could be because 1 or more levels are excluded from the data. Please refactor and try again."))
    }
  }

  list(response = response,
       predictors = predictors,
       numeric_vars = numeric_vars,
       factor_vars = factor_vars,
       character_vars = character_vars)
}


#============================================================================= .preProcessData
# Scale any numeric predictors so that default priors are on a more common
# scale with the estimated coefficients.
.preProcessData <- function(fls, data) {

  # We call data_classes in here because standardizing will change integer
  # predictors to numeric, and we want that change to be reflected later on
  data_classes <- .getDataClasses(fls, data)
  has_intercept <- fls[["include_intercept"]]

  # if there are numeric variables, standardize them
  if (length(data_classes[["numeric_vars"]]) > 0) {
    for (nv in data_classes[["numeric_vars"]]) {
      # Drop needs to be used to return a vector while keeping the attributes
      if (has_intercept) {
        data[[nv]] <- drop(scale(data[[nv]]))
      } else {
        # Centering artificially introduces an intercept. We don't want to do
        # that if no intercept is specified.
        data[[nv]] <- drop(scale(data[[nv]], center = FALSE))
      }
    }
  }

  data
}


#============================================================================= .buildDistributionFormula
# Four cases determined by a mix of bernoulli/binomial, logit/probit
.buildDistributionFormula <- function(fls, link) {

  data_mode <- fls[["data_mode"]]
  LHS <- fls[["LHS"]]

  assertthat::assert_that(link %in% c("logit", "probit"),
                          msg = "'Link' function must be either 'logit' or 'probit'.")
  assertthat::assert_that(data_mode %in% c("bernoulli", "binomial"),
                          msg = "Data must be either in bernoulli form (0's and 1's) or binomial form (k successes in n trials).")

  if (data_mode == "bernoulli" && link == "logit") {

    # Bernoulli logit --------------------------------------------------------
    m_dist <- paste0(LHS, " ~ bernoulli_logit(theta)")

  } else if (data_mode == "bernoulli" && link == "probit") {

    # Bernoulli probit -------------------------------------------------------
    m_dist <- paste0(LHS, " ~ bernoulli(theta)")

  } else if (data_mode == "binomial" && link == "logit") {

    # Binomial logit ---------------------------------------------------------
    successes <- LHS[1]
    trials    <- LHS[2]
    m_dist <- paste0(successes, " ~ binomial_logit(", trials,", theta)")

  } else if (data_mode == "binomial" && link == "probit") {

    # Binomial probit --------------------------------------------------------
    successes <- LHS[1]
    trials    <- LHS[2]
    m_dist <- paste0(successes, " ~ binomial(", trials, ", theta)")

  } else {
    stop("The model is not 'bernoulli' or 'binomial', and the link is not 'logit' or 'probit'.")
  }

  m_dist
}


#============================================================================= .buildLinearModel
# There are 8 cases a user can specify based on if a model has numeric
# variables, factor variables, and an intercept.
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
    warning("Specifying a model with no intercept but also specifying only factor variables is a contradiction. The intercept will be included and estimates will be aggregated at the end.")
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


#============================================================================= .buildLinkFormula
# Bernoulli and Binomial logit have their own functions. Pribt models need to
# apply the Normal Inverse CDF (Phi) to the linear model
.buildLinkFormula <- function(fls, data_classes, link) {

  # Assertions ---------------------------------------------------------------
  assertthat::assert_that(link %in% c("logit", "probit"),
                          msg = "'Link' function must be either 'logit' or 'probit'.")

  factor_vars <- data_classes[["factor_vars"]]
  numeric_vars <- data_classes[["numeric_vars"]]

  m_prob <- "theta[i]"
  m_lm <- .buildLinearModel(data_classes, fls[["include_intercept"]])

  # Probit link uses the inverse CDF of the normal distribution, Phi
  if (link == "probit") {
    m_lm <- paste0("Phi(", m_lm, ")")
  }

  paste0(m_prob, " = ", m_lm)
}


#============================================================================= .getModelTerms
# TODO: Rewrtie this so that it's more in line with .buildLinearModel()
.getModelTerms <- function(fls, data_classes){

  factor_vars <- data_classes[["factor_vars"]]
  numeric_vars <- data_classes[["numeric_vars"]]

  factor_coefs <- NULL
  for (nv in numeric_vars) {
    if (length(factor_vars) == 0) {
      factor_coefs <- c(factor_coefs, paste0("b", nv))
    } else {
      factor_coefs <- c(
        factor_coefs,
        paste0("b", nv),
        paste0("b", nv, "_", factor_vars)
      )
    }
  }

  intercepts <- NULL
  if (fls[["include_intercept"]]) {
    if (length(factor_vars) == 0) {
      intercepts <- c(intercepts, "a0")
    } else {
      intercepts <- c(intercepts,
                      "a0",
                      paste0("a_", factor_vars))
    }
  }

  list(factor_coefs = factor_coefs,
       intercepts = intercepts)
}


#============================================================================= .buildPriorFormula
# TODO: extend this function to handle partial and complete pooling modes
.buildPriorFormula <- function(m_terms) {

  lapply(c(m_terms[["intercepts"]], m_terms[["factor_coefs"]]), function(var) {
    paste0(var, " ~ normal(0, 10)")
  })

}


#============================================================================= .processData
# Change factor variables to integer values. Convert data frame to list
.processData <- function(data, fls, data_classes) {

  assertthat::assert_that(!("N" %in% names(data)),
                          msg = "'N' is a reserved variable name in bayesPF. Please rename this column to something else.")

  # Data needs to be a list
  data <- as.list(as.data.frame(data))
  # Get total number of observations
  data[["N"]] <- length(data[[fls[["LHS"]][1]]])
  # Convert factors to integers if any factors are specified
  if (length(data_classes[["factor_vars"]]) > 0) {
    data[data_classes[["factor_vars"]]] <- lapply(
      data[data_classes[["factor_vars"]]],
      as.integer)
    # Get the number of levels of each factor
    for (fv in data_classes[["factor_vars"]]) {
      data[[paste0("N_", fv)]] <- length(unique(data[[fv]]))
    }
  }
  data
}


#============================================================================= .buildStanCode
# Workhorse function
# Creates the Stan code in 3 parts: Data, Parameters, and Model
.buildStanCode <- function(data, model, fls, data_classes) {

  concat <- function(...) {
    paste(..., collapse = "", sep = "")
  }

  indent <- "    "  # indent is four spaces

  body_data <- concat(
    indent, "// Number of observations, factor levels, etc.\n"
  )
  body_parameters <- ""
  body_model      <- ""

  # Build data block ---------------------------------------------------------

  # All models should have data => set number of observations to N
  body_data <- concat(
    body_data,
    indent, "int<lower=1> N;\n"
  )

  # Declare number of factor levels if any factors are in the model
  if (length(data_classes[["factor_vars"]]) > 0) {
    for (fv in data_classes[["factor_vars"]]) {
      body_data <- concat(
        body_data,
        indent, "int<lower=1> N_", fv, ";\n"
      )
    }
  }

  # Response data (bernoulli or binomial)
  body_data <- concat(body_data, "\n", indent, "// Response Data\n")
  if (fls[["data_mode"]] == "bernoulli") {
    body_data <- concat(body_data, indent, "int ", fls[["LHS"]], "[N];\n")
  } else if (fls[["data_mode"]] == "binomial") {
    body_data <- concat(body_data, indent, "int ", fls[["LHS"]][1], "[N];\n")
    body_data <- concat(body_data, indent, "int ", fls[["LHS"]][2], "[N];\n")
  } else {
    stop("Data responses must be bernoulli or binomial")
  }

  # Numeric variables
  if (length(data_classes[["numeric_vars"]]) > 0) {
    body_data <- concat(body_data, "\n", indent, "// Numeric Data\n")
    for(nv in data_classes[["numeric_vars"]]) {
      # Get the specific type of the numeric variable (numeric or integer)
      if (data_classes[["predictors"]][nv] == "integer") {
        nv_type <- "int"
      } else if (data_classes[["predictors"]][nv] == "numeric") {
        nv_type <- "real"
      } else {
        stop("Predictor variable must be of type 'numeric' or 'integer'.")
      }
      body_data <- concat(body_data, indent, nv_type, " ", nv, "[N];\n")
    }
  }

  # Factor variables
  if (length(data_classes[["factor_vars"]]) > 0) {
    body_data <- concat(body_data, "\n", indent, "// Factor Data\n")
    for(fv in data_classes[["factor_vars"]]) {
      body_data <- concat(body_data, indent, "int ", fv, "[N];\n")
    }
  }

  # Build parameters block ---------------------------------------------------

  # Intercept terms
  if (fls[["include_intercept"]]) {
    body_parameters <- concat(body_parameters, indent, "// Intercept terms\n")
    body_parameters <- concat(body_parameters, indent, "real a0;\n")
    # Add vectors for factors if any are in the model
    if (length(data_classes[["factor_vars"]]) > 0) {
      for (fv in data_classes[["factor_vars"]]) {
        body_parameters <- concat(
          body_parameters,
          indent, "vector[N_", fv, "] a_", fv, ";\n"
        )
      } # for fv
    } # if has factors
  } # if has intercept

  # Slope terms
  if (length(data_classes[["numeric_vars"]]) > 0) {
    body_parameters <- concat(
      body_parameters, "\n",
      indent, "// Slope terms\n"
    )
    for (nv in data_classes[["numeric_vars"]]) {
      body_parameters <- concat(
        body_parameters,
        indent, "real b", nv, ";\n"
      )
      # Factor slope terms
      if (length(data_classes[["factor_vars"]]) > 0) {
        for (fv in data_classes[["factor_vars"]]) {
          body_parameters <- concat(
            body_parameters,
            indent, "vector[N_", fv, "] b", nv, "_", fv, ";\n"
          )
        } # for fv
      } # if fv
    } # for nv
  } # if nv

  # Build model block --------------------------------------------------------
  body_model <- concat(
    body_model,
    indent, "vector[N] theta;\n\n",
    indent, "// Priors\n",
    concat(indent, model[["priors"]], ";\n"), "\n",
    indent, "// General linear model\n",
    indent, "for (i in 1:N) {\n",
    indent, indent, model[["linear_model"]], ";\n",
    indent, "}\n",
    indent, model[["mode"]], ";\n"
  )

  # Return the entire model --------------------------------------------------
  concat(
    "data{\n",
      body_data,
    "}\n",

    "parameters{\n",
      body_parameters,
    "}\n",

    "model{\n",
      body_model,
    "}\n"
  )
}
