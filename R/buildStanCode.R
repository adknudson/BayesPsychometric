.buildStanCode <- function(data, model, fls, data_classes) {

  concat <- function(...) {
    paste(..., collapse = "", sep = "")
  }

  indent <- "    "  # indent is four spaces

  body_data       <- concat(
    indent,
    "// Number of observations, factor levels, etc.\n"
  )
  body_parameters <- ""
  body_model      <- ""

  # Build data block ---------------------------------------------------------
  # All models should have data
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
    # Add factor vectors if they are in the model
    if (length(data_classes[["factor_vars"]]) > 0) {
      for (fv in data_classes[["factor_vars"]]) {
        body_parameters <- concat(
          body_parameters,
          indent, "vector[N_", fv, "] a_", fv, ";\n"
        )
      }
    }
  }
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
    indent, "vector[N] theta;\n",
    indent, "real default_sd = inv_sqrt(0.001);  // ~32\n\n",
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
