# Process the formula ========================================================
process_formula <- function(f) {
  f_str  <- as.character(f)
  f_terms <- terms(f)
  f_vars <- attr(f_terms, "term.labels")
  f_int  <- as.logical(attr(f_terms, "intercept"))

  LHS <- f_str[2]
  RHS <- f_str[3]

  if (grepl(pattern = "\\|", LHS)) {
    data_mode <- "binomial"
    LHS <- trimws(strsplit(LHS, split = "\\|")[[1]])
  } else {
    data_mode <- "bernoulli"
  }

  list(vars = f_vars,
       LHS  = LHS,
       RHS  = RHS,
       has_intercept = f_int,
       data_mode = data_mode)
}


# Get the metadata ===========================================================
get_metadata <- function(data, f_ls) {

  classes <- purrr::map(data, ~class(.x)[1])

  LHS <- f_ls[["LHS"]]
  f_vars <- f_ls[["vars"]]
  has_intercept <- f_ls[["has_intercept"]]

  response_vars <- LHS
  numeric_vars <- subset(f_vars, classes[f_vars] %in% c("numeric", "integer"))
  factor_vars <- subset(f_vars, classes[f_vars] %in% c("factor", "ordered"))
  character_vars <- subset(f_vars, classes[f_vars] == "character")

  response_class <- classes[response_vars]
  numeric_class <- classes[numeric_vars]
  factor_class <- classes[factor_vars]

  nvs <- numeric_vars
  fvs <- factor_vars
  cvs <- character_vars

  has_numeric <- length(nvs) > 0
  has_factor <- length(fvs) > 0
  has_character <- length(cvs) > 0

  assertthat::assert_that(
    !has_character,
    msg = paste("The model must not contain character variables.",
                "The offending",
                ifelse(length(cvs) == 1, "variable is:", "variables are:"),
                cvs, ". Did you mean for these to be factor variables instead?")
  )

  # Get list of internal coefficients
  ret_list <- list()
  if (!has_numeric && !has_factor && !has_intercept) {
    # y ~ 0
    stop("You must specify a non-null model to fit.")

  } else if (has_numeric && !has_factor && !has_intercept ) {
    # y ~ b1*x1 + b2*x2 + ...
    ret_list[[length(ret_list) + 1]] <- paste0("b_", nvs)

  } else if (!has_numeric && has_factor && !has_intercept) {
    # y ~ k1 + k2 + ...
    ret_list[[length(ret_list) + 1]] <- paste0("a_", fvs)

  } else if (!has_numeric && !has_factor && has_intercept) {
    # y ~ 1
    ret_list[[length(ret_list) + 1]] <- "a"

  } else if (has_numeric && has_factor && !has_intercept) {
    # y ~ (bx1 + bx1_k1)*x1 + (bx2 + bx2_k1)*x2 + ...
    for (nv in nvs) {
      ret_list[[length(ret_list) + 1]] <- c(paste0("b_", nv), paste0("b_", nv, "_", fvs))
    }

  } else if (has_numeric && !has_factor && has_intercept) {
    # y ~ a + b1*x1 + b2*x2 + ...
    ret_list[[length(ret_list) + 1]] <- "a"
    ret_list[[length(ret_list) + 1]] <- paste0("b_", nvs)

  } else if (!has_numeric && has_factor && has_intercept) {
    # y ~ a + k1 + k2 + ...
    ret_list[[length(ret_list) + 1]] <- c("a", paste0("a_", fvs))

  } else {
    # y ~ (a + k) + (bx1 + bx1_k1)*x1 + (bx2 + bx2_k1)*x2 + ...
    ret_list[[length(ret_list) + 1]] <- c("a", paste0("a_", fvs))
    for (nv in nvs) {
      ret_list[[length(ret_list) + 1]] <- c(paste0("b_", nv), paste0("b_", nv, "_", fvs))
    }
  }

  list(vars  = list(response  = response_vars,
                    numeric   = numeric_vars,
                    factor    = factor_vars,
                    character = character_vars),
       class = list(response = response_class,
                    numeric  = numeric_class,
                    factor   = factor_class),
       coefs = ret_list)
}


# Process the data ===========================================================
# Turn factors into integers, get number of levels, etc.
process_data <- function(data, metadata) {

  fvs <- metadata[["vars"]][["factor"]]

  N <- nrow(data)
  data <- as.list(data)
  data[["N"]] <- N

  if (length(fvs) > 0) {
    data[paste0("levels_", fvs)] <- lapply(data[fvs], levels)
    data[fvs] <- lapply(data[fvs], as.integer)
    # Get the number of levels of each factor
    for (fv in fvs) {
      data[[paste0("N_", fv)]] <- length(unique(data[[fv]]))
    }
  }

  data
}


# Build the Stan code ========================================================
make_stan <- function(metadata, f_ls, link) {
  concat <- function(...) {
    paste(..., collapse = "", sep = "")
  }

  # Set up variables
  LHS <- f_ls[["LHS"]]
  nvs <- metadata[["vars"]][["numeric"]]
  fvs <- metadata[["vars"]][["factor"]]
  has_intercept <- f_ls[["has_intercept"]]
  has_numeric <- length(nvs) > 0
  has_factor <- length(fvs) > 0
  adaptive_pooling <- f_ls[["adaptive_pooling"]]
  data_mode <- f_ls[["data_mode"]]
  m_coefs <- metadata[["coefs"]]

  indent <- "    " # four spaces

  # Create distribution formula ----------------------------------------------
  m_dist <- switch(
    paste(data_mode, link),
    "bernoulli logit"  = paste0(LHS, " ~ bernoulli_logit(theta)"),
    "bernoulli probit" = paste0(LHS, " ~ bernoulli(theta)"),
    "binomial logit"   = paste0(LHS[1], " ~ binomial_logit(", LHS[2],", theta)"),
    "binomial probit"  = paste0(LHS[1], " ~ binomial(", LHS[2],", theta)"),
    stop("You must specify a valid model: (bernoulli/binomial), (logit/probit)")
  )

  # Create linear model ------------------------------------------------------
  if (!has_numeric && !has_factor && !has_intercept) {
    # y ~ 0
    stop("You must specify a non-null model to fit.")

  } else if (has_numeric && !has_factor && !has_intercept ) {
    # y ~ b1*x1 + b2*x2 + ...
    # creates: bx1, bx2, ...
    m <- paste0("b_", nvs)
    # creates: bx1*x1 + bx2*x2 + ...
    m <- paste0(m, " * ", nvs, "_std[i]", collapse = " + ")

  } else if (!has_numeric && has_factor && !has_intercept) {
    # y ~ k1 + k2 + ...
    m <- paste0("a_", fvs, "[", fvs, "[i]]", collapse = " + ")

  } else if (!has_numeric && !has_factor && has_intercept) {
    # y ~ 1
    m <- "a"

  } else if (has_numeric && has_factor && !has_intercept) {
    # y ~ (bx1 + bx1_k1)*x1 + (bx2 + bx2_k1)*x2 + ...
    # Builds inner slope (bx1 + bx1_k1 + ...) then the outer product
    m <- paste0(
      sapply(seq_along(nvs), function(i) {
        # creates: bxi_k1 + bxi_k2 + ...
        inner <- paste0("b_", nvs[i], "_", fvs, "[", fvs, "[i]]", collapse = " + ")
        # creates: bxi + bxi_k1 + bxi_k2 + ...
        inner <- paste0("b_", nvs[i], " + ", inner)
        # creates: (bxi + bxi_k1 + bxi_k2 + ...)*xi
        paste0("(", inner, ") * ", nvs[i], "_std[i]")
      }),
      collapse = " + ")

  } else if (has_numeric && !has_factor && has_intercept) {
    # y ~ a + b1*x1 + b2*x2 + ...
    # creates: bx1, bx2, ...
    m <- paste0("b_", nvs)
    # creates: bx1*x1 + bx2*x2 + ...
    m <- paste0(m, " * ", nvs, "_std[i]", collapse = " + ")
    m <- paste0("a", " + ", m)

  } else if (!has_numeric && has_factor && has_intercept) {
    # y ~ a + k1 + k2 + ...
    m <- paste0("a + ", paste0("a_", fvs, "[", fvs, "[i]]", collapse = " + "))

  } else {
    # y ~ (a + k) + (bx1 + bx1_k1)*x1 + (bx2 + bx2_k1)*x2 + ...
    m <- paste0(
      sapply(seq_along(nvs), function(i) {
        inner <- paste0("b_", nvs[i], "_", fvs, "[", fvs, "[i]]", collapse = " + ")
        inner <- paste0("b_", nvs[i], " + ", inner)
        paste0("(", inner, ") * ", nvs[i], "_std[i]")
      }),
      collapse = " + ")
    a <- paste0("a + ", paste0("a_", fvs, "[", fvs, "[i]]", collapse = " + "))
    m <- paste0(a, " + ", m)
  }

  # Create link formula ------------------------------------------------------
  f_link <- switch(
    link,
    probit = paste0("theta[i] = Phi(", m, ")"),
    logit  = paste0("theta[i] = ", m),
    stop("'Link' function must be either 'logit' or 'probit'.")
  )

  # Create priors ------------------------------------------------------------
  if(adaptive_pooling) {
    ret_list <- list()
    for (i in m_coefs) {
      # Prior for the shared coefficient
      tmp <- paste0(i[1], " ~ normal(0, 5)")
      # Prior for the factor coefficients
      tmp <- c(tmp, purrr::map(i[-1], ~ paste0(.x, " ~ normal(0, sd_", .x, ")")))
      # Prior for the shared standard deviation for adaptive pooling
      tmp <- c(tmp, purrr::map(i[-1], ~ paste0("sd_", .x, " ~ cauchy(0, 2.5)")))

      ret_list[[length(ret_list) + 1]] <- tmp
    }
    # Each prior needs to be an element in a list
    m_prior <- as.list(unlist(ret_list))
  } else {
    m_prior <- lapply(unlist(m_coefs), function(var) {
      paste0(var, " ~ normal(0, 5)")
    })
  }

  # data block ---------------------------------------------------------------
  body_data <- concat(
    indent, "// Number of observations, factor levels, etc.\n",
    indent, "int<lower=1> N;\n"
  )

  # Declare number of factor levels if any factors are in the model
  if (length(fvs) > 0) {
    for (fv in fvs) {
      body_data <- concat(
        body_data,
        indent, "int<lower=1> N_", fv, ";\n"
      )
    }
  }

  # Response data (bernoulli or binomial)
  body_data <- concat(
    body_data, "\n",
    indent, "// Response Data\n"
  )
  if (data_mode == "bernoulli") {
    body_data <- concat(
      body_data,
      indent, "int ", LHS, "[N];\n"
    )
  } else if (data_mode == "binomial") {
    body_data <- concat(
      body_data,
      indent, "int ", LHS[1], "[N];\n",
      indent, "int ", LHS[2], "[N];\n"
    )
  } else {
    stop("Data responses must be bernoulli or binomial")
  }

  # Numeric variables
  if (has_numeric) {
    body_data <- concat(
      body_data, "\n",
      indent, "// Numeric Data\n"
    )
    for (nv in nvs) {
      body_data <- concat(
        body_data,
        indent, "vector[N] ", nv, ";\n"
      )
    } # for nv
  } # if nv

  # Factor variables
  if (has_factor) {
    body_data <- concat(
      body_data, "\n",
      indent, "// Factor Data\n"
    )
    for(fv in fvs) {
      body_data <- concat(
        body_data,
        indent, "int ", fv, "[N];\n"
      )
    } # for fv
  } # if fv

  # transformed data block ---------------------------------------------------
  # Only required if there are numeric variables
  block_transformed_data <- NULL
  if (has_numeric) {
    body_td_def  <- NULL
    body_td_calc <- NULL
    for (nv in nvs) {
      body_td_def <- concat(
        body_td_def,
        indent, "real mu_", nv, ";\n",
        indent, "real sd_", nv, ";\n",
        indent, "vector[N] ", nv, "_std;\n\n"
      )
      body_td_calc <- concat(
        body_td_calc, "\n",
        indent, "mu_", nv, " = mean(", nv, ");\n",
        indent, "sd_", nv, " = 2 * sd(", nv, ");\n",
        indent, nv, "_std = (", nv, " - mu_", nv, ") / sd_", nv, ";\n"
      )
    }
    block_transformed_data <- concat(
      "transformed data{\n",
      body_td_def,
      body_td_calc,
      "}\n"
    )
  }

  # parameters block ---------------------------------------------------------
  body_parameters <- NULL

  if (!has_numeric && !has_factor && !has_intercept) {
    # y ~ 0
    stop("You must specify a non-null model to fit.")

  } else if (has_numeric && !has_factor && !has_intercept ) {
    # y ~ b1*x1 + b2*x2 + ...
    body_parameters <- concat(
      indent, "// Slope terms\n",
      paste0(indent, "real b_", nvs, ";\n")
    )

  } else if (!has_numeric && has_factor && !has_intercept) {
    # y ~ k1 + k2 + ...
    body_parameters <- concat(
      indent, "// Intercept terms\n",
      concat(indent, "vector[N_", fvs, "] a_", fvs, ";\n")
    )

  } else if (!has_numeric && !has_factor && has_intercept) {
    # y ~ 1
    body_parameters <- concat(
      indent, "// Intercept terms\n",
      indent, "real a;\n"
    )

  } else if (has_numeric && has_factor && !has_intercept) {
    # y ~ (bx1 + bx1_k1)*x1 + (bx2 + bx2_k1)*x2 + ...
    body_parameters <- concat(
      indent, "// Slope terms\n",
      concat(indent, "real b_", nvs, ";\n")
    )
    for (nv in nvs) {
      body_parameters <- concat(
        body_parameters,
        concat(indent, "vector[N_", fvs, "] b_", nv, "_", fvs, ";\n")
      )
    }

  } else if (has_numeric && !has_factor && has_intercept) {
    # y ~ a + b1*x1 + b2*x2 + ...
    body_parameters <- concat(
      indent, "// Intercept terms\n",
      indent, "real a;\n\n",
      indent, "// Slope terms\n",
      concat(indent, "real b_", nvs, ";\n")
    )

  } else if (!has_numeric && has_factor && has_intercept) {
    # y ~ a + k1 + k2 + ...
    body_parameters <- concat(
      indent, "// Intercept terms\n",
      indent, "real a;\n",
      concat(indent, "vector[N_", fvs, "] a_", fvs, ";\n")
    )

  } else {
    # y ~ (a + k) + (bx1 + bx1_k1)*x1 + (bx2 + bx2_k1)*x2 + ...
    body_parameters <- concat(
      indent, "// Intercept terms\n",
      indent, "real a;\n",
      concat(indent, "vector[N_", fvs, "] a_", fvs, ";\n")
    )
    body_parameters <- concat(
      body_parameters, "\n",
      indent, "// Slope terms\n",
      concat(indent, "real b_", nvs, ";\n"),
    )
    for (nv in nvs) {
      body_parameters <- concat(
        body_parameters,
        concat(indent, "vector[N_", fvs, "] b_", nv, "_", fvs, ";\n")
      )
    }
  }

  # Adaptive pooling terms
  if (adaptive_pooling && has_factor) {
    body_parameters <- concat(
      body_parameters, "\n",
      indent, "// Adaptive Pooling terms\n"
    )
    for (coefs in m_coefs) {
      for (coef in coefs[-1]) {
        body_parameters <- concat(
          body_parameters,
          indent, paste0("real<lower=machine_precision()> sd_", coef, ";\n")
        )
      } # for coef
    } # for coef group
  } # if adaptive

  # model block --------------------------------------------------------------
  body_model <- concat(
    indent, "vector[N] theta;\n\n",
    indent, "// Priors\n",
    concat(indent, m_prior, ";\n"), "\n",
    indent, "// General linear model\n",
    indent, "for (i in 1:N) {\n",
    indent, indent, f_link, ";\n",
    indent, "}\n",
    indent, m_dist, ";\n"
  )

  # generated quantities block -----------------------------------------------
  # Only necessary if there are numeric variables
  block_generated_quantities <- NULL
  if (has_numeric) {
    body_gq_def <- NULL

    # Intercept terms
    if (has_intercept) {
      body_gq_def <- concat(
        body_gq_def,
        indent, "// Intercept terms\n",
        indent, "real alpha;\n"
      )
      # Add vectors for factors if any are in the model
      if (has_factor) {
        for (fv in fvs) {
          body_gq_def <- concat(
            body_gq_def,
            indent, "vector[N_", fv, "] alpha_", fv, ";\n"
          )
        } # for fv
      } # if has factors
    } # if has intercept

    # Slope terms
    body_gq_def <- concat(
      body_gq_def, "\n",
      indent, "// Slope terms\n"
    )
    for (nv in nvs) {
      body_gq_def <- concat(
        body_gq_def,
        indent, "real beta_", nv, ";\n"
      )
      # Factor slope terms
      if (length(fvs) > 0) {
        for (fv in fvs) {
          body_gq_def <- concat(
            body_gq_def,
            indent, "vector[N_", fv, "] beta_", nv, "_", fv, ";\n"
          )
        } # for fv
      } # if fv
    } # for nv

    body_gq_calc <- concat(
      indent, "// Calculations\n"
    )
    if (!has_numeric && !has_factor && !has_intercept) {
      # y ~ 0

    } else if (has_numeric && !has_factor && !has_intercept ) {
      # y ~ bx1*x1 + bx2*x2 + ...
      for (nv in nvs) {
        body_gq_calc <- concat(
          body_gq_calc,
          indent, "beta_", nv, " = b_", nv, " / sd_", nv, ";\n"
        )
      }

    } else if (!has_numeric && has_factor && !has_intercept) {
      # y ~ k1 + k2 + ...
      # nothing to do

    } else if (!has_numeric && !has_factor && has_intercept) {
      # y ~ 1
      # nothing to do

    } else if (has_numeric && has_factor && !has_intercept) {
      # y ~ (bx1 + bx1_k1)*x1 + (bx2 + bx2_k1)*x2 + ...
      for (nv in nvs) {
        body_gq_calc <- concat(
          body_gq_calc,
          indent, "beta_", nv, " = b_", nv, " / sd_", nv, ";\n"
        )
        for (fv in fvs) {
          body_gq_calc <- concat(
            body_gq_calc,
            indent, "beta_", nv, "_", fv, " = b_", nv, "_", fv, " / sd_", nv, ";\n"
          )
        }
      }

    } else if (has_numeric && !has_factor && has_intercept) {
      # y ~ a + b1*x1 + b2*x2 + ...

      # Process alpha = a - b1*mu_x1/sd_x1 - b2*mu_x2/sd_x2 - ...
      tmp <- paste0("b_", nvs, " * mu_", nvs, " / sd_", nvs, collapse = " - ")
      body_gq_calc <- concat(
        body_gq_calc,
        indent, "alpha = a - ", tmp, ";\n"
      )

      # Process beta_xi = b_xi / sd_xi
      for (nv in nvs) {
        body_gq_calc <- concat(
          body_gq_calc,
          indent, "beta_", nv, " = b_", nv, " / sd_", nv, ";\n"
        )
      }

    } else if (!has_numeric && has_factor && has_intercept) {
      # y ~ a + k1 + k2 + ...
      # nothing to do

    } else {
      # y ~ (a + k) + (bx1 + bx1_k1)*x1 + (bx2 + bx2_k1)*x2 + ...

      # Process alpha = a - b1*mu_x1/sd_x1 - b2*mu_x2/sd_x2 - ...
      tmp <- paste0("b_", nvs, " * mu_", nvs, " / sd_", nvs, collapse = " - ")
      body_gq_calc <- concat(
        body_gq_calc,
        indent, "alpha = a - ", tmp, ";\n"
      )

      # Process alpha_k = a_k - b1_k*mu_x1/sd_x1 - b2_k*mu_x2/sd_x2 - ...
      for (fv in fvs) {
        tmp <- paste0("b_", nvs, "_", fv, " * mu_", nvs, " / sd_", nvs, collapse = " - ")
        body_gq_calc <- concat(
          body_gq_calc,
          indent, "alpha_", fv, " = a_", fv, " - ", tmp, ";\n"
        )
      }

      # Process beta_xi = b_xi / sd_xi
      for (nv in nvs) {
        body_gq_calc <- concat(
          body_gq_calc,
          indent, "beta_", nv, " = b_", nv, " / sd_", nv, ";\n"
        )
      }

      for (nv in nvs) {
        for (fv in fvs) {
          body_gq_calc <- concat(
            body_gq_calc,
            indent, "beta_", nv, "_", fv, " = b_", nv, "_", fv, " / sd_", nv, ";\n"
          )
        } # for fv
      } # for nv
    }
    block_generated_quantities <- concat(
      "generated quantities{\n",
      body_gq_def, "\n",
      body_gq_calc,
      "}\n"
    )
  } # if has nvs

  # Put it all together ------------------------------------------------------
  concat(
    "data{\n",
    body_data,
    "}\n",
    block_transformed_data,
    "parameters{\n",
    body_parameters,
    "}\n",
    "model{\n",
    body_model,
    "}\n",
    block_generated_quantities
  )

}
