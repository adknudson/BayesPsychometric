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
