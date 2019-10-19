.buildDistributionFormula <- function(fls, link) {
  if (fls[["data_mode"]] == "bernoulli" && link == "logit") {

    # Bernoulli logit --------------------------------------------------------
    m_dist <- paste0(fls[["LHS"]], " ~ bernoulli_logit(theta)")

  } else if (fls[["data_mode"]] == "bernoulli" && link == "probit") {

    # Bernoulli probit -------------------------------------------------------
    m_dist <- paste0(fls[["LHS"]], " ~ bernoulli(theta)")

  } else if (fls[["data_mode"]] == "binomial" && link == "logit") {

    # Binomial logit ---------------------------------------------------------
    successes <- fls[["LHS"]][1]
    trials    <- fls[["LHS"]][2]
    m_dist <- paste0(successes, " ~ binomial_logit(", trials,", theta)")

  } else if (fls[["data_mode"]] == "binomial" && link == "probit") {

    # Binomial probit --------------------------------------------------------
    successes <- fls[["LHS"]][1]
    trials    <- fls[["LHS"]][2]
    m_dist <- paste0(successes, " ~ binomial(", trials, ", theta)")

  } else {
    stop("The model is not 'bernoulli' or 'binomial', and the link is not 'logit' or 'probit'.")
  }

  m_dist
}
