.buildDistributionFormula <- function(fls, data_mode) {
  if (data_mode == "bernoulli") {
    m_dist <- paste0(fls[["LHS"]], " ~ dbinom(1, theta)")
  } else {
    successes <- fls[["LHS"]][1]
    trials    <- fls[["LHS"]][2]
    m_dist <- paste0(successes, " ~ dbinom(", trials, ", theta)")
  }
  m_dist
}
