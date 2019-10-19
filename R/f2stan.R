#' Convert a typical formula to a formula list that map2stan can understand
#'
#' @param formula A formula that you would pass to `glm` or similar.
#' @param data A list or data.frame.
#' @param link A link function such as "logit" or "probit"
#' @return A formula list that `rethinking::map2stan` can use.
#' @examples
#' # Create a data set
#' n <- 50
#' x1 <- runif(n, -1, 1)
#' x2 <- runif(n, -1, 1)
#' a <- 0
#' b1 <- 10
#' b2 <- -5
#' p <- 1 / (1 + exp(-(a + b1\*x1 + b2\*x2)))
#' y <- rbinom(n, size = 1, prob = p)
#' size <- sample(3:5, n, TRUE)
#' y2 <- rbinom(n, size = size, prob = p)
#' gender <- factor(sample(c("male", "female"), n, TRUE))
#' age <- factor(sample(c("<40", ">=40"), 50, TRUE), levels = c("<40", ">=40"))
#'
#' dat_bern <- data.frame(y = y, x1 = x1, x2 = x2,
#'                        gender = gender, age = age)
#' dat_binom <- data.frame(y = y2, k = size, prop = y2 / size,
#'                         x1 = x1, x2 = x2,
#'                         gender = gender, age = age)
#'
#' # Bernoulli data
#' f2flist(y ~ x1, dat_bern, "logit")
#' f2flist(y ~ x1 + x2 + gender, dat_bern, "logit")
#'
#' # Binomial data
#' f2flist(y|k ~ 0 + x1 + age + gender, dat_binom, "logit")
#' f2flist(y|k ~ x1 + x2, dat_binom, "logit")
#' @export
f2stan <- function(formula, data, link) {

  # Preliminary checks -------------------------------------------------------
  stopifnot(
    link %in% c("logit", "probit")
  )

  # Extract features from formula --------------------------------------------
  fls <- .extractFromFormula(formula)
  if (grepl(pattern = "\\|", fls[["LHS"]])) {
    fls[["data_mode"]] <- "binomial"
    fls[["LHS"]] <- trimws(strsplit(fls[["LHS"]], split = "\\|")[[1]])
  } else {
    fls[["data_mode"]] <- "bernoulli"
  }

  # Get the class of each column in the model --------------------------------
  data_classes <- .getDataClasses(fls, data)

  # Intermediate checks ------------------------------------------------------


  # Build up the formula list ------------------------------------------------
  m_dist  <- .buildDistributionFormula(fls, link)
  m_link  <- .buildLinkFormula(fls, data_classes, link)
  m_terms <- .getModelTerms(fls, data_classes)
  m_prior <- .buildPriorFormula(m_terms)

  # Convert the strings to calls for rethinking::map2stan --------------------
  model <- list(mode = m_dist,
                linear_model = m_link,
                priors = m_prior)

  # Return the model ---------------------------------------------------------
  model
}
