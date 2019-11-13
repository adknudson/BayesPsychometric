#' Convert a typical formula to a formula list that map2stan can understand
#'
#' @param formula A formula that you would pass to `glm` or similar.
#' @param data A data frame of observations.
#' @param link A link function such as "logit" or "probit"
#' @return A model and data that can be used by `rstan::stan` as well as
#'   information about the model and the data.
#' @export
f2stan <- function(formula, data, link, adaptive_pooling = FALSE) {

  # Preliminary checks -------------------------------------------------------
  assertthat::assert_that(link %in% c("logit", "probit"),
                          msg = "'Link' function must be either 'logit' or 'probit'.")

  # Extract features from formula --------------------------------------------
  fls <- .extractFromFormula(formula)
  fls[["adaptive_pooling"]] <- adaptive_pooling
  has_intercept <- fls[["include_intercept"]]

  # Pre-process the data -----------------------------------------------------
  # Standardize the numeric variables
  data <- .preProcessData(fls, data)

  # Get the class of each column in the model --------------------------------
  metadata <- .getDataClasses(fls, data)

  # Build up the model pieces ------------------------------------------------
  m_dist  <- .buildDistributionFormula(fls, link)
  m_link  <- .buildLinkFormula(metadata, link, has_intercept)
  m_coefs <- .getModelCoefs(metadata, has_intercept)
  m_prior <- .buildPriorFormula(m_coefs, adaptive_pooling)

  metadata[["coefs"]] <- m_coefs

  model <- list(mode = m_dist,
                linear_model = m_link,
                priors = m_prior)

  # Process the data ---------------------------------------------------------
  data <- .processData(data, metadata)

  # Build the Stan code
  StanCode <- .buildStanCode(data, model, fls, metadata)

  # Return the model ---------------------------------------------------------
  # model
  list(formula = formula,
       has_intercept = has_intercept,
       adaptive_pooling = adaptive_pooling,
       metadata = metadata,
       data = data,
       StanCode = StanCode)
}
