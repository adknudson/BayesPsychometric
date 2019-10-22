#' Convert a typical formula to a formula list that map2stan can understand
#'
#' @param formula A formula that you would pass to `glm` or similar.
#' @param data A list or data.frame.
#' @param link A link function such as "logit" or "probit"
#' @return A model that is passed to `rstan::stan`
#' @export
f2stan <- function(formula, data, link) {

  # Preliminary checks -------------------------------------------------------
  assertthat::assert_that(link %in% c("logit", "probit"),
                          msg = "'Link' function must be either 'logit' or 'probit'.")

  # Extract features from formula --------------------------------------------
  fls <- .extractFromFormula(formula)

  # Pre-process the data -----------------------------------------------------
  # Standardize the numeric variables
  data <- .preProcessData(fls, data)

  # Get the class of each column in the model --------------------------------
  data_classes <- .getDataClasses(fls, data)

  # Build up the model pieces ------------------------------------------------
  m_dist  <- .buildDistributionFormula(fls, link)
  m_link  <- .buildLinkFormula(fls, data_classes, link)
  m_terms <- .getModelTerms(fls, data_classes)
  m_prior <- .buildPriorFormula(m_terms)

  model <- list(mode = m_dist,
                linear_model = m_link,
                priors = m_prior)

  # Process the data ---------------------------------------------------------
  data <- .processData(data, fls, data_classes)

  # Build the Stan code
  modelCode <- .buildStanCode(data, model, fls, data_classes)

  # Return the model ---------------------------------------------------------
  # model
  list(model = model,
       fls = fls,
       data_classes = data_classes,
       data = data,
       StanCode = modelCode,
       terms = m_terms)
}
