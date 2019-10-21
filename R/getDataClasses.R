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
