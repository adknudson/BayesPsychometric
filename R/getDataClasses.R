.getDataClasses <- function(fls, data) {

  # Get the class of each column ---------------------------------------------
  data_classes <- unlist(lapply(data, class))

  # Split up the classes by part of formula ----------------------------------
  response <- data_classes[fls[["LHS"]]]
  predictors <- data_classes[fls[["vars"]]]

  factor_vars <- subset(fls[["vars"]],
                        data_classes[fls[["vars"]]] == "factor")
  numeric_vars <- subset(fls[["vars"]],
                         data_classes[fls[["vars"]]] %in% c("numeric", "integer"))
  character_vars <- subset(fls[["vars"]],
                           data_classes[fls[["vars"]]] == "character")

  list(response = response,
       predictors = predictors,
       numeric_vars = numeric_vars,
       factor_vars = factor_vars,
       character_vars = character_vars)
}
