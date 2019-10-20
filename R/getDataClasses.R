.getDataClasses <- function(fls, data) {

  # Get the class of each column ---------------------------------------------
  # Ordered factors have two classes (ordered, factor) so get just the first
  data_classes <- unlist(lapply(data, function(col) class(col)[1]))

  # Split up the classes by part of formula ----------------------------------
  response <- data_classes[fls[["LHS"]]]
  predictors <- data_classes[fls[["vars"]]]

  factor_vars <- subset(fls[["vars"]],
                        data_classes[fls[["vars"]]] %in% c("factor", "ordered"))
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
