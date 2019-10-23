.preProcessData <- function(fls, data) {

  # We call data_classes in here because standardizing will change integer
  # predictors to numeric, and we want that change to be reflected later on
  data_classes <- .getDataClasses(fls, data)
  has_intercept <- fls[["include_intercept"]]

  # if there are numeric variables, standardize them
  if (length(data_classes[["numeric_vars"]]) > 0) {
    for (nv in data_classes[["numeric_vars"]]) {
      # Drop needs to be used to return a vector while keeping the attributes
      if (has_intercept) {
        data[[nv]] <- drop(scale(data[[nv]]))
      } else {
        data[[nv]] <- drop(scale(data[[nv]], center = FALSE))
      }
    }
  }

  data
}
