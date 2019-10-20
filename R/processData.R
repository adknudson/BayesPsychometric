.processData <- function(data, fls, data_classes) {
  # Data needs to be a list
  data <- as.list(as.data.frame(data))
  # Get total number of observations
  data[["N"]] <- length(data[[fls[["LHS"]][1]]])
  # Convert factors to integers if any factors are specified
  if (length(data_classes[["factor_vars"]]) > 0) {
    data[data_classes[["factor_vars"]]] <- lapply(
      data[data_classes[["factor_vars"]]],
      as.integer)
    # Get the number of levels of each factor
    for (fv in data_classes[["factor_vars"]]) {
      data[[paste0("N_", fv)]] <- length(unique(data[[fv]]))
    }
  }
  data
}
