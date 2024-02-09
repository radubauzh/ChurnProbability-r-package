getChurnProbability <- function(dataset, customerId) {
  if (!customerId %in% dataset$CustomerId) {
    stop("Customer ID not found in the dataset")
  }
  return(dataset[CustomerId == customerId, ]$ChurnProbability)
}
