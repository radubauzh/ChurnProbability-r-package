library(data.table)
library(lubridate)
library(profvis)
library(Hmisc)
library(testthat)
library(devtools)


setwd("/Users/rafaeldubach/Desktop/UNI/Master/3 Semester/R - a non-technical introduction to big data/Final_Project")


customers <- fread("data_customer.csv")
personal <- fread("data_personal.csv")

# data[, TransDate:=dmy(TransDate)]


merged_data <- merge(customers, personal, by = "CustomerId")
merged_data$Exited <- as.factor(merged_data$Exited)
merged_data$Gender <- as.factor(merged_data$Gender)
str(merged_data)

# 3. Predict churn probability
churn_model <- glm(Exited ~ CreditScore + Gender + Age + Tenure + Balance + NumOfProducts + HasCrCard + IsActiveMember + EstimatedSalary, data = merged_data, family = "binomial")
merged_data$ChurnProbability <- predict(churn_model, newdata = merged_data, type = "response")
highest_churn <- merged_data[which.max(merged_data$ChurnProbability), ]
lowest_churn <- merged_data[which.min(merged_data$ChurnProbability), ]

average_churn_by_gender <- aggregate(ChurnProbability ~ Gender, data = merged_data, FUN = mean)
average_churn_by_gender_tapply <- tapply(merged_data$ChurnProbability, merged_data$Gender, mean)

# 4. Create a package for churn prediction
getChurnProbability <- function(dataset, customerId) {
  if (!customerId %in% dataset$CustomerId) {
    stop("Customer ID not found in the dataset")
  }
  return(dataset[CustomerId == customerId, ]$ChurnProbability)
}

# create a package (already done)
library(MyFinalPackage)
# test package
getChurnProbability(merged_data, "15565701")

# Tests
# Assuming this test is inside your package's tests/testthat directory
test_that("Highest churn probability is greater than lowest", {
  library(MyFinalPackage) # Ensure your package is loaded to access its functions

  # Use actual IDs from your analysis
  highest_id <- highest_churn$CustomerId
  lowest_id <- lowest_churn$CustomerId

  highest_prob <- getChurnProbability(merged_data, highest_id)
  lowest_prob <- getChurnProbability(merged_data, lowest_id)

  expect_gt(highest_prob, lowest_prob)
  print("nice")
})




