mse = function(actual, predicted) {
  mean((actual - predicted) ^ 2)
}

h1q4_test <- read.csv("h1q4-test-data.csv")
h1q4_train <- read.csv("h1q4-train-data.csv")

# do for k = 1, 5, 25
for (k in list(1, 5, 25)) {
  predTest = FNN::knn.reg(train = h1q4_train[, -1], test = h1q4_test[, -1], y = h1q4_train$y, k = k)$pred
  test_mse = mse(predicted = predTest, actual = h1q4_test$y)
  
  predScaled = FNN::knn.reg(train = scale(h1q4_train[, -1]), test = scale(h1q4_test[, -1]), y = h1q4_train$y, k = k)$pred
  scaled_test_mse = mse(predicted = predScaled, actual = h1q4_test$y)
  
  cat("k:", k)
  cat("\ttest mse:", test_mse, "\tscaled test mse:", scaled_test_mse, "\n")
}
