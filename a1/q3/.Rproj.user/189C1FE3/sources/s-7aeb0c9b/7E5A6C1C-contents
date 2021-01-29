rmse = function(actual, predicted) {
  sqrt(mean((actual - predicted) ^ 2))
}

h1q3_test <- read.csv("h1q3-test-data.csv")
h1q3_train <- read.csv("h1q3-train-data.csv")

for (k in seq(5, 50, by=5)) {
  cat("k:", k, "\t")
  pred_train = FNN::knn.reg(train = h1q3_train["x"], test = h1q3_test["x"], y = h1q3_train$y, k = k)$pred
  train_rmse = rmse(predicted = pred_train, actual = h1q3_test$y)
  
  pred_test = FNN::knn.reg(train = h1q3_train["x"], test = h1q3_train["x"], y = h1q3_train$y, k = k)$pred
  test_rmse = rmse(predicted = pred_test, actual = h1q3_test$y)
  
  cat("train rmse:", train_rmse, "\ttest rmse:", test_rmse, "\n")
}
