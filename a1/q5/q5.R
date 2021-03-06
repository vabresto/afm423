# install.packages("ISLR")
library(ISLR)

rmse = function(actual, predicted) {
  sqrt(mean((actual - predicted) ^ 2))
}

get_rmse = function(model, data, response) {
  rmse(actual = subset(data, select = response, drop = TRUE),
       predicted = predict(model, data))
}

auto = Auto[, !names(Auto) %in% c("name")]
set.seed(42)
auto_idx = sample(1:nrow(auto), size = round(0.5 * nrow(auto)))
auto_trn = auto[auto_idx, ]
auto_tst = auto[-auto_idx, ]

model = lm(mpg ~ ., data = auto_trn)

get_rmse(model, data=test_data, response="mpg")

pred = FNN::knn.reg(train = scale(auto_trn[, -1]), test = scale(auto_trn[, -1]), y = auto_tst$y, k = 6)$pred
rmse(predicted = pred, actual = auto_tst$y)
