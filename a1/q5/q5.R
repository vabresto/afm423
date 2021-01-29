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

train_index = sample(1:nrow(auto), size=round(0.5*nrow(auto)))
train_data = auto[train_index,]
test_data = auto[-train_index,]

pred = FNN::knn.reg(train = scale(train_data[, -1]), test = scale(test_data[, -1]), y = train_data$y, k = 6)$pred
rmse(predicted = pred, actual = test_data$y)
