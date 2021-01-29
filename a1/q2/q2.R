# install.packages(c("readr", "tibble"))

library(readr)
library(tibble)
library(MASS)
data(Boston)
Boston = as_tibble(Boston)

rmse = function(actual, predicted) {
  sqrt(mean((actual - predicted) ^ 2))
}

get_rmse = function(model, data, response) {
  rmse(actual = subset(data, select = response, drop = TRUE),
       predicted = predict(model, data))
}

get_complexity = function(model) {
  length(coef(model)) - 1
}

set.seed(42)
boston_index = sample(1:nrow(Boston), size = 400)
train_boston = Boston[boston_index, ]
test_boston = Boston[-boston_index, ]

fit = lm(medv ~ .^2, data = train_boston)
fit_smaller = lm(medv ~ ., data = train_boston)
fit_larger = lm(medv ~ .^2 + I(black^2), data=train_boston)

get_rmse(fit, data=train_boston, response="medv")
get_rmse(fit_smaller, data=train_boston, response="medv")
get_rmse(fit_larger, data=train_boston, response="medv")

get_rmse(fit, data=test_boston, response="medv")
get_rmse(fit_smaller, data=test_boston, response="medv")
get_rmse(fit_larger, data=test_boston, response="medv")

get_complexity(fit)
get_complexity(fit_smaller)
get_complexity(fit_larger)

