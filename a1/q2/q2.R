library(readr)
library(tibble)
library(MASS)
data(Boston)
Boston = as_tibble(Boston)

set.seed(42)
boston_index = sample(1:nrow(Boston), size = 400)
train_boston = Boston[boston_index, ]
test_boston = Boston[-boston_index, ]

fit = lm(medv ~ . ^ 2, data = train_boston)
