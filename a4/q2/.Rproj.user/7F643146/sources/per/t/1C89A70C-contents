library(ISLR)
library(caret)
library(e1071)
library(randomForest)
library(gbm)

calc_rmse = function(actual, predicted) {
  sqrt(mean((actual - predicted) ^ 2))
}

Hitters = na.omit(Hitters)

uin = STUDENT_ID 
set.seed(uin)
hit_idx = createDataPartition(Hitters$Salary, p = 0.6, list = FALSE)
hit_trn = Hitters[hit_idx,]
hit_tst = Hitters[-hit_idx,]

cv_5 = trainControl(method = "cv", number = 5)
oob_method = trainControl(method = "oob")

gbm_grid = expand.grid(interaction.depth = c(1, 2),
  n.trees = c(500, 1000, 1500),
  shrinkage = c(0.001, 0.01, 0.1),
  n.minobsinnode = 10)

rf_grid = expand.grid(mtry = c(1:19))
bt_grid = expand.grid(mtry = 19)

gb_cv = train(
  Salary ~ .,
  data = hit_trn,
  trControl = cv_5,
  method = "gbm",
  tuneGrid = gbm_grid
)

rf_oob = train(
  Salary ~ .,
  data = hit_trn,
  trControl = oob_method,
  method = "rf",
  tuneGrid = rf_grid
)

bt_oob = train(
  Salary ~ .,
  data = hit_trn,
  trControl = oob_method,
  method = "rf",
  tuneGrid = bt_grid
)

gb_cv$bestTune
gb_cv$results
calc_rmse(hit_tst$Salary, predicted = predict(gb_cv, newdata = hit_tst))

rf_oob$bestTune
rf_oob$results
calc_rmse(hit_tst$Salary, predicted = predict(rf_oob, newdata = hit_tst))

bt_oob$bestTune
bt_oob$results
calc_rmse(hit_tst$Salary, predicted = predict(bt_oob, newdata = hit_tst))
