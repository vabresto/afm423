library(ISLR) 
library(caret)
library(e1071)
library(randomForest)
library(kernlab)

calc_acc = function(actual, predicted) {
  mean(actual == predicted)
}

uin = STUDENT_ID 
set.seed(uin)
oj_idx = createDataPartition(OJ$Purchase, p = 0.5, list = FALSE)
oj_trn = OJ[oj_idx,]
oj_tst = OJ[-oj_idx,]

lin_grid = expand.grid(C = c(2 ^ (-5:5)))
rad_grid = expand.grid(C = c(2 ^ (-2:3)), sigma = c(2 ^ (-3:1)))

cv_5 = trainControl(method = "cv", number = 5)

lin_model = train(
  Purchase ~ .,
  data = oj_trn,
  trControl = cv_5,
  method = "svmLinear",
  tuneGrid = lin_grid
)
lin_model$bestTune
calc_acc(oj_tst$Purchase, predicted = predict(lin_model, newdata = oj_tst))

poly_model = train(
  Purchase ~ .,
  data = oj_trn,
  trControl = cv_5,
  method = "svmPoly"
)
poly_model$bestTune
calc_acc(oj_tst$Purchase, predicted = predict(poly_model, newdata = oj_tst))

rad_model = train(
  Purchase ~ .,
  data = oj_trn,
  trControl = cv_5,
  method = "svmRadial",
  tuneGrid = rad_grid
)
rad_model$bestTune
calc_acc(oj_tst$Purchase, predicted = predict(rad_model, newdata = oj_tst))

rf_model = train(
  Purchase ~ .,
  data = oj_trn,
  method = "rf",
)
rf_model$bestTune
calc_acc(oj_tst$Purchase, predicted = predict(rf_model, newdata = oj_tst))
