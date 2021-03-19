# A3

## Q1

```R
library(e1071)
library(randomForest)
library(mlbench)

calc_class_error = function(actual, predicted) {
  mean(actual == predicted)
}

set.seed(42)
sim_trn = mlbench.spirals(n = 2500, cycles = 1.5, sd = 0.125)
sim_trn = data.frame(sim_trn$x, class = as.factor(sim_trn$classes))
sim_tst = mlbench.spirals(n = 10000, cycles = 1.5, sd = 0.125)
sim_tst = data.frame(sim_tst$x, class = as.factor(sim_tst$classes))

uin = STUDENT_ID 
set.seed(uin)

library(caret)
cv_5 = trainControl(method = "cv", number = 5)

glm_cv_time = system.time({
  sim_glm_cv = train(class ~ ., data = sim_trn, trControl = cv_5, method = "glm")
})
tree_cv_time = system.time({
  sim_tree_cv = train(class ~ ., data = sim_trn, trControl = cv_5, method = "rpart")
})


glm_cv_time["elapsed"]
tree_cv_time["elapsed"]

library(rpart.plot)
rpart.plot(sim_tree_cv$finalModel)

rf_grid = expand.grid(mtry = c(1, 2))
oob_method = trainControl(method = "oob")

rf_oob_time = system.time({
  rf_model_oob = train(class ~ ., data = sim_trn, trControl = oob_method, method = "rf", tuneGrid = rf_grid)
})
rf_cv_time = system.time({
  rf_model_cv = train(class ~ ., data = sim_trn, trControl = cv_5, method = "rf", tuneGrid = rf_grid)
})

rf_cv_time["elapsed"]
rf_oob_time["elapsed"]

sim_glm_cv$bestTune
sim_tree_cv$bestTune
rf_model_cv$bestTune
rf_model_oob$bestTune

sim_glm_cv$results
sim_tree_cv$results
rf_model_cv$results
rf_model_oob$results

glm_tst = predict(sim_glm_cv, newdata = sim_tst)
calc_class_error(sim_tst$class, glm_tst)

tree_tst = predict(sim_tree_cv, newdata = sim_tst)
calc_class_error(sim_tst$class, tree_tst)

rf_model_cv_tst = predict(rf_model_cv, newdata = sim_tst)
calc_class_error(sim_tst$class, rf_model_cv_tst)

rf_model_oob_tst = predict(rf_model_oob, newdata = sim_tst)
calc_class_error(sim_tst$class, rf_model_oob_tst)

```

## Q2

```R
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

# Q5
plot(gb_cv)
varImpPlot(rf_oob$finalModel)
varImpPlot(bt_oob$finalModel)

```

## Q3

```R
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

```
