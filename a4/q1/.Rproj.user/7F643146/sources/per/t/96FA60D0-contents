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

