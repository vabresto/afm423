library(caret)
library(ISLR)
library(party)

set.seed(42)
index = createDataPartition(College$Outstate, p = 0.75, list = FALSE)
college_trn = College[index, ]
college_trn$Private = as.factor(college_trn$Private)
college_tst = College[-index, ]
college_tst$Private = as.factor(college_tst$Private)

calc_rmse = function(actual, predicted) {
  sqrt(mean((actual - predicted) ^ 2))
}


set.seed(STUDENT_ID)
control = trainControl(method = "cv", number = 5)

college_glm_mod = train(
  Outstate ~ ., 
  data = college_trn,
  method = "glm",
  trControl = control
)

college_glmnet_mod = train(
  Outstate ~ ., 
  data = college_trn,
  method = "glmnet",
  trControl = control,
  tuneLength = 10
)

college_glmnet_mod_2 = train(
  Outstate ~ .^2 , 
  data = college_trn,
  method = "glmnet",
  trControl = control,
  tuneLength = 10
)

college_knn_mod = train(
  Outstate ~ ., 
  data = college_trn,
  method = "knn",
  trControl = control
)

college_knn_mod2 = train(
  Outstate ~ .^2 , 
  data = college_trn,
  method = "knn",
  trControl = control
)

college_knn_mod_scaled = train(
  Outstate ~ ., 
  data = college_trn,
  method = "knn",
  trControl = control
)

college_knn_mod2_scaled = train(
  Outstate ~ .^2 , 
  data = college_trn,
  method = "knn",
  trControl = control,
  preProcess = "scale"
)

college_tree_mod = train(
  Outstate ~ ., 
  data = college_trn,
  method = "cforest",
  trControl = control,
  preProcess = "scale"
)

calc_rmse(college_tst$Outstate, predict(college_glm_mod, newdata = college_tst))
calc_rmse(college_tst$Outstate, predict(college_glmnet_mod, newdata = college_tst))
calc_rmse(college_tst$Outstate, predict(college_glmnet_mod_2, newdata = college_tst))
calc_rmse(college_tst$Outstate, predict(college_knn_mod, newdata = college_tst))
calc_rmse(college_tst$Outstate, predict(college_knn_mod2, newdata = college_tst))
calc_rmse(college_tst$Outstate, predict(college_knn_mod_scaled, newdata = college_tst))
calc_rmse(college_tst$Outstate, predict(college_knn_mod2_scaled, newdata = college_tst))
calc_rmse(college_tst$Outstate, predict(college_tree_mod, newdata = college_tst))

# For Q6

college_glm_mod$bestTune
college_glmnet_mod$bestTune
college_glmnet_mod_2$bestTune
college_knn_mod$bestTune
college_knn_mod2$bestTune
college_knn_mod_scaled$bestTune
college_knn_mod2_scaled$bestTune
college_tree_mod$bestTune

calc_best_result = function(caret_fit) {
  best = which(rownames(caret_fit$results) == rownames(caret_fit$bestTune))
  result = caret_fit$results[best, ]
  rownames(result) = NULL
  result
}

calc_best_result(college_glm_mod)
calc_best_result(college_glmnet_mod)
calc_best_result(college_glmnet_mod_2)
calc_best_result(college_knn_mod)
calc_best_result(college_knn_mod2)
calc_best_result(college_knn_mod_scaled)
calc_best_result(college_knn_mod2_scaled)
calc_best_result(college_tree_mod)

college_knn_mod_scaled = train(
  Outstate ~ ., 
  data = college_trn,
  method = "knn",
  trControl = control,
  preProcess = "scale"
)

college_knn_mod2_scaled = train(
  Outstate ~ .^2 , 
  data = college_trn,
  method = "knn",
  trControl = control,
  preProcess = "scale"
)

calc_rmse(college_tst$Outstate, predict(college_knn_mod_scaled, newdata = college_tst))
calc_rmse(college_tst$Outstate, predict(college_knn_mod2_scaled, newdata = college_tst))

college_knn_mod_scaled$results
college_knn_mod2_scaled$results
