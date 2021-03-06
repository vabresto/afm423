# A3

## Q1

```R
library(caret)
library(glmnet)
library(readr)
library(e1071)

set.seed(STUDENT_ID)

leukemia = read_csv("leukemia.csv", progress = FALSE)
leukemia$class = as.factor(leukemia$class)

y = as.factor(leukemia$class)
X = as.matrix(leukemia[, -1])

fit_ridge = glmnet(X, y, alpha = 0, family = "binomial")
plot(fit_ridge)
plot(fit_ridge, xvar = "lambda", label = TRUE)

fit_lasso = glmnet(X, y, alpha = 1, family = "binomial")
plot(fit_lasso)
plot(fit_lasso, xvar = "lambda", label = TRUE)


fit_ridge_cv = cv.glmnet(X, y, alpha = 0, family = "binomial")
plot(fit_ridge_cv)
fit_ridge_cv$lambda.min
fit_ridge_cv$lambda.1se


fit_lasso_cv = cv.glmnet(X, y, alpha = 1, family = "binomial")
plot(fit_lasso_cv)
fit_lasso_cv$lambda.min
fit_lasso_cv$lambda.1se

control = trainControl(method = "cv", number = 5)
rr_grid = expand.grid(alpha = 0, lambda = c(fit_ridge_cv$lambda.min,
                                            fit_ridge_cv$lambda.1se))
lasso_grid = expand.grid(alpha = 1, lambda = c(fit_lasso_cv$lambda.min,
                                               fit_lasso_cv$lambda.1se))
sim_data = data.frame(y, X)
fitRR = train(
  y ~ .,
  data = sim_data,
  method = "glmnet",
  trControl = control,
  tuneGrid = rr_grid
)
fitLasso = train(
  y ~ .,
  data = sim_data,
  method = "glmnet",
  trControl = control,
  tuneGrid = lasso_grid
)
fitRR$results
fitLasso$results

knn_mod = train(
  y ~ .,
  data = sim_data,
  method = "knn",
  trControl = control
)

knn_mod$results

knn_mod_scaled = train(
  y ~ .,
  data = sim_data,
  method = "knn",
  trControl = control,
  preProcess = "scale"
)

knn_mod_scaled$results

```

## Q2

```R
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

```
