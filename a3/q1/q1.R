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

