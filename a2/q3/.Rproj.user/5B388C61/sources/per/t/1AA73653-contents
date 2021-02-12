set.seed(123456789)

LOOPS = 1000
MODELS = 3

make_sim_data = function(n_obs = 25) {
  x1 = runif(n = n_obs, min = 0, max = 2)
  x2 = runif(n = n_obs, min = 0, max = 4)
  prob = exp(1 + 2 * x1 - 1 * x2) / (1 + exp(1 + 2 * x1 - 1 * x2))
  y = rbinom(n = n_obs, size = 1, prob = prob)
  data.frame(y, x1, x2)
}

calc_var = function(estimate) { 
  mean((estimate - mean(estimate)) ^ 2) 
} 
calc_bias = function(estimate, truth) { 
  (mean(estimate) - truth)^2
} 
calc_mse = function(truth, estimate) { 
  mean((estimate - truth) ^ 2) 
}

results = matrix(0, nrow = LOOPS, ncol = MODELS)
ground_truth = data.frame(x1 = 1, x2 = 1, y = exp(1 + 2 * 1 - 1 * 1) / (1 + exp(1 + 2 * 1 - 1 * 1)))

for (loop in 1:LOOPS) {
  sim_data = make_sim_data()
  
  raw_m1 = glm(y ~ 1, data = sim_data, family = "binomial")
  raw_m2 = glm(y ~ ., data = sim_data, family = "binomial")
  raw_m3 = glm(y ~ . + I(x1^ 2) + I(x2^ 2) + I(x1*x2), data = sim_data, family = "binomial")
  
  results[loop, 1] = predict(raw_m1, newdata = ground_truth, type = "response")
  results[loop, 2] = predict(raw_m2, newdata = ground_truth, type = "response")
  results[loop, 3] = predict(raw_m3, newdata = ground_truth, type = "response")
}

calc_var(results[,1])
calc_bias(results[,1], ground_truth$y)
calc_mse(results[,1], ground_truth$y)

calc_var(results[,2])
calc_bias(results[,2], ground_truth$y)
calc_mse(results[,2], ground_truth$y)

calc_var(results[,3])
calc_bias(results[,3], ground_truth$y)
calc_mse(results[,3], ground_truth$y)
