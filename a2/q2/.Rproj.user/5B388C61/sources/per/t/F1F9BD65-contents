h2q1_trn <- read.csv("h2q1-trn-data.csv")
h2q1_tst <- read.csv("h2q1-tst-data.csv")

B = "dodgerblue"
O = "darkorange"

convert_to_num = function(str) {
  ifelse(str == O, 0, 1)
}

h2q1_trn$y = convert_to_num(h2q1_trn$y)
h2q1_tst$y = convert_to_num(h2q1_tst$y)

calc_class_error = function(actual, predicted) {
  mean(actual != predicted)
}

raw_m1 = glm(y ~ 1, data = h2q1_trn, family = "binomial")
raw_m2 = glm(y ~ ., data = h2q1_trn, family = "binomial")
raw_m3 = glm(y ~ . + I(x1^ 2) + I(x2^ 2), data = h2q1_trn, family = "binomial")
raw_m4 = glm(y ~ . + I(x1^ 2) + I(x2^ 2) + I(x1*x2), data = h2q1_trn, family = "binomial")

m1_trn_pred = ifelse(predict(raw_m1, h2q1_trn, type = "response") > 0.5, 1, 0)
m1_tst_pred = ifelse(predict(raw_m1, h2q1_tst, type = "response") > 0.5, 1, 0)
calc_class_error(h2q1_trn$y, m1_trn_pred)
calc_class_error(h2q1_tst$y, m1_tst_pred)


m2_trn_pred = ifelse(predict(raw_m2, h2q1_trn, type = "response") > 0.5, 1, 0)
m2_tst_pred = ifelse(predict(raw_m2, h2q1_tst, type = "response") > 0.5, 1, 0)
calc_class_error(h2q1_trn$y, m2_trn_pred)
calc_class_error(h2q1_tst$y, m2_tst_pred)


m3_trn_pred = ifelse(predict(raw_m3, h2q1_trn, type = "response") > 0.5, 1, 0)
m3_tst_pred = ifelse(predict(raw_m3, h2q1_tst, type = "response") > 0.5, 1, 0)
calc_class_error(h2q1_trn$y, m3_trn_pred)
calc_class_error(h2q1_tst$y, m3_tst_pred)


m4_trn_pred = ifelse(predict(raw_m4, h2q1_trn, type = "response") > 0.5, 1, 0)
m4_tst_pred = ifelse(predict(raw_m4, h2q1_tst, type = "response") > 0.5, 1, 0)
calc_class_error(h2q1_trn$y, m4_trn_pred)
calc_class_error(h2q1_tst$y, m4_tst_pred)

