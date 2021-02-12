h2q1_trn <- read.csv("h2q1-trn-data.csv")
h2q1_tst <- read.csv("h2q1-tst-data.csv")

B = "dodgerblue"
O = "darkorange"

m1_f = function(val1){
  ifelse(val1 > 0, B, O)
}

m2_f = function(val1, val2) {
  ifelse(val2 > val1 + 1, B, O)
}

m3_f = function(val1, val2) {
  ifelse(val2 > val1 + 1, B, ifelse(val2 < val1 -1, B, O))
}

m4_f = function(val1, val2) {
  ifelse(val2 > (val1 + 1) ^ 2, B, ifelse(val2 < -(val1 - 1) ^ 2, B, O))
}

calc_class_error = function(actual, predicted) {
  mean(actual != predicted)
}

m1_trn_pred = m1_f(val1=h2q1_trn$x1)
m1_tst_pred = m1_f(val1=h2q1_tst$x1)
calc_class_error(h2q1_trn$y, m1_trn_pred)
calc_class_error(h2q1_tst$y, m1_tst_pred)


m2_trn_pred = m2_f(val1=h2q1_trn$x1, val2=h2q1_trn$x2)
m2_tst_pred = m2_f(val1=h2q1_tst$x1, val2=h2q1_tst$x2)
calc_class_error(h2q1_trn$y, m2_trn_pred)
calc_class_error(h2q1_tst$y, m2_tst_pred)


m3_trn_pred = m3_f(val1=h2q1_trn$x1, val2=h2q1_trn$x2)
m3_tst_pred = m3_f(val1=h2q1_tst$x1, val2=h2q1_tst$x2)
calc_class_error(h2q1_trn$y, m3_trn_pred)
calc_class_error(h2q1_tst$y, m3_tst_pred)


m4_trn_pred = m4_f(val1=h2q1_trn$x1, val2=h2q1_trn$x2)
m4_tst_pred = m4_f(val1=h2q1_tst$x1, val2=h2q1_tst$x2)
calc_class_error(h2q1_trn$y, m4_trn_pred)
calc_class_error(h2q1_tst$y, m4_tst_pred)

