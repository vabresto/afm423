library(caret) # install.packages('e1071', dependencies=TRUE)
library(class)

set.seed(314)

LOOPS = 51

convert_to_num = function(str) {
  # B -> 0, M -> 1
  ifelse(str == "B", 0, 1)
}

calc_class_error = function(actual, predicted) {
  mean(actual != predicted)
}

wisc_trn = read.csv("wisc-trn.csv")
wisc_trn$class = convert_to_num(wisc_trn$class)
wisc_tst = read.csv("wisc-tst.csv")
wisc_tst$class = convert_to_num(wisc_tst$class)

for (loop in seq(1, LOOPS, by=2)) {
  pred = knn(train = wisc_trn[, -1], test = wisc_trn[, -1], cl = wisc_trn$class, k = loop)
  trn_err = calc_class_error(pred, wisc_tst$class)
  
  pred = knn(train = wisc_trn[, -1], test = wisc_tst[, -1], cl = wisc_trn$class, k = loop)
  tst_err = calc_class_error(pred, wisc_tst$class)
  
  cat("k:", loop, "\ttrn_err:", trn_err, "\ttst_err:", tst_err, "\n")
}

base_model = glm(class ~ radius + symmetry, data = wisc_trn, family = "binomial")

m1 = ifelse(predict(base_model, wisc_tst, type = "response") > 0.1, 1, 0)
m2 = ifelse(predict(base_model, wisc_tst, type = "response") > 0.5, 1, 0)
m3 = ifelse(predict(base_model, wisc_tst, type = "response") > 0.9, 1, 0)

(t1 = table(predicted = m1, actual = wisc_tst$class))
(cm1 = confusionMatrix(t1, positive = "1"))
(t2 = table(predicted = m2, actual = wisc_tst$class))
(cm2 = confusionMatrix(t2, positive = "1"))
(t3 = table(predicted = m3, actual = wisc_tst$class))
(cm3 = confusionMatrix(t3, positive = "1"))

