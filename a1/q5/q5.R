# install.packages("ISLR")
library(ISLR)
auto = Auto[, !names(Auto) %in% c("name")]
set.seed(42)
auto_idx = sample(1:nrow(auto), size = round(0.5 * nrow(auto)))
auto_trn = auto[auto_idx, ]
auto_tst = auto[-auto_idx, ]

lm(mpg ~ ., data = auto_trn)