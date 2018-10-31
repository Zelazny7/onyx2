
d <- GBMDiscretizer$new()

cps <- d$discretize(x=titanic$Age, y=titanic$Fare, depth = 3)

perf <- BinomialPerf$new(y=titanic$Fare)
metric <- MetricIV$new()

bins <- onyx::bin(titanic["Fare"], titanic$Survived, max.bin = 4)

perf$tbl(metric, addNA(cut(titanic$Age, cps)))

bins$variables$Fare

microbenchmark::microbenchmark()

perf$tbl(metric, titanic$Sex)

perf$tbl(metric, factor(mtcars$mpg))
