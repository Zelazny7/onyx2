
d <- GBMDiscretizer$new()
cps <- d$discretize(x=titanic$Age, y=titanic$Fare, depth = 3)





metric <- MetricIV$new()

bins <- onyx::bin(titanic["Fare"], titanic$Survived, max.bin = 4)

perf$tbl(metric, addNA(cut(titanic$Age, cps)))

bins$variables$Fare

microbenchmark::microbenchmark()

perf$tbl(metric, titanic$Sex)

perf$tbl(metric, factor(mtcars$mpg))



### TESTING STUFF ###

tf <- new_numeric_transform(0:10)
tf <- new_discrete_transform(letters)
tf$mapping[[10]] <- list(label="j,k,l", members=c("j","k","l"))
tf$mapping[11:12] <- NULL
predict(tf, letters)

## get a factor transform

tf <- new_discrete_transform(levels(titanic$Pclass))
tf <- collapse(tf, 1:2)

x <- predict(tf, titanic$Pclass)

perf <- perf_new(y=titanic$Fare, type="continuous")
make_table(perf, x)

