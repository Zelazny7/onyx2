### TESTING STUFF ###

x <- runif(1e6)
y <- rbinom(1e6, 1, 0.25)

pf <- new_perf(y)

## get a factor transform
tf <- new_transform_numeric(quantile(x, seq(0.1, 0.9, 0.1)), exceptions = -1)
#tf <- collapse(tf, 1:2)
v <- new_variable("Random", x=x, tf)

v2 <- collapse(v, 1:5)
tbl <- make_table(v, pf)

x <- mtcars$mpg
tf <- new_transform_numeric(c(10,20,30), exceptions = c(21, 30.4, 15.2))

predict(tf, x)

table(predict(v2, x))


##### TEST HERE

data(titanic, package="onyx")

data(mtcars)

perf <- new_perf(y=mtcars$mpg, type="continuous")
# make_table(perf, mtcars$Pclass)

# perf <- list(
#   Fare = new_perf(titanic$Fare, type = "continuous"),
#   Survived = new_perf(titanic$Survived, type="binomial")
# )

bins <- bin(mtcars[-1], list(mpg=perf), var.monotone=0)
bins$fit(method = "binnr")
bins$..neutralize("disp", 1:2)
bins$fit(method = "onyx")

bins$models$model1$transforms$disp$neutral
bins$models$model2$transforms$disp$neutral

bins$select("model2")
bins$..display_variable("disp")
bins$select("model1")
bins$..display_variable("disp")


bins


bins$collapse("disp", 1:2)

bins$predict(type="sparse")
bins$fit(method = "onyx")

#p = predict(mod, bins$predict(type="sparse"))



bins$variables$disp <- neutralize(bins$variables$disp, 1:2)

bins$variables$disp <- undo(bins$variables$disp)
bins$..display_variable("disp")


length(bins$variables$disp$hist)

bins$variables$disp$hist[[1]]


# bins$variables
# bins$variables$disp$tf

bins$..display_variable("cyl")
bins$..select_performance("Survived")

bins$predict(type="sparse")
predict(bins$variables$disp, newx = mtcars$disp, type = "sparse")

predict(bins$variables$Embarked, newx = bins$variables$Embarked$x, perf=perf$Fare, type = "perf")

bins$predict(type = "sparse")



p <- mapply(predict, bins, titanic[-1], SIMPLIFY = FALSE)

## make tables
tbls <- lapply(p, function(x) make_table(pf, x))













