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

data(titanic, package="onyx")

perf <- new_perf(y=titanic$Fare, type="continuous")
make_table(perf, titanic$Pclass)

perf <- list(
  Fare = new_perf(titanic$Fare, type = "continuous"),
  Survived = new_perf(titanic$Survived, type="binomial")
)

bins <- bin(titanic[-1], perf, var.monotone=0)

# bins$variables

bins$display_variable("Fare")


p <- mapply(predict, bins, titanic[-1], SIMPLIFY = FALSE)

## make tables
tbls <- lapply(p, function(x) make_table(pf, x))







