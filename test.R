### TESTING STUFF ###

tf <- new_numeric_transform(0:10)
tf <- new_discrete_transform(letters)
tf$mapping[[10]] <- list(label="j,k,l", members=c("j","k","l"))
tf$mapping[11:12] <- NULL
predict(tf, letters)

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

perf <- perf_new(y=titanic$Fare, type="continuous")
make_table(perf, x)

