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


#perf <- new_perf(y=titanic$Fare, type="continuous")
#make_table(perf, titanic$Pclass)

data(titanic, package="onyx")
perf <- list(
  Survived = new_perf(titanic$Survived, type="binomial"),
  Fare = new_perf(titanic$Fare, type = "continuous"))

bins <- bin(titanic[-c(1,7)], perf, var.monotone=0, interaction.depth=1)

bins$..display_variable("Embarked")
bins$..collapse("Embarked", c(1,3))

bins$..expand("Embarked", 1)

bins$..display_variable("Age")
bins$..expand("Age", 1)

bins$..set_step(c("Age", "Embarked", "Pclass"), 1)
bins$fit(titanic, method="onyx", steps = 1)

bins$..display_variable("SibSp")
bins$..display_variable("Embarked")

bins$..select_performance("Survived")
bins$fit(titanic, method="binnr")

bins$..display_variable("SibSp")
bins$..display_variable("Embarked")
bins$..display_variable("Sex")

p <- bins$predict_model()

mod <- bins$models[[bins$current_model]]

x <- bins$transform(titanic[mod$modelvars], type = "sparse")
x <- do.call(cbind, x)

p2 <- predict(mod$object, x, s="lambda.min")


p <- bins$predict_model()

p1 <- bins$predict_model(titanic)

bins$fit(titanic, method="onyx")
p2 <- bins$predict_model(titanic)

bins$select("model1")
p3 <- bins$predict_model(titanic)




d <- bins$..display_variable("Age")


ins$model

bins$..display_variable("Age")

bins$..collapse("Age", 1:3)
bins$..display_variable("Age")

bins$..select_performance("Survived")
bins$fit(titanic, method="onyx")

## verify that predictions are the same from the glmnet object





Qp1 <- bins$predict_model()

bins$fit(titanic, method="onyx")
p2 <- bins$predict_model()
plot(p1, p2)
plot(p1, titanic$Fare)
plot(p2, titanic$Fare)



# bins$variables

bins$..select_performance("Survived")


predict(bins$variables$Embarked, newx = bins$variables$Embarked$x, perf=perf$Fare, type = "sparse")

bins$predict(type = "sparse")



p <- mapply(predict, bins, titanic[-1], SIMPLIFY = FALSE)


