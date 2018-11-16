##### TEST HERE


data(titanic, package="onyx")
perf <- list(
  Survived = new_perf(titanic$Survived),
  Fare = new_perf(titanic$Fare, type = "continuous"))

bins <- bin(titanic[-c(1,7)], perf, var.monotone=0, interaction.depth=3)

bins$..display_variable("Embarked")
bins$..collapse("Embarked", c(1,3))
bins$..display_variable("Embarked")
bins$..expand("Embarked", 1)
bins$..display_variable("Embarked")

bins$..display_variable("Age")
bins$..expand("Age", 2)
bins$..display_variable("Age")
bins$..collapse("Age", 1:3)
bins$..display_variable("Age")

bins$..neutralize("Age", 1)
bins$..display_variable("Age")

bins$fit(titanic, method="onyx")
bins$..display_variable("Age")
bins$fit(titanic, method="binnr")

bins

bins <- bin(titanic[-c(1,7)], perf, var.monotone=0, interaction.depth=3)

tbl <- make_table.variable(bins$variables$Age, bins$perf$Survived)

plt <- plot_data(bins$perf$Survived, tbl=tbl)

plot(plt)

plt <- plot_data(bins$perf$Survived, x = titanic$Embarked)


bins$..neutralize("Age", 1)
bins$fit(titanic, method="binnr")
bins$..display_variable("Age")

p1 <- bins$predict_model()

## verify results match
woe <- bins$transform(newx = titanic, type="perf", pf = bins$perf$Survived)

p2 <- predict(bins$models$model1$object, woe, s="lambda.min")


bins$..set_step(c("Age", "Embarked", "Pclass"), 1)

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

bins$..select_performance("Fare")
bins$fit(method="onyx")


p1 <- bins$predict_model()

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


