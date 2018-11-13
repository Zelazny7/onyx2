### Model object for storing results from fitting

## creates a new model object and then calls the next method
#' @export
new_model <- function(method, mod, step, intercept, transforms, perf, modelvars, description="") {

  model <- structure(
    list(
      object=mod,
      step=step,
      intercept=intercept,
      transforms=transforms,
      perf = perf,
      modelvars=modelvars,
      description = description
    ),
    class = "model")

  class(model) <- c(class(model), paste0("model_", method))
  model

}

setOldClass(c("model", "model_binnr", "model_onyx"))

#' @export
toString.model <- function(x, ...) {
  ## TODO: print other stats about the model and stuff
  cat("Model --", NextMethod(), sep="")
}

#' @export
toString.model_binnr <- function(x, ...) {
  "binnr"
}

#' @export
toString.model_onyx <- function(x, ...) {
  "onyx"
}

### what things should the model object have?

## mapping of variable levels to weights?

#' @export
make_table.model_onyx <- function(mod, tbl) {

  browser()
  print()


}

#' @export
model_table <- function(mod, tbl, v) UseMethod("model_table")


#' @export
model_table.model_binnr <- function(mod, tbl, v) {
  browser()
  print()

}


#' @export
model_table.model_onyx <- function(mod, tbl, v) {

  browser()
  print()
}
