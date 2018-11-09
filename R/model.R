### Model object for storing results from fitting

## creates a new model object and then calls the next method
#' @export
new_model <- function(method, mod, step, transforms, perf, description="") {

  base <- structure(
    list(
      object=mod,
      step=step,
      transforms=transforms,
      perf = perf,
      description = description
    ),
    class = "model")

  switch(
    method,
    onyx = new_model_binnr(base),
    binnr = new_model_onyx(base),
    stop("Model not supported"))
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

new_model_binnr <- function(mod) {
  class(mod) <- c(class(mod), "model_binnr")
  mod
}

new_model_onyx <- function(mod) {
  class(mod) <- c(class(mod), "model_onyx")
  mod
}

