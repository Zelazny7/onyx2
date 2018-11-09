#' @include Scorecard.R

#' @export
bin <- function(x, perf, discretize, exceptions, ...) UseMethod("bin")

#' @export
bin.default <- function(x, perf, discretize=discretize_gbm, exceptions=numeric(), name="", ...) {
  cuts <- discretize(x, perf$y, perf$w, ...)
  tf <- new_transform_numeric(cuts, exceptions = exceptions)
  new_variable(name = name, transform = tf, x = x)
}

#' @export
bin.factor <- function(x, perf, discretize, name="", ...) {
  tf <- new_transform_discrete(levels(x))
  new_variable(name = name, transform = tf, x = x)
}

check_perf_ <- function(perf) {
  if (!is.list(perf) || is.null(names(perf)))
    stop("perf must be a named list")
  if (!all(sapply(perf, is, "perf")))
    stop("perf must be passed as a list of perf objects")
}

#' @export
bin.data.frame <- function(x, perf, discretize=discretize_gbm, exceptions=numeric(), ...) {
  check_perf_(perf)
  vars <- list()
  for (nm in names(x)) {
    vars[[nm]] <- bin(x[[nm]], perf[[1]], discretize, exceptions=exceptions, name=nm, ...)
  }
  Scorecard(vars, perf)
}
