##---- Numeric Transform

make_level_continuous_ <- function(start, stop) {
  list(label=sprintf("(%f - %f]", start, stop), right=stop)
}


#' @export
new_transform_numeric <- function(cuts, exceptions=numeric()) {
  tf <- new_transform()
  stopifnot(is.numeric(cuts))
  cuts <- unique(c(-Inf, cuts, Inf))

  ## stagger the numeric mapping levels tp create labels
  tf$mapping <- mapply(make_level_continuous_,
                       head(cuts, -1), tail(cuts, -1), SIMPLIFY = FALSE)
  tf$exceptions <- exceptions
  class(tf) <- c("transform_numeric", class(tf))
  tf
}


#' @export
predict.transform_numeric <- function(tf, x) {
  cuts <- unique(c(-Inf, sapply(tf$mapping, "[[", "right")))
  lbls <- sapply(tf$mapping, "[[", "label")
  res <- cut(x, cuts, labels = lbls, right = TRUE)
  levels(res) <- c(levels(res), tf$exceptions)
  res[x %in% tf$exceptions] <- as.character(tf$exceptions)
  NextMethod("predict", object=tf, res=res)
}


check_inputs.transform_numeric <- function(x, i) {
  ## return a transform with modified levels
  if (!all(abs(diff(i)) == 1))
    stop("Must specify adjacent range")
  if (min(i) < 1 | max(i) > length(x$mapping))
    stop("Must specify values within current range")
}


#' @export
collapse.transform_numeric <- function(x, i) {
  check_inputs(x, i)

  ## otherwise collapse the mapping
  cuts <- sapply(x$mapping, "[[", "right")
  res <- new_transform_numeric(cuts[-i])
  x$mapping <- res$mapping
  x
}


expand.transform_numeric <- function(x, i) {
  check_inputs(x, i)
}

#' @export
neutralize.transform_numeric <- function(x, i) {
  if (min(i) < 1 | max(i) > length(x$mapping))
    stop("Must specify values within current range")

  labels <- sapply(x$mapping, "[[", "label")
  z <- labels[[i]]

  x$neutral <-
    if (z %in% x$neutral)
      setdiff(x$neutral, z)
    else
      union(x$neutral, z)

  x
}




