##---- Base Transform

new_transform <- function() {
  structure(
    list(
      mapping = list(),
      neutral = list()
    ),
    class = "transform"
  )
}


#' @export
predict.transform <- function(tf, res, ...) {
  res <- addNA(res)
  levels(res) <- c(levels(res), as.character(tf$excepts), "Missing")
  res[is.na(res)] <- "Missing"
  res
}


#' @export
collapse <- function(x, i) UseMethod("collapse")


#' @export
expand <- function(x, i) UseMethod("expand")


#' @export
neutralize <- function(x, i) UseMethod("neutralize")

#' @export
neutralize.transform <- function(x, i) {
  if (min(i) < 1 | max(i) > length(x$mapping))
    stop("Must specify values within current range")

  labels <- sapply(x$mapping, "[[", "label")

  vals <- x$neutral
  for (el in unlist(labels[i])) {
    if (el %in% x$neutral) ## if requested lvl already neutral, remove it
      vals <- setdiff(vals, el)
    else ## otherwise add it
      vals <- union(vals, el)
  }
  x$neutral <- vals
  x
}


check_inputs <- function(x, ...) UseMethod("check_inputs")
