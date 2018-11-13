##---- Base Transform

new_transform <- function() {
  structure(
    list(
      mapping = list(),
      neutral = list(),
      weights = NULL
    ),
    class = "transform"
  )
}

#' @export
transform <- function(x, y, ...) UseMethod("transform")


#' @export
transform.transform <- function(tf, x, ...) {
  res <- addNA(x)
  levels(res) <- c(levels(x), as.character(tf$excepts), "Missing")
  res[is.na(res)] <- "Missing"
  res
}


#' @export
collapse <- function(x, i) UseMethod("collapse")


#' @export
expand <- function(x, i, ...) UseMethod("expand")


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

update_transforms_binnr <- function(tfs, tbls, coefs, perf) {
  ## loop over coefficients
  for (v in names(coefs)) {
    weights <- head(tbls[[v]][,perf], -1) * coefs[[v]]
    tfs[[v]]$weights <- weights
  }
  tfs
}

update_transforms_onyx <- function(tfs, skeleton, coefs) {
  ## skeleton is the list of column names by variable that map to coefs
  weights <- split(coefs, rep(names(skeleton), lengths(skeleton)))
  for (v in names(weights)) tfs[[v]]$weights <- weights[[v]]
  tfs
}
