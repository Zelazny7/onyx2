
## transform has the following info:
new_transform <- function() {
  structure(
    list(
      mapping = list(),
      neutral = list(),
      excepts = list()
    ),
    class = "transform"
  )
}


label <- function(x) list(label=paste0(x, collapse=","), members=x)

bins <- function(start, stop) {
  list(label=sprintf("(%f - %f]", start, stop), right=stop)
}

#' @export
new_discrete_transform <- function(levels) {
  tf <- new_transform()
  ## check that levels are character
  stopifnot(is.character(levels))
  tf$mapping <- lapply(levels, label)
  class(tf) <- c("discrete.transform", "transform")
  tf
}

#' @export
new_numeric_transform <- function(cuts) {
  tf <- new_transform()
  stopifnot(is.numeric(cuts))
  cuts <- unique(c(-Inf, cuts, Inf))

  ## stagger the numeric mapping levels tp create labels
  tf$mapping <- mapply(bins, head(cuts, -1), tail(cuts, -1), SIMPLIFY = FALSE)
  class(tf) <- c("numeric.transform", "transform")
  tf
}

## Things common to both types of transforms
#' @export
predict.transform <- function(tf, res, ...) {
  res <- addNA(res)
  levels(res) <- c(levels(res), as.character(tf$excepts), "Missing")
  res[is.na(res)] <- "Missing"
  res
}

#' @export
predict.discrete.transform <- function(tf, x) {
  lvls <- sapply(tf$mapping, "[[", "label")
  mems <- lapply(tf$mapping, "[[", "members")
  res <- factor(x, unlist(mems))
  levels(res) <- setNames(mems, lvls)
  NextMethod("predict", object=tf, res=res)
}

#' @export
predict.numeric.transform <- function(tf, x) {
  cuts <- unique(c(-Inf, sapply(tf$mapping, "[[", "right")))
  lbls <- sapply(tf$mapping, "[[", "label")
  res <- cut(x, cuts, labels = lbls, right = TRUE)
  NextMethod("predict", object=tf, res=res)
}

#' @export
collapse <- function(tf, i) UseMethod("collapse")

check_inputs <- function(x, ...) UseMethod("check_inputs")

check_inputs.numeric.transform <- function(x, i) {
  ## return a transform with modified levels
  if (!all(abs(diff(i)) == 1))
    stop("Must specify adjacent range")
  if (min(i) < 1 | max(i) > length(tf$mapping))
    stop("Must specify values within current range")
}

check_inputs.discrete.transform <- function(x, i) {
  if (min(i) < 1 | max(i) > length(tf$mapping))
    stop("Must specify values within current range")
}

#' @export
collapse.numeric.transform <- function(tf, i) {
  check_inputs(tf, i)

  ## otherwise collapse the mapping
  cuts <- sapply(tf$mapping, "[[", "right")
  res <- new_numeric_transform(cuts[-i])
  tf$mapping <- res$mapping
  tf
}

#' @export
collapse.discrete.transform <- function(tf, i) {
  check_inputs(tf, i)

  mems <- lapply(tf$mapping, "[[", "members")
  ## insert after the first one in the set
  spot <- min(i)
  comb <- list(unlist(mems[i]))
  mems <- mems[-i]
  mems <- append(mems, comb, after=spot-1L)
  tf$mapping <- lapply(mems, label)
  tf
}

### expand as well ....
expand <- function(tf, i) UseMethod("expand")

expand.numeric.transform <- function(tf, i) {
  check_inputs(tf, i)
}

expand.discrete.transform <- function(tf, i) {
  check_inputs(tf, i)
}

# neutralize <- function(tf, i)
