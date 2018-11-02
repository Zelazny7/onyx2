##---- Discrete Transform

make_level_discrete_ <- function(x) list(label=paste0(x, collapse=","), members=x)


#' @export
new_transform_discrete <- function(levels) {
  tf <- new_transform()
  ## check that levels are character
  stopifnot(is.character(levels))
  tf$mapping <- lapply(levels, make_level_discrete_)
  class(tf) <- c("transform_discrete", class(tf))
  tf
}


#' @export
predict.transform_discrete <- function(tf, x) {
  lvls <- sapply(tf$mapping, "[[", "label")
  mems <- lapply(tf$mapping, "[[", "members")
  res <- factor(x, unlist(mems))
  levels(res) <- setNames(mems, lvls)
  NextMethod("predict", object=tf, res=res)
}


check_inputs.transform_discrete <- function(x, i) {
  if (min(i) < 1 | max(i) > length(x$mapping))
    stop("Must specify values within current range")
}


#' @export
collapse.transform_discrete <- function(x, i) {
  check_inputs(x, i)
  mems <- lapply(x$mapping, "[[", "members")
  ## insert after the first one in the set
  spot <- min(i)
  comb <- list(unlist(mems[i]))
  mems <- mems[-i]
  mems <- append(mems, comb, after=spot-1L)
  x$mapping <- lapply(mems, make_level_discrete_)
  x
}


expand.transform_discrete <- function(x, i) {
  check_inputs(x, i)
}
