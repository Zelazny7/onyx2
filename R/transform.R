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


check_inputs <- function(x, ...) UseMethod("check_inputs")
