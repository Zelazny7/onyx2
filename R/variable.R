## Class for storing variable and transformation

#'@name new_variable
#'@title Variable Container
#'@param name Character name of the variable
#'@param x A vector containing the variable data
#'@param transform An object created using \link{new_transform} that contains
#'information necessary to bin a variable.
#'@return A list of the above arguments with a hist element that is a list of
#'all changes to the transform.
#' @export
new_variable <- function(name, x, transform) {
  structure(
    list(
      name = name,
      x = x,
      tf = transform,
      hist = list()
    ),
    class = "variable"
  )
}


#' @export
make_table.variable <- memoise(function(v, perf) {
  make_table(perf, predict(v$tf, v$x))
})


#' @export
collapse.variable <- function(x, i) {
  x$hist <- append(x, x$tf)
  x$tf <- collapse(x$tf, i)
  x
}


#' @export
expand.variable <- function(x, i) {
  x$hist <- append(x, x$tf)
  x$tf <- expand(x$tf, i)
  x
}


#' @export
predict.variable <- function(x, newx, type=NULL) {
  predict(x$tf, newx)
}


