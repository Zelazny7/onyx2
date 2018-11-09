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
make_table.variable <- memoise::memoise(function(v, perf) {
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
predict.variable <- function(x, newx=x$x, type=c("factor", "sparse", "perf"), perf=NULL) {

  ## can predict ... perf measure?
  f <- predict(x$tf, newx)
  neutral <- match(unlist(x$tf$neutral), levels(f), 0)

  ## TODO:: Pick up here tomorrow

  switch(
    match.arg(type),
    factor = f,
    sparse = Matrix::sparseMatrix(seq_along(f), as.integer(f)),
    perf = {
      if(is.null(perf)) stop("must supply performance if requesting perf substitution perdiction", call. = F)

      ## grab a column from the performance table
      pf <- make_table(perf, f)[,perf_col(perf)]

      ## TODO:: need to handle neutral values here ... overrides?

      ## index using factor labels
      pf[as.character(f)]

    }
  )



}


