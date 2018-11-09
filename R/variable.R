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
make_table.variable <- function(v, perf) {
  tbl <- make_table(perf, predict(v$tf, v$x))
  neutral <- setNames(character(nrow(tbl)), row.names(tbl))
  neutral[unlist(v$tf$neutral)] <- "*"
  res <- data.frame(tbl, Neu=neutral, check.names = F, stringsAsFactors = F)
  do.call(format, c(x=list(res), getOption("onyx2_format", default = list())))
}


#' @export
collapse.variable <- function(x, i) {
  x$hist <- append(x$hist, list(x$tf))
  x$tf <- collapse(x$tf, i)
  x
}


#' @export
expand.variable <- function(x, i) {
  x$hist <- append(x$hist, list(x$tf))
  x$tf <- expand(x$tf, i)
  x
}


#' @export
neutralize.variable <- function(x, i) {
  x$hist <- append(x$hist, list(x$tf))
  x$tf <- neutralize(x$tf, i)
  x
}

#' @export
undo <- function(v) {
  if (identical(length(v$hist), 0L)) {
    return(v)
  } else {
    v$tf <- tail(v$hist, 1)[[1]]
    v$hist <- head(v$hist, -1)
    return(v)
  }
}


#' @export
predict.variable <- function(x, newx=x$x, type=c("factor", "sparse", "perf"), perf=NULL) {

  f <- predict(x$tf, newx)
  neutral <- match(unlist(x$tf$neutral), levels(f), 0)

  switch(
    match.arg(type),
    factor = f,
    sparse = {
      res <- Matrix::sparseMatrix(seq_along(f), as.integer(f))
      if (!all(neutral == 0)) res[,-neutral,drop=FALSE] else res
    },
    perf = {
      if(is.null(perf)) stop("must supply performance if requesting perf substitution perdiction", call. = F)

      ## grab a column from the performance table
      pf <- make_table(perf, f)[,perf_col(perf)]
      pf[neutral] <- pf[['Total']] ## replace neutralized with "average"

      pf[as.character(f)] ## index using factor labels
    }
  )



}


