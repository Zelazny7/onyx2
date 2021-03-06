
#' @export
new_perf <- function(y, w=rep(1, length(y)), type=c("binomial", "continuous")) {
  if (any(is.na(y))) stop("`y` not allowed to have NAs")
  type <- match.arg(type)
  res <- switch(
    type,
    binomial = new_perf_binomial(y, w),
    continuous = new_perf_continuous(y, w),
    stop("perf type not supported", call. = F)
  )
  class(res) <- c(class(res), "perf")
  res
}


#' @export
make_table <- function(perf, x, ...) UseMethod("make_table")
#make_table <- memoise::memoise(function(perf, x, ...) UseMethod("make_table"))


#' @export
plot_data <- function(perf, tbl, x, ...) UseMethod("plot_data")

## Returns the name of the column to be used for WoE substitution

#' @export
perf_col <- function(x) UseMethod("perf_col")
