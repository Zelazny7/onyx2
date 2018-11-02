
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

