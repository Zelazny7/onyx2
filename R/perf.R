#' @include metric.R

Perf <- setRefClass(
  "Perf",
  contains = "VIRTUAL",
  fields = c(y="vector", w="numeric"),
  methods = list(
    initialize = function(y, w=rep(1, length(y))) {
      y <<- y
      w <<- w
      callSuper()
    },
    tbl = function(metric, x) tbl_(.self, metric, x)
  )
)


setGeneric("tbl_", function(perf, metric, x) standardGeneric("tbl_"))

setMethod("tbl_", c("Perf", "Metric", "factor"), function(perf, metric, x) {
  stop("Must Implement")
})


BinomialPerf <- setRefClass(
  "BinomialPerf",
  contains = "Perf",
  methods=list(
    initialize = function(...) {
      callSuper(...)
      stopifnot(all(y %in% 0:1))
      # TODO: better error messages
    }
  ))

weighted_table_ <- function(x, y, w, names=NULL) {
  ones <- tapply((y == 1) * w, x, sum, na.rm=T)
  zero <- tapply((y == 0) * w, x, sum, na.rm=T)
  tbl <- cbind(ones + zero, ones, zero)
  tbl[is.na(tbl)] <- 0
  `colnames<-`(tbl, names)
}


setMethod("tbl_", c("BinomialPerf", "MetricIV", "factor"), function(perf, metric, x) {
  ### N, #1, #0, %N, %1, %0, P(1), ... Metric stuff
  tbl <- weighted_table_(x, perf$y, perf$w, names=c("N", "#1", "#0"))

  pt <- prop.table(tbl, margin = 2L)
  colnames(pt) <- c("%N", "%1", "%0")
  res <- cbind(tbl, pt, `P(1)`=tbl[,"#1"]/tbl[,"N"])
  res[is.nan(res) | is.infinite(res)] <- 0

  ## pass info to the metric to calculate what it needs
  metric_cols <- metric$tbl(res)

  ## add totals for res
  tot <- colSums(res)
  tot["P(1)"] <- tot["#1"]/tot["N"]

  ## Combine everything
  cbind(
    rbind(res, Total=tot),
    metric_cols)

})



ContinuousPerf <- setRefClass("ContinuousPerf", contains = "Perf")

