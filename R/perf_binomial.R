
new_perf_binomial <- function(y, w) {
  if (!all(y %in% 0:1)) stop("binomial perf can only have 0s and 1s")
  structure(list(y=y, w=w), class="perf_binomial")
}

weighted_table_ <- function(x, y, w, names=NULL) {
  ones <- tapply((y == 1) * w, x, sum, na.rm=T)
  zero <- tapply((y == 0) * w, x, sum, na.rm=T)
  tbl <- cbind(ones + zero, ones, zero)
  tbl[is.na(tbl)] <- 0
  `colnames<-`(tbl, names)
}

calc_iv_ <- function(x) {
  woe <- log(x[,'%1']/x[,'%0'])
  woe[is.nan(woe) | is.infinite(woe)] <- 0
  iv <- (x[,'%1'] - x[,'%0']) * woe

  res <- cbind(WoE=woe, IV=iv)

  rbind(res, 'Total'=c(0, sum(res[,'IV'])))
}

#' @export
make_table.perf_binomial <- function(perf, x, ...) {
  stopifnot(is.factor(x))
  tbl <- weighted_table_(x, perf$y, perf$w, names=c("N", "#1", "#0"))

  pt <- prop.table(tbl, margin = 2L)
  colnames(pt) <- c("%N", "%1", "%0")
  res <- cbind(tbl, pt, `P(1)`=tbl[,"#1"]/tbl[,"N"])
  res[is.nan(res) | is.infinite(res)] <- 0

  ## pass info to the metric to calculate what it needs
  iv_cols <- calc_iv_(res)

  ## add totals for res
  tot <- colSums(res)
  tot["P(1)"] <- tot["#1"]/tot["N"]

  ## Combine everything
  cbind(rbind(res, Total=tot), iv_cols)
}


#' @export
perf_col.perf_binomial <- function(x) "WoE"
