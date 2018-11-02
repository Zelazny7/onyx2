
new_perf_continuous <- function(y, w) {
  structure(list(y=y, w=w), class="perf_continuous")
}

tbl_continuous_ <- function(x, y, w) {
  z <- tapply(perf$w, x, sum)
  w <- tapply(perf$w * y, x, sum)
  tbl <- cbind(N=z, Perf=w)
  tbl[is.na(tbl)] <- 0
  tbl
}

calc_var_ <- function(x, y, w) {
  tbl <- c(
    mapply(Hmisc::wtd.var, split(y, x), split(w, x)),
    Total=Hmisc::wtd.var(y, w))
  tbl[is.na(tbl)] <- 0
  tbl
}

#' @export
make_table.perf_continuous <- function(perf, x, ...) {
  stopifnot(is.factor(x))
  tbl <- tbl_continuous_(x, perf$y, perf$w)

  pt <- prop.table(tbl, margin = 2L)
  colnames(pt) <- c("%N", "%Perf")
  res <- cbind(tbl, pt, Mean=tbl[,"Perf"]/tbl[,"N"])
  res[is.nan(res) | is.infinite(res)] <- 0

  ## pass info to the metric to calculate what it needs
  variance <- calc_var_(x, perf$y, perf$w)

  ## add totals for res
  tot <- colSums(res)
  tot["Mean"] <- tot["Perf"]/tot["N"]

  ## Combine everything
  cbind(rbind(res, Total=tot), Variance=variance)
}
