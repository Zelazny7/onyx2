
#' @export
perf_new <- function(y, w=rep(1, length(y)), type=c("binomial", "continuous")) {
  res <- list(y=y, w=w)
  type <- match.arg(type)
  switch(
    type,
     binomial = {
       if(!all(y %in% 0:1)) {
         stop("binomial perf must only contain 0s and 1s", call. = FALSE)
       }
     },
     {
       if (any(is.na(y))) stop("y cannot have NA values")
     })

  class(res) <- c("perf", paste0("perf.", match.arg(type), collapse = ""))
  res
}

#' @export
make_table <- function(perf, x, ...) UseMethod("make_table")

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
make_table.perf.binomial <- function(perf, x, ...) {
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
make_table.perf.continuous <- function(perf, x, ...) {
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
