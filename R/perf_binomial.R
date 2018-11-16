
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


#' @export
plot_data <- function(perf, tbl, x, ...) {
  if (missing(tbl)) tbl <- make_table(perf, x)

  ## store everything needed to plot in a list
  tmp <- head(tbl, -1)
  lbls <- rev(row.names(tmp))

  data <- list(
    text = list(
      list(
        value = lbls,
        margin = "left"
      ),
      list(
        value = sprintf("%0.1f%%", rev(tmp[,"%N"] * 100)),
        margin = "right"
      )
    ),
    series = list(
      list(
        values = rev(tmp[,"WoE"]),
        label = "Weight of Evidence",
        color = "red"
      )
    )
  )

  class(data) <- "plot_data_perf_binomial"
  data

}

make_bars_ <- function(v, width=0.70, ...) {
  left <- pmin(v, 0)
  right <- pmax(v, 0)
  center <- seq_along(left)
  top <- center - 0.5 * width
  bottom <- center + 0.5 * width
  rect(left, bottom, right, top, ...)
  center
}


#' @export
plot.plot_data_perf_binomial <- function(plt) {

  on.exit(par(oma=rep(0, 4))) # restore them on exit

  lbls <- plt$text[[1]]$value
  woe <- plt$series[[1]]$values

  xlim <-  range(woe) + c(-0.5, 0.5)
  width <- max(nchar(plt$text[[1]]$value))
  par(oma=c(0, width/6, 0, 0))



  graphics::plot(NA, xlim=xlim, ylim=c(0.5, length(woe) + 0.5),
                 xlab = plt$series[[1]]$label, ylab=NA, yaxt="n", main = "Plot Test")


  abline(v = 0, lty=3)
  center <- make_bars_(woe, col=rgb(0, 0, 0, alpha = 0.30))

  text(x = min(xlim), y = center, labels =
         sprintf(" [%02d]", rev(seq_along(lbls))), cex=0.80)

  text(x = max(xlim) - 0.1, y = center, labels = plt$text[[2]]$value, cex=0.80)

  axis(side = 2, labels = lbls, at = center, las = 2, lwd.ticks = 0,
       cex.axis = 0.80)



}

