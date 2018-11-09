
.onLoad <- function(package, libname) {
  ## set formatting option for tables
  options("onyx2_format"=list(
    digits=2,
    nsmall=2,
    big.mark=",",
    big.interval=3))
}
