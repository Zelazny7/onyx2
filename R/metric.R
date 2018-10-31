
Metric <- setRefClass(
  "Metric",
  contains = "VIRTUAL",
  methods = list(
  )
)


## Needs to return WoE and information value for each row and a total
MetricIV <- setRefClass(
  "MetricIV",
  contains = "Metric",
  methods = list(

    tbl = function(x) {

      woe <- log(x[,'%1']/x[,'%0'])
      woe[is.nan(woe) | is.infinite(woe)] <- 0
      iv <- (x[,'%1'] - x[,'%0']) * woe

      res <- cbind(WoE=woe, IV=iv)

      rbind(
        res,
        'Total'=c(0, sum(res[,'IV'])))
    }
  )
)
