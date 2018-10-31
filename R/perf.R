Perf <- setRefClass("Perf", contains = "VIRTUAL", fields = c(y="vector", w="numeric"))

BinomialPerf <- setRefClass("BinomialPerf", contains = "Perf")

ContinuousPerf <- setRefClass("ContinuousPerf", contains = "Perf")

Metric <- setRefClass(
  "Metric",
  contains = "VIRTUAL",
  methods = list(



  )
)
