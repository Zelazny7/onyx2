#' @export
Scorecard <- setRefClass(
  "Scorecard",
  fields = c(variables="list", perf="list", models="list"),

  methods = list(
    initialize = function(variables, perf, ...) {
      variables <<- variables
      perf <<- perf
      callSuper()
    },


    ## Experimental funtionality
    display_variable = function(v) {
      x <- predict(variables[[v]]$tf, variables[[v]]$x)
      make_table(perf[[1]], x)
    },

    select_performance = function(v) {
      ## to move to the front
      perf <<- perf[c(v, setdiff(names(perf), v))]
    }



  )
)
