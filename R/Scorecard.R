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
    ..display_variable = function(v) {
      x <- predict.variable(variables[[v]], variables[[v]]$x, type="factor", perf=perf[[1]])
      make_table(perf[[1]], x)
    },

    ..select_performance = function(v) {
      ## to move to the front
      perf <<- perf[c(v, setdiff(names(perf), v))]
    },

    ..as.data.frame.. = function() {
      data.frame(lapply(variables, "[[", "x"))
    },

    predict = function(newx=..as.data.frame..(), type=c("factor", "sparse", "perf")) {

      i <- names(variables)

      ## loop over each pass stuff in ... profit
      res <- mapply(predict.variable, variables, newx[i],
                    MoreArgs = list(
                      type=match.arg(type),
                      perf=perf[[1]]),
                    SIMPLIFY = FALSE)

      ## return object based on type
      switch(
        match.arg(type),
        factor = data.frame(res),
        sparse = do.call(Matrix::cBind, res),
        perf = do.call(cbind, res)
      )

    }



  )
)
