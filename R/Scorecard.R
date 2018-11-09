#' @export
Scorecard <- setRefClass(
  "Scorecard",
  fields = c(variables="list",
             perf="list",
             models="list",
             step="numeric",
             current_model="character"),

  methods = list(
    initialize = function(variables, perf, ...) {
      variables <<- variables
      perf <<- perf
      step <<- setNames(rep(NA_real_, length(variables)), names(variables))
      callSuper()
    },

    ## Experimental funtionality
    ..display_variable = function(v) {
      make_table.variable(variables[[v]], perf[[1]])
    },

    ..select_performance = function(v) {
      ## to move to the front
      perf <<- perf[c(v, setdiff(names(perf), v))]
    },

    ..as.data.frame.. = function(s=names(variables)) {
      data.frame(lapply(variables[s], "[[", "x"))
    },

    ..add_model = function(mod) {
      models <<- c(models, list(mod))
      names(models) <<- paste0("model", seq_along(models))
      current_model <<- tail(names(models), 1)
    },

    ..load_model = function(mod) {
      step <<- mod$step
      for (v in names(mod$transforms)) {
        variables[[v]]$tf <<- mod$transforms[[v]]
      }
      ..select_performance(mod$perf)
    },

    select = function(modname) {
      ## load the transforms and step from the selected model
      if (!modname %in% names(models))
        stop("requested model ", modname, "not found", call. = F)
      current_model <<- modname
      ..load_model(models[[modname]])
    },

    predict = function(newx=..as.data.frame..(), type=c("factor", "sparse", "perf")) {

      i <- names(variables)

      res <- mapply(predict.variable, variables, newx[i],
                    MoreArgs = list(
                      type=match.arg(type),
                      perf=perf[[1]]),
                    SIMPLIFY = FALSE)

      ## return object based on type
      switch(
        match.arg(type),
        factor = data.frame(res),
        sparse = res,
        perf = do.call(cbind, res)
      )
    },

    get_step = function(s) names(step)[step %in% s],

    fit = function(newx=..as.data.frame..(), s=c(1:3, NA), method=c("onyx", "binnr"), ...) {

      mod <- switch(
        match.arg(method),
        onyx = ..fit_onyx_style(newx, s, ...),
        binnr = ..fit_binnr_style(newx, s, ...)
      )

      ..add_model(
        new_model(mod=mod,
                  step=step,
                  transforms=lapply(variables, "[[", "tf"),
                  perf=names(perf)[[1]],
                  method=method)
      )
      return(invisible())
    },

    ..fit_binnr_style = function(newx=..as.data.frame..(), s=c(1:3, NA), alpha=1, nfolds=5, ...) {

      vars <- get_step(s)
      x <- predict(newx, type="perf")
      glmnet::cv.glmnet(x, y=perf[[1]]$y, w=perf[[1]]$w,
                        alpha=alpha,
                        nfolds=nfolds,
                        lower.limits=0,
                        upper.limits=3,
                        keep=TRUE, ...)
    },

    ..fit_onyx_style = function(newx=..as.data.frame..(), s=c(1:3, NA), alpha=0, nfolds=5, ...) {

      vars <- get_step(s)
      x <- predict(newx, type="sparse")
      glmnet::cv.glmnet(do.call(cbind, x), y=perf[[1]]$y, w=perf[[1]]$w,
                        alpha=alpha,
                        nfolds=nfolds,
                        keep=TRUE, ...)
    },

    ..collapse = function(v, i) {
      variables[[v]] <<- collapse(variables[[v]], i)
    },

    ..expand = function(v, i) {
      variables[[v]] <<- expand(variables[[v]], i)
    },

    ..neutralize = function(v, i) {
      variables[[v]] <<- neutralize(variables[[v]], i)
    },

    ..undo = function(v) {
      variables[[v]] <<- undo(variables[[v]])
    }

  )
)

#' @export
setMethod("show", "Scorecard", function(object) {
  cat("Scorecard", sep = "\n")
  for (mod in object$models) {
    cat(toString(mod), sep = "\n")
  }
})

