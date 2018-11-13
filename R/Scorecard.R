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
      ## Make sure perf and variables don't have any names in common
      pf <- names(perf)
      k <- pf %in% names(variables)
      if (any(k)) {
        stop("Performance vars cannot be in dataset: ", paste0(pf[k], collapse=", "), call. = FALSE)
      }


      variables <<- variables
      perf <<- perf
      step <<- setNames(rep(NA_real_, length(variables)), names(variables))
      callSuper()
    },

    ## Experimental funtionality
    ..display_variable = function(v) {
      ## append information from fitted model?
      res <- make_table.variable(variables[[v]], perf[[1]])

      ## format here...
      do.call(format, c(x=list(res), getOption("onyx2_format", default = list())))

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
      select(tail(names(models), 1))
      # current_model <<- tail(names(models), 1)
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

    predict_model = function(newx=..as.data.frame..()) {

      if (identical(length(models), 0L)) stop("No models have been fit", call. = F)

      select(current_model)
      mod <- models[[current_model]]
      v <- mod$modelvars
      wgt <- lapply(mod$transforms[v], "[[", "weights")
      dat <- transform(newx[v], type="factor")
      res <- rowSums(mapply(function(x, w) w[x], dat, wgt)) + mod$intercept
      unname(res)

    },

    transform = function(newx, type=c("factor", "sparse", "perf"), pf=perf[[1]]) {

      res <- mapply(transform.variable, variables[names(newx)], newx,
                    MoreArgs = list(
                      type=match.arg(type),
                      perf=pf),
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

    ..infer_family = function() {
      if (is(perf[[1]], "perf_binomial")) "binomial" else "gaussian"
    },

    fit = function(newx=..as.data.frame..(), steps=c(1:2, NA), method=c("onyx", "binnr"), s="lambda.min", family=..infer_family(), ...) {

      v <- get_step(steps) ## get the variables requested

      mod <- switch(
        match.arg(method),
        onyx = ..fit_onyx_style(newx, v, s, family=family, ...),
        binnr = ..fit_binnr_style(newx, v, s, family=family, ...)
      )
      ..add_model(mod)
      return(invisible())
    },

    ..fit_binnr_style = function(newx, v, s, alpha=1, nfolds=5, ...) {


      ## convert to factor dataset
      factors <- transform(newx[v], type="factor")
      tables <- lapply(factors, function(x) make_table(perf[[1]], x))

      x <- transform(newx[v], type="perf")
      mod <- glmnet::cv.glmnet(x, y=perf[[1]]$y, w=perf[[1]]$w,
                        alpha=alpha,
                        nfolds=nfolds,
                        lower.limits=0,
                        upper.limits=3,
                        keep=TRUE, ...)


      coefs <- coef(mod, s=s)
      old_tfs <- lapply(variables, "[[", "tf")
      new_tfs <- update_transforms_binnr(old_tfs[v], tables, coefs[-1,], perf_col(perf[[1]]))

      new_model(
        method="binnr",
        mod=mod,
        step=step,
        intercept=coefs[1,],
        transforms=modifyList(old_tfs, new_tfs),
        perf=names(perf)[[1]],
        modelvars=colnames(x))
    },

    ..fit_onyx_style = function(newx, v, s, alpha=0, nfolds=5, ...) {

      x <- transform(newx[v], type="sparse")
      mod <- glmnet::cv.glmnet(do.call(cbind, x), y=perf[[1]]$y, w=perf[[1]]$w,
                               alpha=alpha,
                               nfolds=nfolds,
                               keep=TRUE, ...)

      coefs <- coef(mod, s=s)
      old_tfs <- lapply(variables, "[[", "tf")
      new_tfs <- update_transforms_onyx(old_tfs[v], lapply(x, colnames), coefs[-1,])

      ## Create the specific model object here
      new_model(
        method="onyx",
        mod=mod,
        step=step,
        intercept = coefs[1,],
        transforms=modifyList(old_tfs, new_tfs),
        perf=names(perf)[[1]],
        modelvars=names(x))

    },

    ..set_step = function(v, s) {
      ## must be by name
      if(!is(v, "character")) stop("Must specify step by name", call. = F)

      not_found <- !v %in% names(variables)
      if(any(not_found)) stop("Requested variables not found: ",
                              paste0(v[not_found], collapse = ", "), call. = F)

      if (!all(s %in% c(1:3, NA))) stop("Requested step must be in c(1:3,NA)", call. = F)

      step[v] <<- s

    },

    ..collapse = function(v, i) {
      variables[[v]] <<- collapse(variables[[v]], i)
    },

    ..expand = function(v, i) {
      variables[[v]] <<- expand(variables[[v]], i, w=perf[[1]]$w)
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

