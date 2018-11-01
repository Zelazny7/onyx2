infer_distro <- function(y) {
  if (any(is.na(y))) {
    stop("Cannot have missing values in response variable.")
  }
  if (all(y %in% 0:1)) {
    "bernoulli"
  } else if (is.factor(y)) {
    "multinomial"
  } else {
    "gaussian"
  }
}

#' @export
discretize_gbm <- function(x, y, w=rep(1, length(x)), interaction.depth=5,
                           min.cnt=10, var.monotone=0, depth=5, shrinkage=1,
                           bag.fraction=1, distribution=infer_distro(y),
                           verbose=FALSE, ...) {
  mod <- tryCatch({

    capture.output(
      mod <- gbm::gbm.fit(
        x = data.frame(x),
        y = y,
        w = w,
        n.minobsinnode = min.cnt,
        interaction.depth = interaction.depth,
        bag.fraction = bag.fraction,
        shrinkage = shrinkage,
        var.monotone = var.monotone,
        keep.data = FALSE,
        verbose = verbose,
        n.trees = 1,
        distribution = distribution,
        ...)
    )
    mod
  },
  error = function(e) {
    warning("gbm not fit properly: ", e)
    NULL
  })

  if (is.null(mod)) {
    c(-Inf, Inf)
  } else {
      tree <- mod$trees[[1L]]
      sort(c(-Inf, tree[[2]][tree[[1]] != -1], Inf))
  }
}
