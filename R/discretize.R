Discretizer <- setRefClass(
  "Discretizer",
  contains="VIRTUAL",
  methods = list(
    discretize = function(x, w, ...) stop("Must implement!")
  )
)

SupervisedDiscretizer <- setRefClass(
  "SupervisedDiscretizer",
  contains = c("Discretizer", "VIRTUAL"),
  methods = list(
    discretize = function(x, w, y, ...) stop("Must implement!")
  )
)

UnsupervisedDiscretizer <- setRefClass(
  "UnsupervisedDiscretizer",
  contains = c("Discretizer", "VIRTUAL"),
  methods = list(
    discretize = function(x, w, ...) stop("Must implement!")
  )
)

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

GBMDiscretizer <- setRefClass(
  "GBMDiscretizer",
  contains = "SupervisedDiscretizer",
  methods = list(
    discretize = function(x, y, w=rep(1, length(x)), min.cnt=10, mono=0, depth=5, shrinkage=1,
                          bag.frac=1, distribution=infer_distro(y), ...) {

      mod <- tryCatch({

        capture.output(
          mod <- gbm::gbm.fit(
            x = data.frame(x),
            y = y,
            w = w,
            n.minobsinnode = min.cnt,
            interaction.depth = depth,
            bag.fraction = bag.frac,
            shrinkage = shrinkage,
            var.monotone = mono,
            keep.data = FALSE,
            verbose = FALSE,
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

      if (is.null(mod)) c(-Inf, Inf) else get_cuts(mod, 1L)
    },

    get_cuts = function(mod, i) {
      tree <- mod$trees[[i]]
      sort(c(-Inf, tree[[2]][tree[[1]] != -1], Inf))
    }
  )

)
