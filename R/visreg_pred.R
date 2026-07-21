# All model class-specific adaptations/patches go here:
# - `args` to patch the arguments passed to predict()
# - `call` if the results need to be reshaped
predict_adapters <- list(
  mlm = list(call = function(fit, args, se_fit) {
    if (!se_fit) {
      return(default_predict_call(fit, args, se_fit))
    }
    list(
      fit = suppressWarnings(do.call("predict", args)),
      se.fit = se_mlm(fit, newdata = args$newdata)
    )
  }),
  randomForest = list(call = function(fit, args, se_fit) {
    if (fit$type != "classification") {
      return(default_predict_call(fit, args, se_fit))
    }
    args$type <- "prob"
    p_mat <- suppressWarnings(do.call("predict", args))
    if (se_fit) list(fit = p_mat[, 2], se.fit = NA) else p_mat[, 2]
  }),
  loess = list(call = function(fit, args, se_fit) {
    if (se_fit) {
      args$se <- TRUE
    }
    suppressWarnings(do.call("predict", args))
  }),
  rq = list(
    args = function(fit, args, predict) {
      args$interval <- "confidence"
      args
    },
    call = function(fit, args, se_fit) {
      p <- suppressWarnings(do.call("predict", args))
      if (se_fit) p else p[, 1]
    }
  ),
  svm = list(
    args = function(fit, args, predict) {
      args$probability <- TRUE
      args
    },
    call = function(fit, args, se_fit) {
      p <- default_predict_call(fit, args, se_fit)
      if (fit$type < 3) attr(p, "probabilities") else p
    }
  ),
  lme = list(args = function(fit, args, predict) {
    args$level <- 0
    args
  }),
  merMod = list(args = function(fit, args, predict) {
    if (!("re.form" %in% names(predict))) {
      args$re.form <- NA
    }
    args
  }),
  multinom = list(args = function(fit, args, predict) {
    args$type <- "probs"
    args
  }),
  polr = list(args = function(fit, args, predict) {
    args$type <- "probs"
    args
  }),
  gbm = list(args = function(fit, args, predict) {
    args$n.trees <- length(fit$trees)
    args
  }),
  betareg = list(
    args = function(fit, args, predict) {
      args$type <- c("link", "variance")
      args
    },
    disable_se = TRUE
  )
)

default_predict_call <- function(fit, args, se_fit) {
  if (se_fit) {
    args$se.fit <- TRUE
  }
  suppressWarnings(do.call("predict", args))
}

visreg_pred <- function(fit, dat, se_fit = FALSE, predict = list()) {
  adapters <- predict_adapters[vapply(
    names(predict_adapters),
    function(cls) inherits(fit, cls),
    logical(1)
  )]

  # For models where passing se.fit produces an error
  if (any(vapply(adapters, function(a) isTRUE(a$disable_se), logical(1)))) {
    se_fit <- FALSE
  }

  # Set up model-specific and user-passed arguments
  args <- list(object = fit, newdata = dat)
  for (adapter in adapters) {
    if (!is.null(adapter$args)) args <- adapter$args(fit, args, predict)
  }
  if (length(predict)) {
    args[names(predict)] <- predict
  }

  # If model needs a special call, use that, otherwise default
  call_adapter <- Find(function(a) !is.null(a$call), adapters)
  call_fn <- if (!is.null(call_adapter)) call_adapter$call else default_predict_call
  call_fn(fit, args, se_fit)
}
