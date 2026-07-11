predict_arg_patches <- list(
  lme = function(fit, args, predict) {
    args$level <- 0
    args
  },
  merMod = function(fit, args, predict) {
    if (!("re.form" %in% names(predict))) {
      args$re.form <- NA
    }
    args
  },
  rq = function(fit, args, predict) {
    args$interval <- "confidence"
    args
  },
  svm = function(fit, args, predict) {
    args$probability <- TRUE
    args
  },
  multinom = function(fit, args, predict) {
    args$type <- "probs"
    args
  },
  polr = function(fit, args, predict) {
    args$type <- "probs"
    args
  },
  gbm = function(fit, args, predict) {
    args$n.trees <- length(fit$trees)
    args
  },
  betareg = function(fit, args, predict) {
    args$type <- c("link", "variance")
    args
  }
)

build_predict_args <- function(fit, dat, predict) {
  predict_args <- list(object = fit, newdata = dat)
  for (cls in names(predict_arg_patches)) {
    if (inherits(fit, cls)) {
      predict_args <- predict_arg_patches[[cls]](fit, predict_args, predict)
    }
  }
  if (length(predict)) {
    predict_args[names(predict)] <- predict
  }
  predict_args
}

do_predict <- function(fit, dat, se_fit, predict_args) {
  if (se_fit) {
    if (inherits(fit, "mlm")) {
      p <- list(
        fit = suppressWarnings(do.call("predict", predict_args)),
        se.fit = se_mlm(fit, newdata = dat)
      )
    } else if (inherits(fit, "randomForest") && fit$type == "classification") {
      predict_args$type <- "prob"
      p_mat <- suppressWarnings(do.call("predict", predict_args))
      p <- list(fit = p_mat[, 2], se.fit = NA)
    } else if (inherits(fit, "loess")) {
      predict_args$se <- TRUE
      p <- suppressWarnings(do.call("predict", predict_args))
    } else {
      predict_args$se.fit <- TRUE
      p <- suppressWarnings(do.call("predict", predict_args))
    }
  } else if (inherits(fit, "randomForest") && fit$type == "classification") {
    p <- predict(fit, type = "prob")[, 2]
  } else if (inherits(fit, "rq")) {
    p <- suppressWarnings(do.call("predict", predict_args))[, 1]
  } else {
    p <- suppressWarnings(do.call("predict", predict_args))
  }
  if (inherits(fit, "svm") && fit$type < 3) {
    p <- attr(p, "probabilities")
  }
  p
}

visreg_pred <- function(fit, dat, se_fit = FALSE, predict = list()) {
  if (inherits(fit, "betareg")) {
    se_fit <- FALSE
  }
  predict_args <- build_predict_args(fit, dat, predict)
  do_predict(fit, dat, se_fit, predict_args)
}
