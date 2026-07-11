setup_contrast_data <- function(fit, f, name, nn, cond) {
  x_res <- f[, name]

  # Setup reference value (baseline the contrast is measured against)
  if (is.factor(x_res)) {
    x_ref <- 1
    if (name %in% names(cond)) {
      if (cond[[name]] %in% levels(x_res)) {
        x_ref <- which(levels(x_res) == cond[[name]])
      } else if (cond[[name]] %in% seq_along(levels(x_res))) {
        x_ref <- cond[[name]]
      } else {
        warning(paste0(
          "You have specified a value for ",
          name,
          " that is not one of its levels.\n  Using reference level instead."
        ))
      }
    }
  } else {
    if (name %in% names(cond)) {
      x_ref <- cond[[name]]
    } else {
      x_ref <- mean(x_res)
    }
  }

  # Residual design matrix (n rows)
  x_res <- if (is.factor(x_res)) {
    factor(c(x_ref, as.integer(x_res)), labels = levels(x_res))
  } else {
    c(x_ref, x_res)
  }
  x_res_df <- data.frame(x_res)
  names(x_res_df) <- name
  frame_res <- fill_frame(f, x_res_df, cond)
  # Stack the original data on top of the reference + residual rows so that
  # model.matrix() derives contrasts/bases (e.g. spline knots) from the full
  # data, then below we discard the original rows and keep only the tail.
  stacked_frame_res <- rbind(f[, names(frame_res)], frame_res)
  coefs <- visreg_coef(fit)

  # Various special cases
  if (inherits(fit, "mlm")) {
    finite_coef <- apply(is.finite(coefs), 1, all)
    if (!identical(finite_coef, apply(is.finite(coefs), 1, any))) {
      stop("Inconsistent NA/NaN coefficients across outcomes", call. = FALSE)
    }
  } else {
    finite_coef <- is.finite(coefs)
  }
  if (inherits(fit, "gam")) {
    form <- parse_formula(formula(fit)[3])
    stacked_frame_res <- model.frame(as.formula(paste("~", form)), frame_res)
    raw_matrix_res <- predict(fit, newdata = as.list(stacked_frame_res), type = "lpmatrix")
  } else if (inherits(fit, "merMod")) {
    form <- formula(fit, fixed.only = TRUE)
    fixed_rhs <- formula(substitute(~R, list(R = form[[length(form)]])))
    raw_matrix_res <- model.matrix(fixed_rhs, stacked_frame_res)[-(seq_len(nrow(f))), finite_coef]
  } else if (inherits(fit, "glmmadmb")) {
    form <- as.formula(paste("~", as.character(fit$fixed[3])))
    raw_matrix_res <- model.matrix(form, stacked_frame_res)[-(seq_len(nrow(f))), finite_coef]
  } else if (inherits(fit, "betareg")) {
    form <- formula(fit)[3]
    finite_coef <- finite_coef[-length(finite_coef)]
    raw_matrix_res <- model.matrix(as.formula(paste("~", form)), stacked_frame_res)[
      -(seq_len(nrow(f))),
      finite_coef
    ]
  } else if (inherits(fit, "glmmTMB")) {
    form <- lme4::nobars(formula(fit))[3]
    raw_matrix_res <- model.matrix(as.formula(paste("~", form)), stacked_frame_res)[
      -(seq_len(nrow(f))),
      finite_coef
    ]
  } else {
    form <- formula(fit)[3]
    raw_matrix_res <- model.matrix(as.formula(paste("~", form)), stacked_frame_res)[
      -(seq_len(nrow(f))),
      finite_coef
    ]
  }
  matrix_res <- t(t(raw_matrix_res[-1, ]) - raw_matrix_res[1, ])

  # Fit design matrix (nn rows)
  if (is.factor(x_res)) {
    x_fit <- factor(c(x_ref, seq_along(levels(x_res))), labels = levels(x_res))
  } else {
    x_fit <- c(x_ref, seq(min(x_res), max(x_res), length = nn))
  }
  x_fit_df <- data.frame(x_fit)
  names(x_fit_df) <- name
  frame_fit <- fill_frame(f, x_fit_df, cond)
  stacked_frame_fit <- rbind(f[, names(frame_fit)], frame_fit)
  if (inherits(fit, "gam")) {
    stacked_frame_fit <- model.frame(as.formula(paste("~", form)), frame_fit)
    raw_matrix_fit <- predict(fit, newdata = as.list(stacked_frame_fit), type = "lpmatrix")
  } else if (inherits(fit, "merMod")) {
    raw_matrix_fit <- model.matrix(fixed_rhs, stacked_frame_fit)[-(seq_len(nrow(f))), finite_coef]
  } else {
    raw_matrix_fit <- model.matrix(as.formula(paste("~", form)), stacked_frame_fit)[
      -(seq_len(nrow(f))),
      finite_coef
    ]
  }
  matrix_fit <- t(t(raw_matrix_fit[-1, ]) - raw_matrix_fit[1, ])

  # Remove extraneous columns for coxph/polr
  if (inherits(fit, "coxph")) {
    remove_cols <- c(
      grep("(Intercept)", colnames(matrix_fit), fixed = TRUE),
      grep("strata(", colnames(matrix_fit), fixed = TRUE),
      grep("cluster(", colnames(matrix_fit), fixed = TRUE)
    )
    matrix_fit <- matrix_fit[, -remove_cols, drop = FALSE]
    matrix_res <- matrix_res[, -remove_cols, drop = FALSE]
  } else if (inherits(fit, "polr")) {
    remove_cols <- grep("(Intercept)", colnames(matrix_fit), fixed = TRUE)
    matrix_fit <- matrix_fit[, -remove_cols, drop = FALSE]
    matrix_res <- matrix_res[, -remove_cols, drop = FALSE]
  }

  # Conditioning values for the other covariates
  cond_names <- names(model.frame(
    as.formula(paste("~", parse_formula(formula(fit)[3]))),
    frame_fit
  ))
  cond_names <- setdiff(cond_names, name)
  cond_names <- intersect(cond_names, names(frame_fit))

  list(
    x_res = x_res[-1],
    x_fit = x_fit[-1],
    matrix_res = matrix_res,
    matrix_fit = matrix_fit,
    factor = is.factor(x_res),
    name = name,
    cond = frame_fit[1, cond_names, drop = FALSE]
  )
}
