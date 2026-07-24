# Returns a list of x, y, and z for plotting
build_visreg2d <- function(fit, f, xvar, yvar, nn, cond, type, scale, trans) {
  n_outcomes <- if (inherits(fit, "mlm")) ncol(coef(fit)) else 1
  form <- parse_formula(formula(fit)[3])
  x_vals <- f[, xvar]
  y_vals <- f[, yvar]
  x_grid <- if (is.factor(x_vals)) {
    factor(levels(x_vals), levels = levels(x_vals))
  } else {
    seq(min(x_vals), max(x_vals), length = nn)
  }
  y_grid <- if (is.factor(y_vals)) {
    factor(levels(y_vals), levels = levels(y_vals))
  } else {
    seq(min(y_vals), max(y_vals), length = nn)
  }
  grid_df <- as.data.frame(expand.grid(x_grid, y_grid))
  names(grid_df) <- c(xvar, yvar)

  if (type == "conditional") {
    filled_frame <- fill_frame(f, grid_df, cond)
    pred_frame <- model.frame(as.formula(paste("~", form)), filled_frame)
    pred_frame <- cbind(
      pred_frame,
      filled_frame[, setdiff(names(filled_frame), names(pred_frame)), drop = FALSE]
    )
    preds <- predict(fit, newdata = pred_frame)
    if (inherits(fit, "mlm")) {
      z_grid <- vector("list", n_outcomes)
      for (i in 1:n_outcomes) {
        z_grid[[i]] <- matrix(trans(preds[, i]), nrow = length(x_grid), ncol = length(y_grid))
      }
    } else {
      z_grid <- matrix(trans(preds), nrow = length(x_grid), ncol = length(y_grid))
    }
  } else if (type == "contrast") {
    x_ref <- if (is.factor(x_vals)) x_grid[1] else mean(x_vals)
    y_ref <- if (is.factor(y_vals)) y_grid[1] else mean(y_vals)
    grid_df <- rbind(c(x_ref, y_ref), grid_df)
    filled_frame <- fill_frame(f, grid_df, cond)
    stacked_frame <- rbind(f, filled_frame)
    if (inherits(fit, "mlm")) {
      finite_coef <- apply(is.finite(coef(fit)), 1, all)
      if (!identical(finite_coef, apply(is.finite(coef(fit)), 1, any))) {
        stop("Inconsistent NA/NaN coefficients across outcomes", call. = FALSE)
      }
    } else {
      finite_coef <- is.finite(coef(fit))
    }
    raw_matrix_grid <- model.matrix(as.formula(paste("~", formula(fit)[3])), stacked_frame)[
      -(seq_len(nrow(f))),
      finite_coef
    ]
    matrix_grid <- t(t(raw_matrix_grid[-1, ]) - raw_matrix_grid[1, ])
    if (inherits(fit, "mlm")) {
      z_grid <- vector("list", n_outcomes)
      for (i in 1:n_outcomes) {
        z_grid[[i]] <- matrix(
          trans(matrix_grid %*% coef(fit)[finite_coef, i]),
          nrow = length(x_grid),
          ncol = length(y_grid)
        )
      }
    } else {
      z_grid <- matrix(
        trans(matrix_grid %*% coef(fit)[finite_coef]),
        nrow = length(x_grid),
        ncol = length(y_grid)
      )
    }
  }
  var_labels <- attr(f, "var_labels")
  label_for <- function(name) {
    if (is.null(var_labels) || !(name %in% names(var_labels))) return(NA_character_)
    unname(var_labels[name])
  }
  z_label <- make_y_name(fit, scale, trans, type, var_labels)
  term_frame <- model.frame(as.formula(paste("~", form)), filled_frame)
  cond_names <- setdiff(names(term_frame), c(xvar, yvar))
  cond_names <- intersect(cond_names, names(filled_frame))
  base_meta <- list(
    x = xvar,
    x_label = label_for(xvar),
    y = yvar,
    y_label = label_for(yvar),
    trans = trans,
    class = class(fit),
    cond = term_frame[1, cond_names, drop = FALSE]
  )

  if (n_outcomes > 1) {
    result <- vector("list", n_outcomes)
    for (i in 1:n_outcomes) {
      meta <- base_meta
      meta$z <- z_label[i]
      result[[i]] <- list(x = x_grid, y = y_grid, z = z_grid[[i]], meta = meta)
      class(result[[i]]) <- "visreg2d"
    }
    class(result) <- "visreg_list"
  } else {
    meta <- base_meta
    meta$z <- z_label
    result <- list(x = x_grid, y = y_grid, z = z_grid, meta = meta)
    class(result) <- "visreg2d"
  }
  result
}
