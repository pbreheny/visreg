# v is a list of three elements: fit, res, and meta
# alternatively (class "visreg_list"), a list of visreg elements

build_visreg <- function(fit, f, xvar, nn, cond, type, trans, alpha, by, y_name, predict) {
  # Initial setup
  if (length(xvar) > 1 && length(cond) > 1) {
    stop("Cannot specify 'by' and multiple x variables simultaneously", call. = FALSE)
  }
  n_cond <- max(length(xvar), length(cond))
  # Variables explicitly addressed via 'by' or 'cond' (same for every element of 'cond')
  covered <- if (length(cond) >= 1) names(cond[[1]]) else character(0)
  interacting <- setNames(lapply(xvar, interacting_vars, fit = fit), xvar)
  has_interaction <- vapply(interacting, function(iv) length(iv) > 0, logical(1))
  main_effect_warn <- vapply(interacting, function(iv) length(setdiff(iv, covered)) > 0, logical(1))
  lev <- attr(cond, "lev")

  # Get xy list
  xy <- vector("list", n_cond)
  for (j in 1:n_cond) {
    cond_j <- if (length(cond) > 1) cond[[j]] else cond[[1]]
    name <- if (length(xvar) > 1) xvar[j] else xvar
    xy[[j]] <- get_xy(fit, f, name, nn, cond_j, type, trans, alpha, predict)
  }
  if (!missing(by)) {
    xy <- subset_xy(xy, f, by, lev, type)
  }

  # Format
  meta <- list(
    x = xvar,
    y = xy[[1]]$y$name,
    has_interaction = unname(has_interaction[xvar[1]]),
    main_effect_warn = unname(main_effect_warn[xvar[1]]),
    y_name = y_name,
    trans = trans,
    class = class(fit)
  )
  n_y <- xy[[1]]$y$n_y
  if (n_y == 1) {
    if (!missing(by)) {
      meta$by <- by
      v <- list(fit = NULL, res = NULL, meta = meta)
      for (j in seq_along(xy)) {
        fit_j <- data.frame(
          xy[[j]]$x$frame_fit,
          visreg_fit = xy[[j]]$y$fit,
          visreg_lwr = xy[[j]]$y$lwr,
          visreg_upr = xy[[j]]$y$upr
        )
        res_j <- data.frame(
          xy[[j]]$x$frame_res,
          visreg_res = xy[[j]]$y$r,
          visreg_pos = xy[[j]]$y$pos
        )
        fit_j[, xvar] <- xy[[j]]$x$x_fit
        res_j[, xvar] <- xy[[j]]$x$x_res
        v$fit <- rbind(v$fit, fit_j)
        v$res <- rbind(v$res, res_j)
      }
      class(v) <- "visreg"
    } else {
      v <- vector("list", n_cond)
      for (j in 1:n_cond) {
        meta_j <- meta
        meta_j$x <- xvar[j]
        meta_j$has_interaction <- unname(has_interaction[xvar[j]])
        meta_j$main_effect_warn <- unname(main_effect_warn[xvar[j]])
        v[[j]] <- list(
          fit = data.frame(
            xy[[j]]$x$frame_fit,
            visreg_fit = xy[[j]]$y$fit,
            visreg_lwr = xy[[j]]$y$lwr,
            visreg_upr = xy[[j]]$y$upr
          ),
          res = data.frame(
            xy[[j]]$x$frame_res,
            visreg_res = xy[[j]]$y$r,
            visreg_pos = xy[[j]]$y$pos
          ),
          meta = meta_j
        )
        v[[j]]$fit[, xvar[j]] <- xy[[j]]$x$x_fit
        v[[j]]$res[, xvar[j]] <- xy[[j]]$x$x_res
        class(v[[j]]) <- "visreg"
      }
      if (n_cond == 1) {
        v <- v[[1]]
      } else {
        class(v) <- "visreg_list"
      }
    }
  } else {
    if (!missing(by)) {
      meta$by <- by
      v <- vector("list", n_y)
      for (k in 1:n_y) {
        meta_k <- meta
        meta_k$y <- meta$y[k]
        meta_k$y_name <- meta$y_name[k]
        v[[k]] <- list(fit = NULL, res = NULL, meta = meta_k)
        for (j in 1:n_cond) {
          fit_jk <- data.frame(
            xy[[j]]$x$frame_fit,
            visreg_fit = xy[[j]]$y$fit[, k],
            visreg_lwr = xy[[j]]$y$lwr[, k],
            visreg_upr = xy[[j]]$y$upr[, k]
          )
          res_jk <- data.frame(
            xy[[j]]$x$frame_res,
            visreg_res = xy[[j]]$y$r[, k],
            visreg_pos = xy[[j]]$y$pos[, k]
          )
          fit_jk[, xvar] <- xy[[j]]$x$x_fit
          res_jk[, xvar] <- xy[[j]]$x$x_res
          v[[k]]$fit <- rbind(v[[k]]$fit, fit_jk)
          v[[k]]$res <- rbind(v[[k]]$res, res_jk)
        }
        class(v[[k]]) <- "visreg"
      }
      class(v) <- "visreg_list"
    } else {
      v <- vector("list", n_cond * n_y)

      for (j in 1:n_cond) {
        for (k in 1:n_y) {
          meta_jk <- meta
          meta_jk$x <- meta$x[j]
          meta_jk$y <- meta$y[k]
          meta_jk$y_name <- meta$y_name[k]
          meta_jk$has_interaction <- unname(has_interaction[meta$x[j]])
          meta_jk$main_effect_warn <- unname(main_effect_warn[meta$x[j]])
          l <- (j - 1) * n_y + k
          v[[l]] <- list(
            fit = data.frame(
              xy[[j]]$x$frame_fit,
              visreg_fit = xy[[j]]$y$fit[, k],
              visreg_lwr = xy[[j]]$y$lwr[, k],
              visreg_upr = xy[[j]]$y$upr[, k]
            ),
            res = data.frame(
              xy[[j]]$x$frame_res,
              visreg_res = xy[[j]]$y$r[, k],
              visreg_pos = xy[[j]]$y$pos[, k]
            ),
            meta = meta_jk
          )
          v[[l]]$fit[, xvar[j]] <- xy[[j]]$x$x_fit
          v[[l]]$res[, xvar[j]] <- xy[[j]]$x$x_res
          class(v[[l]]) <- "visreg"
        }
      }
      class(v) <- "visreg_list"
    }
  }
  v
}
