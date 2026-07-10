# v is a list of three elements: fit, res, and meta
# alternatively (class "visreg_list"), a list of visreg elements

build_visreg <- function(fit, f, xvar, nn, cond, type, trans, alpha, jitter, by, yName, ...) {
  # Initial setup
  if (length(xvar) > 1 && length(cond) > 1) {
    stop("Cannot specify 'by' and multiple x variables simultaneously", call. = FALSE)
  }
  J <- max(length(xvar), length(cond))
  # Variables explicitly addressed via 'by' or 'cond' (same for every element of 'cond')
  covered <- if (length(cond) >= 1) names(cond[[1]]) else character(0)
  interacting <- setNames(lapply(xvar, interacting_vars, fit = fit), xvar)
  has_interaction <- vapply(interacting, function(iv) length(iv) > 0, logical(1))
  main_effect_warn <- vapply(interacting, function(iv) length(setdiff(iv, covered)) > 0, logical(1))
  lev <- attr(cond, "lev")

  # Get xy list
  xy <- vector("list", J)
  for (j in 1:J) {
    cond.j <- if (length(cond) > 1) cond[[j]] else cond[[1]]
    name <- if (length(xvar) > 1) xvar[j] else xvar
    xy[[j]] <- get_xy(fit, f, name, nn, cond.j, type, trans, alpha, jitter, ...)
  }
  if (!missing(by)) {
    xy <- subset_visreg(xy, f, by, lev, type)
  }

  # Format
  meta <- list(
    x = xvar,
    y = xy[[1]]$y$name,
    has_interaction = unname(has_interaction[xvar[1]]),
    main_effect_warn = unname(main_effect_warn[xvar[1]]),
    yName = yName,
    trans = trans,
    class = class(fit)
  )
  K <- xy[[1]]$y$n
  if (K == 1) {
    if (!missing(by)) {
      meta$by <- by
      v <- list(fit = NULL, res = NULL, meta = meta)
      for (j in seq_along(xy)) {
        fit.j <- data.frame(
          xy[[j]]$x$DD,
          visreg_fit = xy[[j]]$y$fit,
          visreg_lwr = xy[[j]]$y$lwr,
          visreg_upr = xy[[j]]$y$upr
        )
        res_j <- data.frame(xy[[j]]$x$D, visreg_res = xy[[j]]$y$r, visreg_pos = xy[[j]]$y$pos)
        fit.j[, xvar] <- xy[[j]]$x$xx
        res_j[, xvar] <- xy[[j]]$x$x
        v$fit <- rbind(v$fit, fit.j)
        v$res <- rbind(v$res, res_j)
      }
      class(v) <- "visreg"
    } else {
      v <- vector("list", J)
      for (j in 1:J) {
        meta.j <- meta
        meta.j$x <- xvar[j]
        meta.j$has_interaction <- unname(has_interaction[xvar[j]])
        meta.j$main_effect_warn <- unname(main_effect_warn[xvar[j]])
        v[[j]] <- list(
          fit = data.frame(
            xy[[j]]$x$DD,
            visreg_fit = xy[[j]]$y$fit,
            visreg_lwr = xy[[j]]$y$lwr,
            visreg_upr = xy[[j]]$y$upr
          ),
          res = data.frame(xy[[j]]$x$D, visreg_res = xy[[j]]$y$r, visreg_pos = xy[[j]]$y$pos),
          meta = meta.j
        )
        v[[j]]$fit[, xvar[j]] <- xy[[j]]$x$xx
        v[[j]]$res[, xvar[j]] <- xy[[j]]$x$x
        class(v[[j]]) <- "visreg"
      }
      if (J == 1) {
        v <- v[[1]]
      } else {
        class(v) <- "visreg_list"
      }
    }
  } else {
    if (!missing(by)) {
      meta$by <- by
      v <- vector("list", K)
      for (k in 1:K) {
        meta.k <- meta
        meta.k$y <- meta$y[k]
        meta.k$yName <- meta$yName[k]
        v[[k]] <- list(fit = NULL, res = NULL, meta = meta.k)
        for (j in 1:J) {
          fit.jk <- data.frame(
            xy[[j]]$x$DD,
            visreg_fit = xy[[j]]$y$fit[, k],
            visreg_lwr = xy[[j]]$y$lwr[, k],
            visreg_upr = xy[[j]]$y$upr[, k]
          )
          res_jk <- data.frame(
            xy[[j]]$x$D,
            visreg_res = xy[[j]]$y$r[, k],
            visreg_pos = xy[[j]]$y$pos[, k]
          )
          fit.jk[, xvar] <- xy[[j]]$x$xx
          res_jk[, xvar] <- xy[[j]]$x$x
          v[[k]]$fit <- rbind(v[[k]]$fit, fit.jk)
          v[[k]]$res <- rbind(v[[k]]$res, res_jk)
        }
        class(v[[k]]) <- "visreg"
      }
      class(v) <- "visreg_list"
    } else {
      v <- vector("list", J * K)

      for (j in 1:J) {
        for (k in 1:K) {
          meta.jk <- meta
          meta.jk$x <- meta$x[j]
          meta.jk$y <- meta$y[k]
          meta.jk$yName <- meta$yName[k]
          meta.jk$has_interaction <- unname(has_interaction[meta$x[j]])
          meta.jk$main_effect_warn <- unname(main_effect_warn[meta$x[j]])
          l <- (j - 1) * K + k
          v[[l]] <- list(
            fit = data.frame(
              xy[[j]]$x$DD,
              visreg_fit = xy[[j]]$y$fit[, k],
              visreg_lwr = xy[[j]]$y$lwr[, k],
              visreg_upr = xy[[j]]$y$upr[, k]
            ),
            res = data.frame(
              xy[[j]]$x$D,
              visreg_res = xy[[j]]$y$r[, k],
              visreg_pos = xy[[j]]$y$pos[, k]
            ),
            meta = meta.jk
          )
          v[[l]]$fit[, xvar[j]] <- xy[[j]]$x$xx
          v[[l]]$res[, xvar[j]] <- xy[[j]]$x$x
          class(v[[l]]) <- "visreg"
        }
      }
      class(v) <- "visreg_list"
    }
  }
  v
}
