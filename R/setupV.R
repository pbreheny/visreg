# v is a list of three elements: fit, res, and meta
# alternatively (class "visregList"), a list of visreg elements

setupV <- function(fit, f, xvar, nn, cond, type, trans, alpha, jitter, by, yName, ...) {
  
  # Initial setup
  if (length(xvar) > 1 & length(cond) > 1) stop("Cannot specify 'by' and multiple x variables simultaneously", call.=FALSE)
  J <- max(length(xvar), length(cond))
  Attempt <- try(max(attr(terms(as.formula(formula(fit))), "order")) > 1, silent=TRUE)
  hasInteraction <- ifelse(inherits(Attempt, 'try-error'), FALSE, Attempt)
  lev <- attr(cond, "lev")

  # Get xy list
  xy <- vector("list", J)
  for (j in 1:J) {
    cond.j <- if (length(cond) > 1) cond[[j]] else cond[[1]]
    name <- if (length(xvar) > 1) xvar[j] else xvar
    xy[[j]] <- getXY(fit, f, name, nn, cond.j, type, trans, alpha, jitter, ...)
  }
  if (!missing(by)) xy <- subsetV(xy, f, by, lev, type)

  # Format
  meta <- list(x=xvar, y=xy[[1]]$y$name, hasInteraction=hasInteraction, yName=yName, trans=trans, class=class(fit))
  K <- xy[[1]]$y$n
  if (K==1) {
    if (!missing(by)) {
      meta$by <- by
      v <- list(fit=NULL, res=NULL, meta=meta)
      for (j in 1:length(xy)) {
        fit.j <- data.frame(xy[[j]]$x$DD, visregFit=xy[[j]]$y$fit, visregLwr=xy[[j]]$y$lwr, visregUpr=xy[[j]]$y$upr)
        res.j <- data.frame(xy[[j]]$x$D, visregRes=xy[[j]]$y$r, visregPos=xy[[j]]$y$pos)
        fit.j[, xvar] <- xy[[j]]$x$xx
        res.j[, xvar] <- xy[[j]]$x$x
        v$fit <- rbind(v$fit, fit.j)
        v$res <- rbind(v$res, res.j)
      }
      class(v) <- "visreg"
    } else {
      v <- vector("list", J)
      for (j in 1:J) {
        meta.j <- meta
        meta.j$x <- xvar[j]
        v[[j]] <- list(fit=data.frame(xy[[j]]$x$DD, visregFit=xy[[j]]$y$fit, visregLwr=xy[[j]]$y$lwr, visregUpr=xy[[j]]$y$upr),
                       res=data.frame(xy[[j]]$x$D, visregRes=xy[[j]]$y$r, visregPos=xy[[j]]$y$pos),
                       meta=meta.j)
        v[[j]]$fit[, xvar[j]] <- xy[[j]]$x$xx
        v[[j]]$res[, xvar[j]] <- xy[[j]]$x$x
        class(v[[j]]) <- "visreg"
      }
      if (J==1) {
        v <- v[[1]]
      } else {
        class(v) <- "visregList"
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
        v[[k]] <- list(fit=NULL, res=NULL, meta=meta.k)
        for (j in 1:J) {
          fit.jk <- data.frame(xy[[j]]$x$DD, visregFit=xy[[j]]$y$fit[,k], visregLwr=xy[[j]]$y$lwr[,k], visregUpr=xy[[j]]$y$upr[,k])
          res.jk <- data.frame(xy[[j]]$x$D, visregRes=xy[[j]]$y$r[,k], visregPos=xy[[j]]$y$pos[,k])
          fit.jk[, xvar] <- xy[[j]]$x$xx
          res.jk[, xvar] <- xy[[j]]$x$x
          v[[k]]$fit <- rbind(v[[k]]$fit, fit.jk)
          v[[k]]$res <- rbind(v[[k]]$res, res.jk)
        }
        class(v[[k]]) <- "visreg"
      }
      class(v) <- "visregList"
    } else {
      v <- vector("list", J*K)

      for (j in 1:J) {
        for (k in 1:K) {
          meta.jk <- meta
          meta.jk$x <- meta$x[j]
          meta.jk$y <- meta$y[k]
          meta.jk$yName <- meta$yName[k]
          l <- (j-1)*K + k
          v[[l]] <- list(fit=data.frame(xy[[j]]$x$DD, visregFit=xy[[j]]$y$fit[,k], visregLwr=xy[[j]]$y$lwr[,k], visregUpr=xy[[j]]$y$upr[,k]),
                         res=data.frame(xy[[j]]$x$D, visregRes=xy[[j]]$y$r[,k], visregPos=xy[[j]]$y$pos[,k]),
                         meta=meta.jk)
          v[[l]]$fit[, xvar[j]] <- xy[[j]]$x$xx
          v[[l]]$res[, xvar[j]] <- xy[[j]]$x$x
          class(v[[l]]) <- "visreg"
        }
      }
      class(v) <- "visregList"
    }
  }
  v
}
