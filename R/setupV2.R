# setupV for visreg2d
# Returns a list of x, y, and z for plotting
setupV2 <- function(fit, f, xvar, yvar, nn, cond, type, scale, trans) {
  n.z <- if (class(fit)[1]=="mlm") ncol(coef(fit)) else 1
  form <- parseFormula(formula(fit)[3])
  x <- f[, xvar]
  y <- f[, yvar]
  xx <- if(is.factor(x)) factor(levels(x), levels=levels(x)) else seq(min(x), max(x), length=nn)
  yy <- if(is.factor(y)) factor(levels(y), levels=levels(y)) else seq(min(y), max(y), length=nn)
  xydf <- as.data.frame(expand.grid(xx, yy))
  names(xydf) <- c(xvar, yvar)

  if (type=="conditional") {
    df <- fillFrame(f, xydf, cond)
    DD <- model.frame(as.formula(paste("~", form)), df)
    DD <- cbind(DD, df[, setdiff(names(df), names(DD)), drop=FALSE])
    P <- predict(fit, newdata=DD)
    if (class(fit)[1]=="mlm") {
      z <- vector("list", n.z)
      for (i in 1:n.z) z[[i]] <- matrix(trans(P[,i]), nrow=length(xx), ncol=length(yy))
    } else {
      z <- matrix(trans(P), nrow=length(xx), ncol=length(yy))
    }
  } else if (type=="contrast") {
    xref <- if(is.factor(x)) xx[1] else xref <- mean(x)
    yref <- if(is.factor(y)) yy[1] else yref <- mean(y)
    xydf <- rbind(c(xref, yref), xydf)
    df <- fillFrame(f, xydf, cond)
    DD <- rbind(f, df)
    if (class(fit)[1]=="mlm") {
      ind <- apply(is.finite(coef(fit)), 1, all)
      if (!identical(ind, apply(is.finite(coef(fit)), 1, any))) stop("Inconsistent NA/NaN coefficients across outcomes")
    } else ind <- is.finite(coef(fit))
    XX. <- model.matrix(as.formula(paste("~", formula(fit)[3])), DD)[-(1:nrow(f)), ind]
    XX <- t(t(XX.[-1,])-XX.[1,])
    if (class(fit)[1]=="mlm") {
      z <- vector("list", n.z)
      for (i in 1:n.z) z[[i]] <- matrix(trans(XX%*%coef(fit)[ind, i]), nrow=length(xx), ncol=length(yy))
    } else {
      z <- matrix(trans(XX%*%coef(fit)[ind]), nrow=length(xx), ncol=length(yy))
    }
  }
  #zname <- if (class(fit)[1]=="mlm") colnames(fit$fitted.values) else as.character(formula(fit)[2])
  zname <- makeYName(fit, scale, trans, type)
  D <- model.frame(as.formula(paste("~", form)), df)
  condNames <- setdiff(names(D), c(xvar, yvar))
  condNames <- intersect(condNames, names(df))
  baseMeta <- list(x=xvar, y=yvar, trans=trans, class=class(fit), cond=D[1, condNames, drop=FALSE])

  if (n.z > 1) {
    v <- vector("list", n.z)
    for (i in 1:n.z) {
      meta <- baseMeta
      meta$z <- zname[i]
      v[[i]] <- list(x=xx, y=yy, z=z[[i]], meta=meta)
      class(v[[i]]) <- 'visreg2d'
    }
    class(v) <- 'visregList'
  } else {
    meta <- baseMeta
    meta$z <- zname
    v <- list(x=xx, y=yy, z=z, meta=meta)
    class(v) <- 'visreg2d'
  }
  v
}
