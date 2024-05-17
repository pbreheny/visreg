setupX <- function(fit, f, name, nn, cond, ...) {
  # Set up n x p matrix for (conditional) partial residuals
  x <- f[, name]
  if (is.factor(x)) {
    xref <- 1
    if (name %in% names(cond)) {
      if (cond[[name]] %in% levels(x)) {
        xref <- which(levels(x) == cond[[name]])
      } else if (cond[[name]] %in% 1:length(levels(x))) {
        xref <- cond[[name]]
      } else {
        warning(paste0("You have specified a value for ", name, " that is not one of its levels.\n  Using reference level instead."))
      }
    }
  } else {
    if (name %in% names(cond)) {
      xref <- cond[[name]]
    } else {
      xref <- mean(x)
    }
  }
  x <- if (is.factor(x)) factor(c(xref, as.integer(x)), labels=levels(x)) else c(xref, x)
  xdf <- data.frame(x)
  names(xdf) <- name
  df <- fillFrame(f, xdf, cond)
  D <- rbind(f[, names(df)], df)
  b <- visreg_coef(fit)

  if (inherits(fit, "mlm")) {
    ind <- apply(is.finite(b), 1, all)
    if (!identical(ind, apply(is.finite(b), 1, any))) stop("Inconsistent NA/NaN coefficients across outcomes", call.=FALSE)
  } else {
    ind <- is.finite(b)
  }
  if (inherits(fit, "gam")) {
    form <- parseFormula(formula(fit)[3])
    D <- model.frame(as.formula(paste("~", form)), df)
    X. <- predict(fit, newdata=as.list(D), type="lpmatrix")
  } else if (inherits(fit, "merMod")) {
    form <- formula(fit, fixed.only = TRUE)
    RHS <- formula(substitute(~R, list(R = form[[length(form)]])))
    X. <- model.matrix(RHS, D)[-(1:nrow(f)), ind]
  } else if (inherits(fit, "glmmadmb")) {
    form <- as.formula(paste("~", as.character(fit$fixed[3])))
    X. <- model.matrix(form, D)[-(1:nrow(f)), ind]
  } else if (inherits(fit, "betareg")) {
    form <- formula(fit)[3]
    ind <- ind[-length(ind)]
    X. <- model.matrix(as.formula(paste("~", form)), D)[-(1:nrow(f)), ind]
  } else if (inherits(fit, 'glmmTMB')) {
    form <- lme4::nobars(formula(fit))[3]
    X. <- model.matrix(as.formula(paste("~", form)), D)[-(1:nrow(f)), ind]
  } else {
    form <- formula(fit)[3]
    X. <- model.matrix(as.formula(paste("~", form)), D)[-(1:nrow(f)), ind]
  }
  X <- t(t(X.[-1,]) - X.[1,])

  ## Set up data frame with nn rows for prediction
  dots <- list(...)
  if (is.factor(x)) {
    xx <- factor(c(xref, 1:length(levels(x))), labels=levels(x))
  } else {
    if ('xtrans' %in% names(dots)) {
      xx <- c(xref, seq(min(x), max(x), length=nn))
      fi <- approxfun(dots$xtrans(x), x)
      xx <- seq(dots$xtrans(min(x)), dots$xtrans(max(x)), len=nn) |> fi()
    } else {
      xx <- c(xref, seq(min(x), max(x), length=nn))
    }
  }
  xxdf <- data.frame(xx)
  names(xxdf) <- name
  df <- fillFrame(f, xxdf, cond)
  DD <- rbind(f[, names(df)], df)
  if (inherits(fit, "gam")) {
    DD <- model.frame(as.formula(paste("~", form)), df)
    XX. <- predict(fit, newdata=as.list(DD), type="lpmatrix")
  } else if (inherits(fit, "merMod")) {
    XX. <- model.matrix(RHS, DD)[-(1:nrow(f)), ind]
  } else XX. <- model.matrix(as.formula(paste("~", form)), DD)[-(1:nrow(f)), ind]
  XX <- t(t(XX.[-1,])-XX.[1,])

  ## Remove extraneous columns for coxph
  if (inherits(fit, "coxph")) {
    remove.xx <- c(grep("(Intercept)", colnames(XX), fixed=TRUE),
                   grep("strata(", colnames(XX), fixed=TRUE),
                   grep("cluster(", colnames(XX), fixed=TRUE))
    remove.x <- c(grep("(Intercept)", colnames(X), fixed=TRUE),
                  grep("strata(", colnames(XX), fixed=TRUE),
                  grep("cluster(", colnames(X), fixed=TRUE))
    XX <- XX[, -remove.xx, drop=FALSE]
    X <- X[, -remove.xx, drop=FALSE]
  } else if (inherits(fit, "polr")) {
    remove.xx <- grep("(Intercept)", colnames(XX), fixed=TRUE)
    remove.x <- grep("(Intercept)", colnames(X), fixed=TRUE)
    XX <- XX[, -remove.xx, drop=FALSE]
    X <- X[, -remove.xx, drop=FALSE]
  }
  condNames <- names(model.frame(as.formula(paste("~", parseFormula(formula(fit)[3]))), df))
  condNames <- setdiff(condNames, name)
  condNames <- intersect(condNames, names(df))
  return(list(x=x[-1], xx=xx[-1], X=X, XX=XX, factor=is.factor(x), name=name, cond=df[1, condNames, drop=FALSE]))
}
