Response <- function(fit, x, trans, alpha, ...) {
  ## Calculate predictions, partial residuals
  if ("randomForest" %in% class(fit)) {
    if (fit$type=="regression") rr <- fit$y - fit$predicted
    if (fit$type=="classification") {
      P <- predict(fit, type="prob")
      rr <- (fit$y==colnames(P)[2]) - P[,2]
    }
  } else {
    rr <- residuals(fit)
  }
  if (class(fit)[1]!="mlm") rr <- rr[!is.na(rr)]
  nr <- if (is.matrix(rr)) nrow(rr) else length(rr)
  if (nrow(x$D) != nr) warning("Residuals do not match data; have you changed the original data set?  If so, visreg is probably not displaying the residuals for the data set that was actually used to fit the model.")
  predict.args <- list(object=fit, newdata=x$D)
  if ("lme" %in% class(fit)) predict.args$level <- 0
  if (inherits(fit, "merMod")) predict.args$re.form <- NA
  dots <- list(...)
  if (length(dots)) predict.args[names(dots)] <- dots
  if ("randomForest" %in% class(fit) && fit$type=="classification") {
    r <- P[,2]+rr
  } else {
    r <- suppressWarnings(do.call("predict", predict.args)) + rr
  }
  predict.args$newdata <- x$DD
  if (class(fit)[1]=="mlm") {
    p <- list(fit = suppressWarnings(do.call("predict", predict.args)), se.fit = se.mlm(fit, newdata=x$DD))
  } else if ("randomForest" %in% class(fit) && fit$type=="classification") {
    predict.args$type <- "prob"
    P <- suppressWarnings(do.call("predict", predict.args))
    p <- list(fit=P[,2], se.fit=NA)
  } else {
    predict.args$se.fit <- TRUE ## note: se.fit required by some; add $se on case-by-case basis
    p <- suppressWarnings(do.call("predict", predict.args))    
  }
  
  ## Format output
  if (class(p)=="svystat") p <- list(fit=as.numeric(p), se.fit=sqrt(attr(p,"var")))
  if ("rms" %in% class(fit)) p$fit <- p$linear.predictors
  if (is.numeric(p)) p <- list(fit=p, se.fit=NA)
  m <- ifelse(identical(class(fit),"lm"),qt(1-alpha/2,fit$df.residual),qnorm(1-alpha/2))
  upr <- p$fit + m*p$se.fit
  lwr <- p$fit - m*p$se.fit
  if (class(fit)[1]=="mlm") {
    val <- list(fit=matrix(trans(p$fit), ncol=ncol(p$fit)), lwr=matrix(trans(lwr), ncol=ncol(p$fit)), upr=matrix(trans(upr), ncol=ncol(p$fit)), r=matrix(trans(r), ncol=ncol(p$fit)))
    val$name <- colnames(val$fit) <- colnames(p$fit)
  } else {
    val <- list(fit=as.numeric(trans(p$fit)), lwr=as.numeric(trans(lwr)), upr=as.numeric(trans(upr)), r=as.numeric(trans(r)), name=as.character(formula(fit)[2]))
  }
  val$pos <- rr>0
  val$n <- if (class(fit)[1]=="mlm") ncol(p$fit) else 1
  val
}
