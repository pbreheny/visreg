## To-do: add too.far option?
visreg2d <- function(fit,xvar,yvar,type=c("conditional","effect"),nn=ifelse(plot.type=="persp",49,99),plot.type=c("image","persp","rgl"),trans=as.numeric,scale=c("linear","response"), cond=list(), whitespace=0.2, ...)
{
  type <- match.arg(type)
  scale <- match.arg(scale)
  plot.type <- match.arg(plot.type)
  if (scale=="response") trans <- family(fit)$linkinv
  if (missing(xvar) | missing(yvar)) stop("Must specify and x and y variable")
  
  ## Set up f
  f <- setupF(fit)
  if (!is.element(xvar,names(f))) stop(paste(xvar,"not in model"))
  if (!is.element(yvar,names(f))) stop(paste(yvar,"not in model"))
  if (attr(f,"needs.update")) fit <- update(fit,data=f)
  cond <- setupCond(cond,f)[[1]]
  
  n.y <- if (class(fit)[1]=="mlm") ncol(coef(fit)) else 1
  
  ## Set up x,y for prediction
  form <- removeFormulaFormatting(formula(fit)[3])
  x <- f[,xvar]
  y <- f[,yvar]
  xx <- if(is.factor(x)) factor(levels(x),levels=levels(x)) else seq(min(x),max(x),length=nn)
  yy <- if(is.factor(y)) factor(levels(y),levels=levels(y)) else seq(min(y),max(y),length=nn)
  xydf <- as.data.frame(expand.grid(xx,yy))
  names(xydf) <- c(xvar,yvar)
  if (type=="conditional")
  {
    df <- fillFrame(f,xydf,cond)
    DD <- model.frame(as.formula(paste("~",form)),df)
    DD <- cbind(DD,df[,setdiff(names(df),names(DD)),drop=FALSE])
    P <- predict(fit,newdata=DD)
    if (class(fit)[1]=="mlm") {
      Z <- vector("list", n.y)
      for (i in 1:n.y) Z[[i]] <- matrix(trans(P[,i]), nrow=length(xx), ncol=length(yy))
      v <- list(DD=DD, z=Z)
    } else {
      z <- matrix(trans(P), nrow=length(xx), ncol=length(yy))
      v <- list(DD=DD, z=z)
    }
  }
  else if (type=="effect")
  {
    xref <- if(is.factor(x)) xx[1] else xref <- mean(x)
    yref <- if(is.factor(y)) yy[1] else yref <- mean(y)
    xydf <- rbind(c(xref, yref), xydf)
    df <- fillFrame(f, xydf, cond)
    DD <- rbind(f, df)
    if (class(fit)[1]=="mlm") {
      ind <- apply(is.finite(coef(fit)), 1, all)
      if (!identical(ind, apply(is.finite(coef(fit)), 1, any))) stop("Inconsistent NA/NaN coefficients across outcomes")
    } else ind <- is.finite(coef(fit))
    XX. <- model.matrix(as.formula(paste("~",formula(fit)[3])),DD)[-(1:nrow(f)), ind]
    XX <- t(t(XX.[-1,])-XX.[1,])
    if (class(fit)[1]=="mlm") {
      Z <- vector("list", n.y)
      for (i in 1:n.y) Z[[i]] <- matrix(trans(XX%*%coef(fit)[ind,i]), nrow=length(xx), ncol=length(yy))
      v <- list(XX=XX, z=Z)
    } else {
      z <- matrix(trans(XX%*%coef(fit)[ind]),nrow=length(xx),ncol=length(yy))
      v <- list(XX=XX, z=z)
    }
  }
  
  oxx <- xx
  oyy <- yy
  for (i in 1:n.y) {
    if (n.y > 1) {
      z <- Z[[i]]
      xx <- oxx
      yy <- oyy
      zname <- yname <- colnames(fit$residuals)[i]
    } else zname <- as.character(formula(fit)[2])
    
    ## Make factor axes
    mx <- lx <- my <- ly <- NULL
    if (is.factor(x)) {
      xAxis <- factorAxis(xx, whitespace, nn)
      xx <- xAxis$x
      mx <- xAxis$m
      lx <- xAxis$l
      z <- z[xAxis$ind,]
    }
    if (is.factor(y)) {
      yAxis <- factorAxis(yy,whitespace,nn)
      yy <- yAxis$x
      my <- yAxis$m
      ly <- yAxis$l
      z <- z[,yAxis$ind]
    }
    
    xlim <- if (is.factor(x)) c(0,1) else range(xx)
    ylim <- if (is.factor(y)) c(0,1) else range(yy)
    ##if (type=="effect") zlab <- as.expression(substitute(list(Delta) * x,list(x=as.character(formula(fit)[2]))))
    
    if (plot.type=="image") {
      plot.axes <- quote({axis(1,at=mx,labels=lx);axis(2,at=my,labels=ly)})
      color.palette=colorRampPalette(c("blue","gray90","red"),space="Lab")
      plot.args <- list(x=xx, y=yy, z=z, xlim=xlim, ylim=ylim, xlab=xvar, ylab=yvar, color.palette=color.palette, plot.axes=plot.axes, main=zname)
      new.args <- list(...)
      if (length(new.args)) plot.args[names(new.args)] <- new.args
      do.call("filled.contour", plot.args)
    } else if (plot.type=="persp") {
      ticktype <- ifelse(is.factor(x) | is.factor(y),"simple","detailed")
      plot.args <- list(x=xx, y=yy, z=z, xlim=xlim, ylim=ylim, xlab=xvar, ylab=yvar, zlab=zname, ticktype=ticktype, theta=-30)
      new.args <- list(...)
      if (length(new.args)) plot.args[names(new.args)] <- new.args
      do.call("persp", plot.args)
    } else if (plot.type=="rgl") {
      require(rgl)
      plot.args <- list(x=xx, y=yy, z=z, xlab=xvar, ylab=yvar, zlab=zname, color="gray")
      new.args <- list(...)
      if (length(new.args)) plot.args[names(new.args)] <- new.args
      do.call("persp3d",plot.args)
    }
  }
  return(invisible(v))
}
