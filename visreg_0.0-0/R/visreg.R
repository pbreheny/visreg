## To-do: add 'cond' argument, allowing user to specify
##        the values of other covariates
## To-do: add 'median' and 'zero' options to fill
visreg <- function(fit,xvar,fill=c("mean"),alpha=.05,nn=101,...)
  {
    fill <- match.arg(fill)
    f <- model.frame(fit)
    
    if (length(xvar) > 1)
      {
        oask <- devAskNewPage(TRUE)
        on.exit(devAskNewPage(oask))
      }

    for (j in match(xvar,names(f)))
      {
        name <- names(f)[j]
        X2 <- fillX(fit,fill,exclude=name)
        ## To-do: Add X3 for cond

        ## Set up n x p matrix for conditional partial residuals
        x <- f[,j]
        newdf <- data.frame(x,as.list(X2))
        names(newdf)[1] <- name
        X <- model.matrix(as.formula(paste("~",formula(fit)[3])),newdf)

        ## Set up data frame with nn rows for prediction
        xx <- seq(min(x),max(x),length=nn)
        newdf <- data.frame(xx,as.list(X2))
        names(newdf)[1] <- name
        df <- model.frame(as.formula(paste("~",formula(fit)[3])),newdf)

        pred <- predict(fit,newdata=df,se=TRUE)
        lower <- pred$fit-qt(1-alpha/2,fit$df.residual)*pred$se
        upper <- pred$fit+qt(1-alpha/2,fit$df.residual)*pred$se
        r <- X%*%coef(fit)+residuals(fit)

        ## To-do: Allow user to change ylim, ylab
        ylim <- range(c(r,lower,upper))
        plot(x,r,type="n",xlab=name,ylim=ylim,ylab="Regression function",...)
        polygon(c(xx,rev(xx)),c(lower,rev(upper)),col="gray85",border=F)
        lines(xx,pred$fit,lwd=2)
        points(x,r,pch=19,cex=0.4)
      }
  }
