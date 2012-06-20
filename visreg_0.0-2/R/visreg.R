## To-do: add 'cond' argument, allowing user to specify
##        the values of other covariates
## To-do: Handle interactions
## To-do: Handle factors
## To-do: Allow transformations
## To-do: Handle glm
visreg <- function(fit,xvar,fill=c("mean","zero","median"),type=c("response","terms"),alpha=.05,nn=101,...)
  {
    fill <- match.arg(fill)
    type <- match.arg(type)
    if (type=="terms") fill <- "zero"
    ##if (missing(intercept)) intercept <- switch(fill,mean=TRUE,zero=FALSE)
    
    if (length(xvar) > 1)
      {
        oask <- devAskNewPage(TRUE)
        on.exit(devAskNewPage(oask))
      }

    for (name in xvar)
      {
        if (type=="response")
          {
            x <- setupD(fit,name,fill,nn)
            y <- Response(fit,x,alpha)
          }
        else if (type=="terms")
          {
            x <- setupX(fit,name,fill,nn)
            y <- Terms(fit,x,alpha)
          }
        
        ## To-do: Allow user to change ylim, ylab, etc.
        ylim <- range(c(y$r,y$lwr,y$upr))
        ylab <- switch(type,response=as.character(formula(fit)[2]),
                       terms=bquote(f(.(name))))
        plot(NULL,type="n",xlab=name,xlim=range(x$xx),ylim=ylim,ylab=ylab,...)
        polygon(c(x$xx,rev(x$xx)),c(y$lwr,rev(y$upr)),col="gray85",border=F)
        lines(x$xx,y$fit,lwd=2)
        points(x$x,y$r,pch=19,cex=0.4)
      }
  }
