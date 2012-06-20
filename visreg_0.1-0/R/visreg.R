## To-do: Handle interactions
## To-do: Handle survival
## To-do: modify ylab for glms
## To-do: think about 'response', 'term' naming
## To-do: Option for returning fillFrame values?
visreg <- function(fit,xvar,fill=c("mean","zero","median"),type=c("response","terms"),trans=as.numeric,xtrans=as.numeric,alpha=.05,nn=101,cond=list(),whitespace=0.2,partial=TRUE,...)
  {
    fill <- match.arg(fill)
    type <- match.arg(type)
    if (type=="terms")
      {
        fill <- "zero"
        cond <- list()
      }
    
    if (length(xvar) > 1)
      {
        oask <- devAskNewPage(TRUE)
        on.exit(devAskNewPage(oask))
      }
    fit <- fixLogical(fit)

    for (name in xvar)
      {
        if (type=="response")
          {
            x <- setupD(fit,name,fill,nn,cond,whitespace)
            y <- Response(fit,x,trans,alpha)
          }
        else if (type=="terms")
          {
            x <- setupX(fit,name,fill,nn,cond)
            y <- Terms(fit,x,trans,alpha)
          }
        
        plot.args <- list(x=1, y=1, ylim=range(c(y$r,y$lwr,y$upr)), xlab=name, ylab=switch(type,response=as.character(formula(fit)[2]),terms=paste("f(",name,")",sep="")), type="n", xlim=range(xtrans(x$xx)),xaxt=ifelse(is.factor(model.frame(fit)[,name]),'n','s'))
        new.args <- list(...)
        if (length(new.args)) plot.args[names(new.args)] <- new.args
        do.call("plot", plot.args)
        
        if(is.factor(model.frame(fit)[,name]))
          {
            xFactor <- model.frame(fit)[,name]
            seg <- length(model.frame(fit)[,name])
            for(i in 1:length(levels(xFactor)))
              {
                sect <- ((i-1)*seg + 1):(i*seg)
                polygon(c(x$xx[sect],rev(x$xx[sect])),c(y$lwr[sect],rev(y$upr[sect])),col="gray85",border=F)
                lines(x$xx[sect],y$fit[sect],lwd=2)
                if (partial) points(x$x[sect],y$r[sect],pch=19,cex=0.4)
                mark <- mean(x$xx[sect])
                axis(side=1,at=mark,labels=levels(xFactor)[i])
              }
          }
        else
          {
            polygon(c(xtrans(x$xx),xtrans(rev(x$xx))),c(y$lwr,rev(y$upr)),col="gray85",border=F)
            lines(xtrans(x$xx),y$fit,lwd=2)
            if (partial) points(xtrans(x$x),y$r,pch=19,cex=0.4)
          }
      }
  }
