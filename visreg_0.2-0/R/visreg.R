## To-do: modify ylab for glms
## Fix cond() for models with interactions
## Fix subsetting for factor plots
## Add padding to ylim
visreg <- function(fit,xvar,by,breaks=4,fill=c("mean","zero","median"),type=c("response","terms"),trans=as.numeric,scale=c("linear","response"),xtrans=as.numeric,alpha=.05,nn=101,cond=list(),whitespace=0.2,partial=TRUE,...)
  {
    fill <- match.arg(fill)
    type <- match.arg(type)
    scale <- match.arg(scale)
    if (scale=="response") trans <- family(fit)$linkinv

    if (type=="terms")
      {
        fill <- "zero"
        cond <- list() ## Does this make sense when interactions are present?
      }

    f <- extract.frame(fit)
    ## Handle some issues with variable classes
    for (i in seq_along(cond)) if(class(cond[[i]])!="character" & class(f[,names(cond)[i]])=="factor") cond[[i]] <- as.character(cond[[i]])
    if (any(sapply(f,class)=="logical"))
      {
        for (j in 1:ncol(f)) if (class(f[,j])=="logical") f[,j] <- as.numeric(f[,j])
        fit <- update(fit,data=f)
      }
    
    if (missing(by)) cond <- list(cond)
    else
      {
        if(is.numeric(f[,by]))
          {
            probs <- seq(0,(breaks-1))/(breaks-1)
            n.by <- breaks
            levels <- as.numeric(signif(quantile(f[,by],probs),digits=3))
          }
        else
          {
            n.by <- length(levels(f[,by]))
            levels <- levels(f[,by])
          }
        cond.orig <- cond
        cond <- vector("list",n.by)
        for (i in 1:n.by)
          {
            cond[[i]] <- c(levels[i],cond.orig)
            names(cond[[i]])[1] <- by
            cond[[i]] <- as.list(cond[[i]])
          }
      }
    
    if (length(xvar) > 1)
      {
        oask <- devAskNewPage(TRUE)
        on.exit(devAskNewPage(oask))
      }

    for (name in xvar)
      {
        if(!missing(by))
          {
            if(is.factor(model.frame(fit)[,name]))
              {
                nn <- length(model.frame(fit)[,name])*length(levels(f[,name]))
              }
            lframe <- matrix(0,nn*length(levels),5)
            lframe <- as.data.frame(lframe)
            names(lframe)<-c('xx','fit','upr','lwr','by')
            if(is.factor(model.frame(fit)[,name]))
              {
                lresids <- matrix(0,length(model.frame(fit)[,name])*length(levels(model.frame(fit)[,name])),3)
              }
            else
              {
                lresids <- matrix(0,length(model.frame(fit)[,name]),3)
              }
            lresids <- as.data.frame(lresids)
            names(lresids)<-c('r','x','by')
          }
        for (i in 1:length(cond))
          {
            if (type=="response")
              {
                x <- setupD(fit,f,name,fill,nn,cond[[i]],whitespace)
                y <- Response(fit,x,trans,alpha)
              }
            else if (type=="terms")
              {
                x <- setupX(fit,f,name,fill,nn,cond[[i]])
                y <- Terms(fit,x,trans,alpha)
              }

            plot.args <- list(x=1, y=1, ylim=range(c(y$r,y$lwr,y$upr)), xlab=name, ylab=switch(type,response=as.character(formula(fit)[2]),terms=paste("f(",name,")",sep="")), type="n", xlim=range(xtrans(x$xx)),xaxt=ifelse(is.factor(model.frame(fit)[,name]),'n','s'))
            
            if (missing(by))
              {
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
                        if (partial)
                          {
                            rpoints <- (as.vector(rep(levels(xFactor)[i],length(f[,name]))) == as.vector(f[,name]))
                            points(x$x[sect][rpoints],y$r[sect][rpoints],pch=19,cex=0.4)
                          }
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
            else
              {
                lframe$xx[((i-1)*nn + 1):(i*nn)] <- x$xx
                lframe$fit[((i-1)*nn + 1):(i*nn)] <- y$fit
                lframe$upr[((i-1)*nn + 1):(i*nn)] <- y$upr
                lframe$lwr[((i-1)*nn + 1):(i*nn)] <- y$lwr
                lframe$by[((i-1)*nn + 1):(i*nn)] <- levels[i]
                if(is.numeric(f[,by]))
                  {
                    if(i == 1)
                      {
                        up <- mean(c(levels[i],levels[i+1]))
                        rpoints <- (model.frame(fit)[,by] < up)
                      }
                    else if(i == length(cond))
                      {
                        lw <- mean(c(levels[i],levels[i-1]))
                        rpoints <- (model.frame(fit)[,by] >= lw)
                      }
                    else
                      {
                        up <- mean(c(levels[i],levels[i+1]))
                        lw <- mean(c(levels[i],levels[i-1]))
                        rpoints <- (model.frame(fit)[,by] >= lw & model.frame(fit)[,by] < up)
                      }
                  }
                else
                  {
                    rpoints <- as.vector(x$D[,by]) == as.vector(model.frame(fit)[,by])
                  }
                lresids$x[rpoints] <- x$x[rpoints]
                lresids$r[rpoints] <- y$r[rpoints]
                lresids$by[rpoints] <- levels[i]
              }
          }

        if(!missing(by))
          {
            ylim <- range(c(lframe$r,lframe$lwr,lframe$upr))
            trellis.par.set(plot.symbol=list(pch=19,cex=0.4))

            lframe$by <- as.factor(lframe$by)
            lresids$by <- as.factor(lresids$by)
            levels(lframe$by) <- levels
            levels(lresids$by) <- levels
           
            if(is.factor(model.frame(fit)[,name]))
              {
                visregPanel <- function(x,y,subscripts,...)
                  {
                    xFactor <- model.frame(fit)[,name]
                    seg <- length(model.frame(fit)[,name])
                    marks <- array(NA,length(levels(xFactor)))
                    for(i in 1:length(levels(xFactor)))
                      {
                        sect <- ((i-1)*seg + 1):(i*seg)
                        lpolygon(c(lframe$xx[subscripts][sect],rev(lframe$xx[subscripts][sect])), c(lframe$lwr[subscripts][sect],rev(lframe$upr[subscripts][sect])),col="grey85", border=F, subscripts=subscripts,...)
                        llines(lframe$xx[subscripts][sect],lframe$fit[subscripts][sect],lwd=2,subscripts=subscripts,...)
                        if(partial)
                          {
                            rpoints <- (as.vector(rep(levels(xFactor)[i],length(f[,name]))) == as.vector(f[,name]))
                            plotpoints <- (as.vector(rep(lframe$by[subscripts][1],length(lresids$by[sect][rpoints]))) == as.vector(lresids$by[sect][rpoints]))
                            lpoints(lresids$x[sect][rpoints][plotpoints],lresids$r[sect][rpoints][plotpoints])
                          }
                      }
                    panel.xyplot(0,0,subscripts=subscripts,...)
                  }

                xFactor <- model.frame(fit)[,name]
                seg <- length(model.frame(fit)[,name])
                marks <- array(NA,length(levels(xFactor)))
                for(i in 1:length(levels(xFactor)))
                  {
                    sect <- ((i-1)*seg + 1):(i*seg)
                    marks[i] <- mean(lframe$xx[sect])
                  }
                scales = list(x = list(at = marks,labels = levels(xFactor)))
                print(xyplot(lframe$fit~lframe$xx | lframe$by,type="l",panel=visregPanel,ylim=ylim,xlab=plot.args$xlab,ylab=plot.args$ylab,scales=scales,...))
              }
            else
              {
                visregPanel <- function(x,y,subscripts,...)
                  {
                    lpolygon(c(lframe$xx[subscripts],rev(lframe$xx[subscripts])), c(lframe$lwr[subscripts],rev(lframe$upr[subscripts])),col="grey85", border=F, subscripts=subscripts,...)
                    panel.xyplot(x,y,subscripts=subscripts,...)
                    if(partial)
                      {
                        plotpoints <- (as.vector(rep(lframe$by[subscripts][1],length(lresids$by))) == as.vector(lresids$by))
                        lpoints(lresids$x[plotpoints],lresids$r[plotpoints])
                      }
                  }
                print(xyplot(lframe$fit~lframe$xx | lframe$by,type="l",panel=visregPanel,ylim=ylim,xlab=plot.args$xlab,ylab=plot.args$ylab,...))

              }
          }

      }
    invisible(list(x=x,y=y))
  }
