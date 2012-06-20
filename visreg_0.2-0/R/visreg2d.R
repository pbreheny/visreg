## To-do: Check for redundancy?
## To-do: Make compatible with glm
## To-do: add too.far option
visreg2d <- function(fit,x,y,fill=c("mean","zero","median"),type=c("response","terms"),res=ifelse(type=="persp",49,99),plot.type=c("image","persp","rgl"),trans=as.matrix,cond=list(),...)
  {
    plot.type <- match.arg(plot.type)
    fill <- match.arg(fill)
    type <- match.arg(type)

    if ("data" %in% names(fit$call)) data <- eval(fit$call$data,env=environment(fit$terms))
    f <- model.frame(fit)
    f <- cbind(f,data[,setdiff(names(data),names(f))])

    if (type=="terms")
      {
        fill <- "zero"
        cond <- list()
        meanx =  mean(f[,x])
        meany =  mean(f[,y])
        f[,x] <- f[,x] - meanx
        f[,y] <- f[,y] - meany
        fit <- update(fit,data=f)
      }

    if(is.factor(f[,x]))
      {
        xx <- levels(f[,x])
        mx <- seq(res/2,res*length(levels(f[,x]))-res/2,length.out=length(levels(f[,x])))
        lx <- levels(f[,x])
      }
    else
      {
        xx <- seq(min(f[,x]),max(f[,x]),len=res)
        mx <- NULL
        lx <- NULL
      }

    if(is.factor(f[,y]))
      {
        yy <- levels(f[,y])
        my <- seq(res/2,res*length(levels(f[,y]))-res/2,length.out=length(levels(f[,y])))
        ly <- levels(f[,y])
      }
    else
      {
        yy <- seq(min(f[,y]),max(f[,y]),len=res)
        my <- NULL
        ly <- NULL
      }


    X1 <- as.data.frame(expand.grid(xx,yy))
    names(X1) <- c(x,y)

    for(i in 1:length(names(f)))
      {
        if(is.factor(f[,names(f)[i]]))
          {
            mode = names(sort(-table(f[i])))[1]
            if(fill=='zero')
              {
                eval(parse(text=c('cond=c(cond,list(',names(f)[i],'=factor(levels(f[,names(f)[i]])[1],levels=levels(f[,names(f)[i]]))))')))
              }
            else
              {
                eval(parse(text=c('cond=c(cond,list(',names(f)[i],'=factor(mode,levels=levels(f[,names(f)[i]]))))')))
              }
          }
      }

    X2 <- fillX(fit,fill,exclude=c(x,y,names(cond)))
    X3 <- cond

    if (length(X3) > 0 & length(X2) > 0)
      {
        df <- model.frame(as.formula(paste("~",formula(fit)[3])),data.frame(X1,as.list(X2),X3))
      }
    else if (length(X3) == 0 & length(X2) > 0)
      {
        df <- model.frame(as.formula(paste("~",formula(fit)[3])),data.frame(X1,as.list(X2)))
      }
    else if (length(X3) > 0 & length(X2) == 0)
      {
        df <- model.frame(as.formula(paste("~",formula(fit)[3])),data.frame(X1,X3))
      }


    if(is.factor(f[,x]) & is.factor(f[,y]))
      {
        tempz <- trans(matrix(predict(fit,newdata=df),nrow=length(levels(f[,x])),ncol=length(levels(f[,y]))))
        z <- matrix(NA,length(levels(f[,x]))*res,length(levels(f[,y]))*res)
        for(i in 1:length(levels(f[,x])))
          {
            for(j in 1:length(levels(f[,y])))
              {
                z[(((i-1)*res + 1):(i*res)),(((j-1)*res + 1):(j*res))] <- tempz[i,j]
              }
          }
        xx <- seq(1,length(levels(f[,x]))*res)
        yy <- seq(1,length(levels(f[,y]))*res)
      }
    else if(is.factor(f[,x]) & !is.factor(f[,y]))
      {
        tempz <- (matrix(predict(fit,newdata=df),nrow=length(levels(f[,x])),ncol=res))
        z <- matrix(NA,length(levels(f[,x]))*res,res)
        for(i in 1:length(levels(f[,x])))
          {
            for(j in (((i-1)*res + 1):(i*res)))
              {
                z[j,] <- tempz[i,]
              }
          }
        xx <- seq(1,length(levels(f[,x]))*res)
      }
    else if(!is.factor(f[,x]) & is.factor(f[,y]))
      {
        tempz <- (matrix(predict(fit,newdata=df),nrow=res,ncol=length(levels(f[,y]))))
        z <- matrix(NA,res,length(levels(f[,y]))*res)
        for(i in 1:length(levels(f[,y])))
          {
            z[,(((i-1)*res + 1):(i*res))] <- tempz[,i]
          }
        yy <- seq(1,length(levels(f[,y]))*res)
      }
    else
      {
        z <- trans(matrix(predict(fit,newdata=df),nrow=res,ncol=res))
      }

    if (type == "terms")
      {
        xx <- xx + meanx
        yy <- yy + meany
      }


    if (plot.type=="image")
      {
        ##image(xx,yy,z,col=colors,xlab=x,ylab=y,...)
        ##contour(xx,yy,z,add=TRUE)
        ## To-do: allow user to set colors, xlab, ylab, etc.
        color.palette=colorRampPalette(c("blue","gray90","red"),space="Lab")
        filled.contour(xx,yy,z,color.palette=color.palette,xlab=x,ylab=y,plot.axes={axis(1,at=mx,labels=lx);axis(2,at=my,labels=ly)},...)
      }
    else if (plot.type=="persp")
      {
        persp(xx,yy,z,xlab=x,ylab=y,zlab="Ozone",...)
      }
    else if (plot.type=="rgl")
      {
        require(rgl)
        persp3d(xx,yy,z,color="gray",xlab=x,ylab=y,zlab="Regression function")
      }

  }
