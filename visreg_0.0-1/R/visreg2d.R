## To-do: add too.far option
## To-do: include "terms" option
visreg2d <- function(fit,x,y,fill=c("mean","zero"),res=ifelse(type=="persp",49,99),type=c("image","persp","rgl"),...)
  {
    type <- match.arg(type)
    fill <- match.arg(fill)
    f <- model.frame(fit)
    xx <- seq(min(f[,x]),max(f[,x]),len=res)
    yy <- seq(min(f[,y]),max(f[,y]),len=res)

    X1 <- as.data.frame(expand.grid(xx,yy))
    names(X1) <- c(x,y)
    X2 <- fillX(fit,fill,exclude=c(x,y))
    ## To-do: Add X3 for cond
    df <- model.frame(as.formula(paste("~",formula(fit)[3])),data.frame(X1,as.list(X2)))
    z <- matrix(predict(fit,newdata=df),nrow=res,ncol=res)

    if (type=="image")
      {
        ##image(xx,yy,z,col=colors,xlab=x,ylab=y,...)
        ##contour(xx,yy,z,add=TRUE)
        ## To-do: allow user to set colors, xlab, ylab, etc.
        color.palette=colorRampPalette(c("blue","gray90","red"),space="Lab")
        filled.contour(xx,yy,z,color.palette=color.palette,xlab=x,ylab=y,...)
      }
    else if (type=="persp")
      {
        persp(xx,yy,z,xlab=x,ylab=y,zlab="Ozone",...)
      }
    else if (type=="rgl")
      {
        require(rgl)
        persp3d(xx,yy,z,color="gray",xlab=x,ylab=y,zlab="Regression function")
      }
  }
