setupD <- function(fit,f,name,fill,nn,cond,whitespace)
  {
    ## Set up n-row data frame for residuals
    x <- f[,name]
    if(is.factor(x))
      {
        l = length(x)
        tx <- as.vector(rep(levels(x)[1],l))
        for(i in 2:length(levels(x)))
          {
            tx <- c(tx,rep(levels(x)[i],l))
          }
        x <- factor(tx,levels=levels(x))
      }
    attr(x,"name") <- name
    df <- fillFrame(f,x,fill,cond)
    ##print(df)
    D <- model.frame(as.formula(paste("~",formula(fit)[3])),df)
    D <- cbind(D,df[,setdiff(names(df),names(D))])

    ## Set up nn-row data frame for prediction
    if(is.factor(x))
      {
        xx <- rep(levels(x)[1],l)
        for(i in 2:length(levels(x)))
          {
            xx <- c(xx,rep(levels(x)[i],l))
          }
        attr(xx,"name") <- name
        df <- fillFrame(f,xx,fill,cond)
        DD <- model.frame(as.formula(paste("~",formula(fit)[3])),df)
        DD <- cbind(DD,df[,setdiff(names(df),names(DD))])

        x <- seq(1,100,length=l)
        for(i in 1:(length(levels(f[,name]))-1))
          {
            x <- c(x,seq((100*i + i*whitespace*100 + 1),(100*(i+1) + i*whitespace*100),length=l))
          }
        xx <- x
      }
    else
      {
        xx <- seq(min(x),max(x),length=nn)
        attr(xx,"name") <- name
        df <- fillFrame(f,xx,fill,cond)
        DD <- model.frame(as.formula(paste("~",formula(fit)[3])),df)
        DD <- cbind(DD,df[,setdiff(names(df),names(DD))])
      }

    return(list(x=x,xx=xx,D=D,DD=DD))
  }
