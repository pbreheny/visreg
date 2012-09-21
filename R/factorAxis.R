factorAxis <- function(xx, w, nn)
  {
    l <- levels(xx)
    K <- length(levels(xx))
    len <- K*(1-w)+(K-1)*w
    m <- ((0:(K-1))/len+(1-w)/(2*len))
    ind <- numeric(nn)
    for(k in 1:K)
      {
        i1 <- ceiling(nn*(k-1)/len)
        i2 <- ceiling(nn*((k-1)/len + (1-w)/len))
        i3 <- ceiling(nn*k/len)
        ind[i1:i2] <- k
        if (k!=K) ind[(i2+1):i3] <- NA
      }
    return(list(x=seq(0,1,length=nn),m=m,l=l,ind=ind))
  }
