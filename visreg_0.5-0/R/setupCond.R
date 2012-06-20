setupCond <- function(cond,by,f,breaks)
  {
    for (i in seq_along(cond)) if(class(cond[[i]])!="character" & class(f[,names(cond)[i]])=="factor") cond[[i]] <- as.character(cond[[i]])
    
    if (missing(by)) cond <- list(cond)
    else
      {
        cond.orig <- cond
        if(is.numeric(f[,by])) 
          {
            brk <- quantile(f[,by],seq(0,1,length=breaks+1))
            brk[1] <- brk[1]-1e-10
            new.by <- cut(f[,by],breaks=brk)
            n.by <- breaks
            lev <- numeric(breaks)
          }
        else
          {
            n.by <- length(levels(f[,by]))
            lev <- levels(f[,by])
          }
        cond <- vector("list",n.by)
        for (i in 1:n.by)
          {
            if (is.numeric(f[,by]))
              {
                limits <- unlist(strsplit(levels(new.by)[i],","))
                limits <- gsub("(","",limits,fixed=TRUE)
                limits <- gsub("]","",limits,fixed=TRUE)
                cond[[i]] <- c(mean(as.numeric(limits)),cond.orig)
              }
            else cond[[i]] <- c(lev[i],cond.orig)
            names(cond[[i]])[1] <- by
            cond[[i]] <- as.list(cond[[i]])
          }
        if (is.numeric(f[,by]))
          {
            attr(cond,"lev") <- levels(new.by)
            attr(cond,"new.by") <- new.by
          }
        else attr(cond,"lev") <- lev
      }
    return(cond)
  }
