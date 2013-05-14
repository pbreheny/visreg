setupCond <- function(cond, f, by, breaks) {
  for (i in seq_along(cond)) if(class(cond[[i]])!="character" & class(f[,names(cond)[i]])=="factor") cond[[i]] <- as.character(cond[[i]])
  
  if (missing(by)) {
    cond <- list(cond)
  } else {
    cond.orig <- cond
    if(is.numeric(f[,by])) {
      if (length(breaks)==1) {
        unique.by <- unique(f[,by])
        if (breaks > length(unique.by)) {
          lev <- unique.by
        } else {
          a <- 1/5/2^(breaks-2)
          lev <- as.numeric(quantile(f[,by], seq(a,1-a,length=breaks), type=1))
          ##lev <- round(seq(min(f[,by]), max(f[,by]), length=breaks), digits=1-log10(sd(f[,by])))
        }
      } else {
        lev <- breaks
      }
      n.by <- length(lev)
    } else {
      n.by <- length(levels(f[,by]))
      lev <- levels(f[,by])
    }
    
    cond <- vector("list",n.by)
    for (i in 1:n.by) {
      cond[[i]] <- c(lev[i], cond.orig)
      names(cond[[i]])[1] <- by
      cond[[i]] <- as.list(cond[[i]])
    }
    attr(cond, "lev") <- lev
  }
  cond
}
