setupCond <- function(cond, f, by, breaks) {
  for (i in seq_along(cond)) if(!is.character(cond[[i]]) & is.factor(f[, names(cond)[i]])) cond[[i]] <- as.character(cond[[i]])
  
  if (missing(by)) {
    cond <- list(cond)
  } else {
    cond.orig <- cond
    if(is.numeric(f[, by])) {
      if (length(breaks)==1) {
        unique.by <- unique(f[, by])
        if (breaks >= length(unique.by)) {
          lev <- sort(unique.by)
        } else {
          a <- 1/5/2^(breaks-2)
          lev <- as.double(quantile(f[, by], seq(a, 1-a, length=breaks), type=1))
        }
      } else {
        lev <- breaks
      }
      n.by <- length(lev)
    } else {
      if (is.factor(breaks) || is.character(breaks)) {
        if (!all(breaks %in% levels(f[, by]))) stop("'breaks' does not match levels of 'by' variable", call.=FALSE)
        lev <- breaks
      } else {
        lev <- levels(f[, by])
      }
      n.by <- length(lev)
    }
    
    cond <- vector("list", n.by)
    for (i in 1:n.by) {
      a <- if (is.factor(lev)) as.character(lev[i]) else lev[i]
      cond[[i]] <- c(a, cond.orig)
      names(cond[[i]])[1] <- by
      cond[[i]] <- as.list(cond[[i]])
    }
    attr(cond, "lev") <- lev
  }
  cond
}
