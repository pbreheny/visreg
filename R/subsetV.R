## Subsets v so that residuals appear only once, in the appropriate panel
subsetV <- function(v, f, by) {
  lev <- attr(v, "lev")
  ## Calculate distance
  if (is.numeric(f[,by])) {
    D <- matrix(NA, nrow(f), length(v))
    for (i in 1:length(v)) {
      D[,i] <- (f[,by]-lev[i])^2
    }
  }
  for (i in 1:length(v)) {
    if (is.factor(f[,by])) {
      ind <- f[,by]==lev[i]
    } else {
      ind <- (apply(D, 1, which.min)==i)
    }
    v[[i]]$x$x <- v[[i]]$x$x[ind]
    v[[i]]$y$r <- if (v[[i]]$y$n == 1) v[[i]]$y$r[ind] else v[[i]]$y$r[ind,]
    v[[i]]$y$pos <- if (v[[i]]$y$n == 1) v[[i]]$y$pos[ind] else v[[i]]$y$pos[ind,]
  }
  v
}