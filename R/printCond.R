printCond <- function(v, warn=FALSE) {
  if (warn) writeLines(c(strwrap("Please note that you are attempting to plot a 'main effect' in a model that contains an interaction.  This is potentially misleading; you may wish to consider using the 'by' argument."), ""))
  p <- ncol(v$fit)-4
  X <- v$fit[,1:p,drop=FALSE]
  X <- X[,-which(names(X)==v$meta$x),drop=FALSE]
  constant.columns <- which(sapply(X, function(x) all(x==x[1])))
  varying.columns <- setdiff(1:ncol(X), constant.columns)
  for (j in 1:ncol(X)) if (is.factor(X[,j])) X[,j] <- as.character(X[,j])
  cat("Conditions used in construction of plot\n")
  for (j in varying.columns) {
    x <- paste(X[,j], collapse= " / ")
    cat(names(X)[j], ": ", x, "\n", sep="")
  }    
  for (j in constant.columns) {
    cat(names(X)[j], ": ", X[1,j], "\n", sep="")
  }
}
