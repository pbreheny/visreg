fillFrame <- function(f, x, cond) {
  ## x  = data frame of x variable(s) being changed
  ## x2 = variables being filled by median
  ## x3 = variables specified by cond
  if (missing(cond)) cond <- NULL
  for (j in 1:ncol(f)) {
    if (is.factor(f[,j]) && !is.element(names(f)[j], names(cond)) && !is.element(names(f)[j], names(x))) {
      mode = names(sort(-table(f[j])))[1]
      eval(parse(text=c('cond=c(cond, list(', names(f)[j],'=factor(mode, levels=levels(f[, names(f)[j]]))))')))
    }
  }
  exclude <- c(names(x), names(cond), names(which(sapply(f, function(x) inherits(x, "Surv")))))
  x2 <- lapply(as.data.frame(f[, setdiff(names(f), exclude)]), median)
  names(x2) <- setdiff(names(f), exclude)
  x3 <- cond
  for (j in seq_along(x3)) {
    if (is.character(x3[[j]])) x3[[j]] <- factor(x3[[j]], levels=levels(f[, names(x3)[j]]))
  }

  if (length(x2)>0 & length(x3)>0) newdf <- data.frame(x, x2, x3, check.names=FALSE)
  else if (length(x2)>0 & length(x3)==0) newdf <- data.frame(x, x2, check.names=FALSE)
  else if (length(x2)==0 & length(x3)>0) newdf <- data.frame(x, x3, check.names=FALSE)
  else newdf <- x

  newdf
}
