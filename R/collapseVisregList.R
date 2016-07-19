collapse.visregList <- function(obj, labels, ...) {
  fit <- res <- NULL
  if (missing(labels)) {
    l <- sapply(obj, function(v) v$meta$y)
    if (any(duplicated(l))) {
      labels <- paste0("Y", 1:length(obj))
    } else {
      labels <- l
    }
  }
  if (length(labels) != length(obj)) stop("labels do not match list")
  for (i in 1:length(obj)) {
    fit <- rbind(fit, data.frame(obj[[i]]$fit, visregCollapse=labels[i]))
    res <- rbind(res, data.frame(obj[[i]]$res, visregCollapse=labels[i]))
  }
  meta <- obj[[1]]$meta
  meta$by <- "visregCollapse"
  structure(list(fit=fit, res=res, meta=meta), class="visreg")
}
collapse <- function(obj, ...) UseMethod("collapse")
