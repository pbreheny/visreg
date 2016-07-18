collapse.visregList <- function(v) {
  fit <- res <- NULL
  for (i in 1:length(v)) {
    fit <- rbind(fit, data.frame(v[[i]]$fit, visregCollapse=v[[i]]$meta$y))
    res <- rbind(res, data.frame(v[[i]]$res, visregCollapse=v[[i]]$meta$y))
  }
  meta <- v[[1]]$meta
  meta$by <- "visregCollapse"
  structure(list(fit=fit, res=res, meta=meta), class="visreg")
}
