collapse.visreg_list <- function(obj, labels, ...) {
  if (missing(labels)) {
    l <- sapply(obj, function(v) v$meta$y)
    if (any(duplicated(l))) {
      labels <- paste0("Y", 1:length(obj))
    } else {
      labels <- l
    }
  }
  if (length(labels) != length(obj)) {
    stop("labels do not match list", call. = FALSE)
  }
  fit <- data.frame(obj[[1]]$fit, visreg_collapse = labels[1])
  res <- data.frame(obj[[1]]$res, visreg_collapse = labels[1])
  if (length(obj) >= 2) {
    for (i in 2:length(obj)) {
      fit <- merge(
        fit,
        data.frame(obj[[i]]$fit, visreg_collapse = labels[i]),
        all.x = TRUE,
        all.y = TRUE
      )
      res <- merge(
        res,
        data.frame(obj[[i]]$res, visreg_collapse = labels[i]),
        all.x = TRUE,
        all.y = TRUE
      )
    }
  }
  meta <- obj[[1]]$meta
  meta$by <- "visreg_collapse"
  structure(list(fit = fit, res = res, meta = meta), class = "visreg")
}
