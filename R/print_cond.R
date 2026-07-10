print_cond <- function(v) {
  p <- ncol(v$fit) - 4
  X <- v$fit[, 1:p, drop = FALSE]
  X <- X[, -which(names(X) == v$meta$x), drop = FALSE]
  constant_columns <- which(sapply(X, function(x) all(x == x[1])))
  varying_columns <- setdiff(seq_len(ncol(X)), constant_columns)
  for (j in seq_len(ncol(X))) {
    if (is.factor(X[, j])) X[, j] <- as.character(X[, j])
  }
  lines <- "Conditions used in construction of plot"
  for (j in varying_columns) {
    x <- paste(unique(X[, j]), collapse = " / ")
    lines <- c(lines, paste0(names(X)[j], ": ", x))
  }
  for (j in constant_columns) {
    lines <- c(lines, paste0(names(X)[j], ": ", X[1, j]))
  }
  cond_text <- paste(lines, collapse = "\n")

  if (isTRUE(v$meta$main_effect_warn)) {
    warning(
      "  You are plotting the effect of '", v$meta$x, "' as if it were a 'main effect', but the
  model contains an interaction involving this variable that has not been addressed via the 'by'
  or 'cond' argument.  The plot therefore shows the effect of '", v$meta$x, "' conditional on the
  values below, not its overall effect.\n\n  ", cond_text,
      call. = FALSE
    )
  } else {
    cat(cond_text, "\n", sep = "")
  }
}
