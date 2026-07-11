print_cond <- function(v) {
  p <- ncol(v$fit) - 4
  frame_fit <- v$fit[, 1:p, drop = FALSE]
  frame_nox <- frame_fit[, -which(names(frame_fit) == v$meta$x), drop = FALSE]
  v$fit[, 1:p, drop = FALSE] |> _[, -which(names(frame_nox) == v$meta$x), drop = FALSE]
  constant_columns <- which(sapply(frame_nox, \(x) all(x == x[1])))
  varying_columns <- setdiff(seq_len(ncol(frame_nox)), constant_columns)
  for (j in seq_len(ncol(frame_nox))) {
    if (is.factor(frame_nox[, j])) {
      frame_nox[, j] <- as.character(frame_nox[, j])
    }
  }
  lines <- "Conditions used in construction of plot"
  for (j in varying_columns) {
    x <- paste(unique(frame_nox[, j]), collapse = " / ")
    lines <- c(lines, paste0(names(frame_nox)[j], ": ", x))
  }
  for (j in constant_columns) {
    lines <- c(lines, paste0(names(frame_nox)[j], ": ", frame_nox[1, j]))
  }
  cond_text <- paste(lines, collapse = "\n")

  if (isTRUE(v$meta$main_effect_warn)) {
    warning(
      "  You are plotting the effect of '",
      v$meta$x,
      "' as if it were a 'main effect', but the
  model contains an interaction involving this variable that has not been addressed via the 'by'
  or 'cond' argument.  The plot therefore shows the effect of '",
      v$meta$x,
      "' conditional on the
  values below, not its overall effect.\n\n  ",
      cond_text,
      call. = FALSE
    )
  } else {
    cat(cond_text, "\n", sep = "")
  }
}
