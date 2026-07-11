# Subset xy so that residuals appear only once
subset_xy <- function(v, f, by, lev, type) {
  # Calculate distance
  if (is.numeric(f[, by])) {
    distance <- matrix(NA, nrow(f), length(v))
    for (i in seq_along(v)) {
      distance[, i] <- (f[, by] - lev[i])^2
    }
  }
  for (i in seq_along(v)) {
    if (is.factor(f[, by])) {
      ind <- as.character(f[, by]) == as.character(lev[i])
    } else {
      ind <- (apply(distance, 1, which.min) == i)
    }
    v[[i]]$x$frame_res <- v[[i]]$x$frame_res[ind, ]
    v[[i]]$x$x_res <- v[[i]]$x$x_res[ind]
    v[[i]]$y$r <- if (v[[i]]$y$n_y == 1) v[[i]]$y$r[ind] else v[[i]]$y$r[ind, ]
    v[[i]]$y$pos <- if (v[[i]]$y$n_y == 1) {
      v[[i]]$y$pos[ind]
    } else {
      v[[i]]$y$pos[ind, ]
    }
  }
  v
}
