factorAxis2d <- function(xx, w, nn) {
  l <- levels(xx)
  n_l <- nlevels(xx)
  len <- n_l * (1 - w) + (n_l - 1) * w
  m <- ((0:(n_l - 1)) / len + (1 - w) / (2 * len))
  ind <- integer(nn)
  for (k in 1:n_l) {
    i1 <- ceiling(nn * (k - 1) / len)
    i2 <- ceiling(nn * ((k - 1) / len + (1 - w) / len))
    i3 <- ceiling(nn * k / len)
    ind[i1:i2] <- k
    if (k != n_l) ind[(i2 + 1):i3] <- NA
  }
  list(x = seq(0, 1, length = nn), m = m, l = l, ind = ind)
}
