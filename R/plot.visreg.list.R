plot.visreg.list <- function(x, ask=TRUE, ...) {
  n <- length(x)
  prompt.user <- FALSE
  if (ask & (prod(par("mfcol")) < n) && dev.interactive()) {
    oask <- devAskNewPage()
    prompt.user <- TRUE
    on.exit(devAskNewPage(oask))
  }

  for (i in 1:length(x)) {
    plot(x[[i]], ...)
    if (prompt.user) devAskNewPage(TRUE)
  }
}
