plot.visregList <- function(x, ask=TRUE, ...) {
  n <- length(x)
  prompt.user <- FALSE
  if (ask & (prod(par("mfcol")) < n) && dev.interactive()) {
    oask <- devAskNewPage()
    prompt.user <- TRUE
    on.exit(devAskNewPage(oask))
  }

  for (i in 1:length(x)) {
    p <- plot(x[[i]], ...)
    if (inherits(p, 'gg')) {
      if (i==1) {
        ggList <- vector('list', length(x))
      }
      ggList[[i]] <- p
    } else {
      if (prompt.user) devAskNewPage(TRUE)
    }
  }
  if (inherits(p, 'gg')) return(ggList)
}
