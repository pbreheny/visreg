toplegend <- function(...) {
  if (par("oma")[3]==0) {
    x <- mean(par("usr")[1:2])
    yy <- transform_coord(par("usr")[3:4], par("plt")[3:4])
    y  <- mean(c(yy[2], par("usr")[4]))
    legend(x, y, xpd=NA, bty="n", xjust=0.5, yjust=0.5, ...)    
  } else {
    g <- par("mfrow")
    xx <- transform_coord(par("usr")[1:2], par("plt")[1:2])
    yy <- transform_coord(par("usr")[3:4], par("plt")[3:4])
    xxx <- transform_coord(xx, c(g[2]-1, g[2])/g[2])
    yyy <- transform_coord(yy, c(g[1]-1, g[1])/g[1])
    yyyy <- transform_coord(yyy, par("omd")[3:4])
    legend(mean(xxx), mean(c(yyy[2], yyyy[2])), xpd=NA, bty="n", xjust=0.5, yjust=0.5, ...)
  }
}

transform_coord <- function(x, p) {
  ba <- (x[2]-x[1])/(p[2]-p[1])
  a <- x[1]-p[1]*ba
  b <- a + ba
  c(a, b)
}
