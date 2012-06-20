\name{visreg-package}
\alias{visreg-package}
\docType{package}
\title{Visualization of regression functions}
\description{\code{visreg} provides a number of plotting functions for
  visualizing fitted regression models: regression functions, confidence
  bands, partial residuals, interactions, and more.}
\details{
\tabular{ll}{
Package: \tab visreg\cr
Type: \tab Package\cr
Version: \tab 1.0-0\cr
Date: \tab 2011-08-09\cr
License: \tab GPL-2\cr
LazyLoad: \tab yes\cr}
This package allows the use of visreg and visreg 2d, both R functions used to visualize regression functions.  See the examples below for some basic uses and look in the help pages for each function for a much more detailed look.
}
\author{Patrick Breheny and Woodrow Burchett
  
Maintainer: Patrick Breheny <patrick.breheny@uky.edu>}
\references{
}
\keyword{ package }
\seealso{
\code{\link[visreg]{visreg}}
\code{\link[visreg]{visreg2d}}
}
\examples{
ozone <- airquality[complete.cases(airquality),]

fit <- lm(Ozone ~ Solar.R + Wind + Temp,data=ozone)
visreg(fit,"Wind")
visreg(fit,"Wind",type="terms")


}
