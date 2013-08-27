\name{visreg-package}
\alias{visreg-package}
\docType{package}
\title{Visualization of regression models}
\description{\code{visreg} provides a number of plotting functions for
  visualizing fitted regression models: regression functions, confidence
  bands, partial residuals, interactions, and more.}
\details{
\tabular{ll}{
Package: \tab visreg\cr
Type: \tab Package\cr
Version: \tab 2.0-2\cr
Date: \tab 2013-08-27\cr
License: \tab GPL-2\cr
LazyLoad: \tab yes\cr}
This package allows the use of visreg and visreg2d, functions for
visualizing regression models.  See example below for the most basic
use, and the help pages for each function for details. 
Also see the cited manuscript for additional details.
}
\author{Patrick Breheny and Woodrow Burchett
  
Maintainer: Patrick Breheny <patrick-breheny@uiowa.edu>}
\references{
  Breheny, P. and Burchett, W. (2013), Visualizing regression models
  using visreg.
  \url{http://myweb.uiowa.edu/pbreheny/publications/visreg.pdf}
}
\keyword{ package }
\seealso{
\code{\link[visreg]{visreg}}
\code{\link[visreg]{visreg2d}}
}
\examples{
fit <- lm(Ozone ~ Solar.R + Wind + Temp, data=airquality)
visreg(fit, "Wind")
}
