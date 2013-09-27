\name{visreg-package}
\alias{visreg.package}
\alias{visreg-package}
\docType{package}
\title{Visualization of regression models}
\description{\code{visreg} provides a number of plotting functions for
  visualizing fitted regression models: regression functions, confidence
  bands, partial residuals, interactions, and more.}
\details{
This package allows the use of visreg and visreg2d, functions for
visualizing regression models.  See example below for the most basic
use, and the help pages for each function for details. 
Also see the cited manuscript for additional details.  If you have a
question or feature request, please e-mail me at
<patrick-breheny@uiowa.edu>.
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
\code{\link{visreg}}
\code{\link{visreg2d}}
\code{\link{visreg-faq}}
}
\examples{
fit <- lm(Ozone ~ Solar.R + Wind + Temp, data=airquality)
visreg(fit, "Wind")
}
