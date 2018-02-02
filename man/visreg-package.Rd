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
  \itemize{
    \item \url{http://pbreheny.github.io/visreg}
    \item Breheny, P. and Burchett, W. (2017), Visualizing regression
    models using visreg.
    \url{https://journal.r-project.org/archive/2017/RJ-2017-046/index.html}
  }
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
