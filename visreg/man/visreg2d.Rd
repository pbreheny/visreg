\name{visreg2d}
\alias{visreg2d}
\title{Visualization of regression functions for two variables}
\description{
A function used to visualize how two variables interact to affect the
response in regression models.
}
\usage{
visreg2d(fit, xvar, yvar, type=c("conditional","effect"),
nn=ifelse(plot.type=="persp",49,99), plot.type=c("image","persp","rgl"),
trans=as.numeric, scale=c("linear","response"), cond=list(),
whitespace=0.2, ...)
}
\arguments{
  \item{fit}{
This is the fitted model object you wish to visualize.
}
  \item{xvar}{
This is the variable to be put on the x-axis of your plot.  Both continuous variables and factors are supported.
}
  \item{yvar}{
This is the variable to be put on the y-axis of your plot.  Both continuous variables and factors are supported.
}
  \item{type}{
The type of plot to be produced.  The following options are supported:
\itemize{
\item
If 'conditional' is selected, the plot returned shows the value of the
variable on the x-axis and the change in response on the y-axis, holding
all other variables constant (median for numeric variables and most
common category for factors, by default).  See references for further detail.
\item
If 'effect' is selected, the plot returned shows the effect on the expecte value of the response by moving the x variable away from a reference point on the x-axis (median for numeric variables and most common category for factors, by default).  See references for further detail.
}
}
  \item{nn}{
Resolution of the three dimensional plot.  Higher values will results in a smoother looking plot.
}
  \item{plot.type}{
The style of plot to be produced.  The following three options are supported:
\itemize{
\item
image, a filled contour plot.
\item
persp, a 3 dimensional perspective plot.
\item
rgl, a version of the perspective plot that can be rotated.  Note: requires the rgl package to use.
}
}
  \item{trans}{
Allows transformations to the response.  A function should be passed.
}
  \item{scale}{
If response is selected and a glm is passed as the fit parameter, the response will automatically be transformed.
}
  \item{cond}{
Used to set variables to specific values.  Can also be used to select the reference point for an effect plot.  Named lists should be passed.  All variables left unspecified will be filled in with the median/most common category (see examples).}
  \item{whitespace}{
When x (or y) is a factor, whitespace determines the ammount of space in between factors on the x (or y) axis.
}
  \item{\dots}{
Graphical parameters can be passed to the function to customize the plots.
}
}
\value{In addition to providing plots, the visreg function also
  invisibly returns the data frames and estimates used in the creation
  of its plots.}
\references{
  Breheny, P. and Burchett, W. (2012), Visualizing regression models
  using visreg.
  \url{http://web.as.uky.edu/statistics/users/pbreheny/publications/visreg.pdf}
}
\author{
Patrick Breheny and Woodrow Burchett
}
\seealso{\code{visreg}}
\examples{
ozone <- airquality[complete.cases(airquality),]

fit <- lm(Ozone ~ Solar.R + Wind + Temp + I(Wind^2) + I(Temp^2) + I(Wind*Temp)+I(Wind*Temp^2) + I(Temp*Wind^2) + I(Temp^2*Wind^2),data=ozone)

visreg2d(fit,x="Wind",y="Temp",plot.type="image")
visreg2d(fit,x="Wind",y="Temp",plot.type="persp")

## Requires the rgl package
\dontrun{
visreg2d(fit,x="Wind",y="Temp",plot.type="rgl")
}

}
\keyword{models}
\keyword{regression}
\keyword{graphics}
