\name{visreg2d}
\alias{visreg2d}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Visualization of regression functions for two variables
}
\description{
A function used to visualize how two variables interact to affect the response in regression models.
}
\usage{
visreg2d(fit, x, y, fill = c("mean", "zero", "median"), type = c("response", "terms"), res = ifelse(type == "persp", 49, 99), plot.type = c("image", "persp", "rgl"), trans = as.matrix, cond = list(), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fit}{
This is the model you wish to visualize.
}
  \item{x}{
Variable from the model to be put on the x-axis.
}
  \item{y}{
Variable from the model to be put on the y-axis.
}
  \item{fill}{
This argument determines the values of all variables other than x and y.  Possible options are mean, zero, and median.
}
  \item{type}{
The type of plot to be produced.  The following options are supported:
\itemize{
\item
If 'response' is selected, the plot returned shows the value of the variable on the x-axis and the change in response on the y-axis, holding all other variables constant.
\item
If 'terms' is selected, the plot returned shows the effect on the response by moving the selected variable away from the mean on the y-axis.
}
}
  \item{res}{
Resolution of the three dimensional plot.  Higher values will results in a smoother looking plot.
}
  \item{plot.type}{
The style of plot to be produced.  The following three options are supported:
\itemize{
\item
image, a colored contour plot.
\item
persp, a 3 dimensional perspective plot.
\item
rgl, a version of the perspective plot that can be rotated.  Note: requires the rgl package to use.
}
}
  \item{trans}{
Allows transformations to the response.  A function should be passed.
}
  \item{cond}{
Used to set variables to specific values.  Will override the fill parameter.  Lists should be passed.
}
  \item{\dots}{
Graphical parameters can be passed to the function to customize the plots.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Patrick Breheny and Woodrow Burchett
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

ozone <- airquality[complete.cases(airquality),]

fit <- lm(Ozone ~ Solar.R + Wind + Temp + I(Wind^2) + I(Temp^2) + I(Wind*Temp)+I(Wind*Temp^2) + I(Temp*Wind^2) + I(Temp^2*Wind^2),data=ozone)

visreg2d(fit,x="Wind",y="Temp")
visreg2d(fit,x="Wind",y="Temp",type="terms")
visreg2d(fit,x="Wind",y="Temp",plot.type="persp",theta=-30,ticktype="detailed",col="blue",shade=.7)

## Requires the rgl package
\dontrun{
visreg2d(fit,x="Wind",y="Temp",plot.type="rgl")
}

visreg2d(fit,x="Wind",y="Temp",cond=list('Solar.R'=500))

fit <- lm(log(Ozone) ~ Solar.R + Wind + Temp + I(Wind^2) + I(Temp^2) + I(Wind*Temp)+I(Wind*Temp^2) + I(Temp*Wind^2) + I(Temp^2*Wind^2),data=ozone)
visreg2d(fit,x="Wind",y="Temp",trans=exp)

## Tests for factors

ozone$Heat <- cut(ozone$Temp,3,labels=c("Cool","Mild","Hot"))	
ozone$Windy <- cut(ozone$Wind,2,labels=c("Windy","NotWindy"))

fit <- lm(Ozone ~ Solar.R + Wind + Heat,data=ozone)
visreg2d(fit,x="Wind",y="Solar.R")
visreg2d(fit,x="Wind",y="Solar.R",cond=list('Heat'='Hot'))
visreg2d(fit,x="Wind",y="Solar.R",cond=list('Heat'='Cool'))

visreg2d(fit,x="Wind",y="Heat")
visreg2d(fit,x="Heat",y="Wind")

fit <- lm(Ozone ~ Solar.R + Windy + Heat + Month,data=ozone)
visreg2d(fit,x="Heat",y="Windy")
visreg2d(fit,x="Solar.R",y="Windy")
visreg2d(fit,x="Heat",y="Solar.R")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
