\name{visreg}
\alias{visreg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Visualization of regression functions for a single variable
}
\description{
A function used to visualize regression models quickly and easily.  Default plots contain a confidence band, prediction line, and partial residuals.  Factors, transformations, conditioning, interactions, and a variety of other options are supported.
}
\usage{
visreg(fit, xvar, by, breaks=4, type=c("conditional","effect"), trans=as.numeric,
scale=c("linear","response"), xtrans, alpha=.05, nn=101, cond=list(),
whitespace=0.2, partial=TRUE, jitter=FALSE, strip.names=FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fit}{
This is the fitted model object you wish to visualize.  lm, glm, gam, rlm, and coxph data classes are all supported.
}
  \item{xvar}{
This is the variable to be put on the x-axis of your plot.  Both continuous variables and factors are supported.
}
  \item{by}{
An option to use if you wish to divide your plot by a third variable.  Uses the lattice package.  Both continuous variables and factors are supported.
}
  \item{breaks}{
If a continuous variable is used for the 'by' option, the 'breaks' argument determines how many quantiles will be divided to be used for panels.
}
  \item{type}{
The type of plot to be produced.  The following options are supported:
\itemize{
\item
If 'conditional' is selected, the plot returned shows the value of the variable on the x-axis and the change in response on the y-axis, holding all other variables constant (median for numeric variables and most common category for factors, by default).
\item
If 'effect' is selected, the plot returned shows the effect on the expecte value of the response by moving the x variable away from a reference point on the x-axis (median for numeric variables and most common category for factors, by default).
}
}
  \item{trans}{
Allows transformations to the response.  A function should be passed (see examples).
}
  \item{scale}{
If response is selected and a glm is passed as the fit parameter, the response will automatically be transformed.
}
  \item{xtrans}{
Allows transformations to the x-axis.  A function should be passed.  Note that inverse transformations to explanatory variables are performed automatically by visreg.
}
  \item{alpha}{
Alpha level for the confidence band displayed in the plot.
}
  \item{nn}{
Controls the smoothness of the line and confidence band.  The higher the number, the smoother they will appear.
}
  \item{cond}{
Used to set variables to specific values.  Can also be used to select the reference point for an effect plot.  Named lists should be passed.  All variables left unspecified will be filled in with the median/most common category (see examples).
}
  \item{whitespace}{
When xvar is a factor, whitespace determines the ammount of space in between factors on the x-axis.
}
  \item{partial}{
Determines if partial residuals are shown on the plot.
}
  \item{jitter}{
Jitters xvar.  Default is FALSE.
}
  \item{strip.names}{
Adds names to strips.  Default is FALSE.
}
  \item{\dots}{
Graphical parameters can be passed to the function to customize the plots.  If you pass a 'by' argument, lattice parameters can be passed, such as layout (see examples below).
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
In addition to providing plots, the visreg function also invisibly returns the data frames, estimates, confidence intervals, and residuals used in the creation of its plots (see last example).
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
  Breheny, P. and Burchett, W. (2012), Visualizing regression models
  using visreg.
  \url{http://web.as.uky.edu/statistics/users/pbreheny/publications/visreg.pdf}
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
## Linear models

## Basic
fit <- lm(Ozone ~ Solar.R + Wind + Temp,data=airquality)
visreg(fit)
visreg(fit,"Wind",type="effect")
visreg(fit,"Wind",type="conditional")
visreg(fit,c("Solar.R","Wind","Temp"))

## Factors
airquality$Heat <- cut(airquality$Temp,3,labels=c("Cool","Mild","Hot"))
fit.heat <- lm(Ozone ~ Solar.R + Wind + Heat,data=airquality)
visreg(fit.heat,"Heat",type="effect")
visreg(fit.heat,"Heat",type="conditional")
## Use of whitespace option
visreg(fit.heat,"Heat",whitespace=.1)
visreg(fit.heat,"Heat",whitespace=.5)

## Transformations
fit1 <- lm(Ozone ~ Solar.R + Wind + Temp + I(Wind^2),data=airquality)
fit2 <- lm(log(Ozone) ~ Solar.R + Wind + Temp,data=airquality)
fit3 <- lm(log(Ozone) ~ Solar.R + Wind + Temp + I(Wind^2),data=airquality)
visreg(fit1,"Wind")
visreg(fit2,"Wind",trans=exp,ylab="Ozone")
visreg(fit3,"Wind",trans=exp,ylab="Ozone")

## Conditioning
visreg(fit,"Wind",cond=list(Temp=50))
visreg(fit,"Wind")
visreg(fit,"Wind",cond=list(Temp=100))

## Interactions
fit.in1 <- lm(Ozone~ Solar.R + Wind*Heat,data=airquality)
visreg(fit.in1,"Wind",by="Heat")
visreg(fit.in1,"Heat",by="Wind")
visreg(fit.in1,"Wind",by="Heat",type="effect")
visreg(fit.in1,"Wind",cond=list(Heat="Cool"),type="effect")
visreg(fit.in1,"Wind",cond=list(Heat="Hot"),type="effect")


## Nonlinear models

data("birthwt",package="MASS")
birthwt$race <- factor(birthwt$race,labels=c("White","Black","Other"))
birthwt$smoke <- factor(birthwt$smoke,labels=c("Nonsmoker","Smoker"))

fit <- glm(low~age+race+smoke+lwt,data=birthwt,family="binomial")
visreg(fit,"lwt",xlab="Mother's Weight",ylab="Log odds (low birthweight)")
visreg(fit,"lwt",scale="response",partial=FALSE,xlab="Mother's Weight",ylab="P(low birthweight)")

require(survival)
data(ovarian)
ovarian$rx <- factor(ovarian$rx)

fit <- coxph(Surv(futime,fustat)~age+rx,data=ovarian)

visreg(fit,"age",ylab="log(Hazard ratio)")

require(MASS)
fit <- rlm(Ozone ~ Solar.R + Wind*Heat,data=airquality)
visreg(fit,"Wind",cond=list(Heat="Mild"))

v <- visreg(fit,"Wind",cond=list(Heat="Mild"))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
