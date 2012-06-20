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
visreg(fit, xvar, by, breaks = 4, fill = c("mean", "zero", "median"), type = c("response", "terms"), trans = as.numeric, scale = c("linear", "response"), xtrans = as.numeric, alpha = 0.05, nn = 101, cond = list(), whitespace = 0.2, partial = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fit}{
This is the model you wish to visualize.  lm, glm, and coxph data classes are all supported.
}
  \item{xvar}{
This is the variable to be put on the x-axis of your plot.  Both continuous variables and factors are supported.
}
  \item{by}{
An option to use if you wish to divide your plot by a third variable.  Uses the lattice package.  Both continuous variables and factors are supported.
}
  \item{breaks}{
If a continuous variable is used for the 'by' option, the 'breaks' argument determines how many panels to view.
}
  \item{fill}{
This argument determines the values of all variables other than xvar and by.  Possible options are mean, zero, and median.
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
  \item{trans}{
Allows transformations to the response.  A function should be passed (see examples).
}
  \item{scale}{
If response is selected and a glm is passed as the fit parameter, the response will automatically be transformed.
}
  \item{xtrans}{
Allows transformations to the x-axis.  A function should be passed.
}
  \item{alpha}{
Alpha level for the confidence band displayed in the plot.
}
  \item{nn}{
Controls the smoothness of the line and confidence band.  The higher the number, the smoother they will appear.
}
  \item{cond}{
Used to set variables to specific values.  Will override the fill parameter.  Lists should be passed (see examples).
}
  \item{whitespace}{
When xvar is a factor, whitespace determines the ammount of space in between factors.
}
  \item{partial}{
Determines if partial residuals are shown on the plot.
}
  \item{\dots}{
Graphical parameters can be passed to the function to customize the plots.  If you pass a 'by' argument, lattice parameters can be passed, such as layout (see examples below).
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
## Linear models

ozone <- airquality[complete.cases(airquality),]

## Basic
fit <- lm(Ozone ~ Solar.R + Wind + Temp,data=ozone)
visreg(fit,"Wind")
visreg(fit,"Wind",type="terms")
visreg(fit,c("Solar.R","Wind","Temp"))

## Transformations of X
fit <- lm(Ozone ~ Solar.R + Wind + Temp + I(Wind^2),data=ozone)
visreg(fit,"Wind")
visreg(fit,"Wind",type="terms")

## Transformations of y
fit <- lm(log(Ozone) ~ Solar.R + Wind + Temp,data=ozone)
visreg(fit,"Wind",trans=exp)

fit <- lm(sqrt(Ozone) ~ Solar.R + Wind + Temp,data=ozone)
sqr = function(x){ return(x^2)}
visreg(fit,"Wind",trans=sqr)

## Cond
visreg(fit,"Wind",cond=list('Temp'=100))
## Should be same plot:
visreg(fit,"Wind",cond=list('Temp'=0,'Solar.R'=0))
visreg(fit,"Wind",fill='zero')

## Factors
ozone$Heat <- cut(ozone$Temp,3,labels=c("Cool","Mild","Hot"))
fit <- lm(Ozone ~ Solar.R + Wind + Heat,data=ozone)
visreg(fit,"Wind")
visreg(fit,"Wind",cond=list(Heat='Mild')) ## Should be same as above
visreg(fit,"Wind",type="terms")
visreg(fit,"Wind",cond=list(Solar.R=250))
visreg(fit,"Wind",cond=list(Heat = 'Cool'))
visreg(fit,"Heat")
visreg(fit,"Heat")
## Use of whitespace option
visreg(fit,"Heat",whitespace=.1)
visreg(fit,"Heat",whitespace=.5)

## Plotting options
visreg(fit,"Heat",whitespace=.1,xlab="Heat Category")

## Generalized linear models

data("birthwt",package="MASS")
birthwt$race <- factor(birthwt$race,labels=c("White","Black","Other"))
birthwt$smoke <- factor(birthwt$smoke,labels=c("Nonsmoker","Smoker"))

## Basic
fit <- glm(low~age+race+smoke+lwt,data=birthwt,family="binomial")
visreg(fit,"age")
visreg(fit,"lwt")
visreg(fit,"race")
visreg(fit,"smoke")

## Transformation of X
fit <- glm(low~age+race+smoke+lwt,data=birthwt,family="binomial")
visreg(fit,"age")
visreg(fit,"age",type="terms")

## Plot on response scale
fit <- glm(low~age+race+smoke+lwt,data=birthwt,family="binomial")
visreg(fit,"age",scale="response")
visreg(fit,"lwt",scale="response")

## Cond
visreg(fit,"lwt",trans=inv.logit,cond=list(smoke='Smoker'))

## Factors
visreg(fit,"race",cond=list(smoke='Smoker'))

## Interactions and use of the 'by' options

require(lattice)
ozone <- airquality[complete.cases(airquality),]
ozone$Heat <- cut(ozone$Temp,3,labels=c("Cool","Mild","Hot"))
fit <- lm(Ozone ~ Wind*Heat,data=ozone)

visreg(fit,"Wind",by="Heat")
visreg(fit,"Wind",by="Heat",layout=c(3,1))

# fixed whitespace and x-axis
visreg(fit,"Heat",by="Wind")
visreg(fit,"Heat",by="Wind",breaks=9)

fit <- lm(Ozone ~ Wind*Solar.R,data=ozone)
visreg(fit,"Wind",by="Solar.R")
visreg(fit,"Wind",by="Solar.R",breaks=9)

fit <- lm(log(Ozone) ~ Solar.R + Wind + Temp + Wind*Temp + Wind*Solar.R,data=ozone)
visreg(fit,"Wind",by='Temp',,breaks=9,trans=exp)
visreg(fit,"Wind",by='Solar.R',trans=exp)

## Survival data applications

require(survival)
data(ovarian)
ovarian$rx <- factor(ovarian$rx)

fit <- coxph(Surv(futime,fustat)~age+rx,data=ovarian)

visreg(fit,"age")
visreg(fit,"rx")
visreg(fit,"age",trans=exp)

fit <- coxph(Surv(futime,fustat)~age*rx,data=ovarian)

par(mfrow=c(1,2))
visreg(fit,"age",cond=list(rx="1"))
visreg(fit,"age",cond=list(rx="2"))
visreg2d(fit,x="age",y="rx")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
