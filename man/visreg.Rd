\name{visreg}
\alias{visreg}
\title{Visualization of regression functions for a single variable}
\description{
A function for visualizing regression models quickly and easily.
Default plots contain a confidence band, prediction line, and partial
residuals.  Factors, transformations, conditioning, interactions, and a
variety of other options are supported. 
}
\usage{
visreg(fit, xvar, by, overlay=FALSE, breaks=3, type=c("conditional",
"contrast", "effect"), trans=I, scale=c("linear","response"), xtrans,
alpha=.05, nn=101, cond=list(), print.cond=missing(by) &
(max(attr(terms(formula(fit)), "order"))>1), whitespace=0.2,
partial=(!rug), band=TRUE, rug=FALSE, jitter=FALSE,
strip.names=is.numeric(attr(v, "lev")), legend=TRUE, ask=TRUE,
line.par=NULL, fill.par=NULL, points.par=NULL, ...) 
}
\arguments{
  
  \item{fit}{
    The fitted model object you wish to visualize.  Any object with 
    'predict' and 'model.frame' methods are supported, including lm, glm,
    gam, rlm, coxph, and many more.
  }

  \item{xvar}{
    Character string specifying the variable to be put on the x-axis of
    your plot.  Both continuous variables and factors are supported.
  }

  \item{by}{
    (Optional) A variable allowing you to divide your plot into
    cross-sections based on levels of the \code{by} variable;
    particularly useful for visualizing models with interactions.
    Supplied as a character string.  Uses the lattice package.  Both
    continuous variables and factors are supported.
  }
  
  \item{overlay}{
    When \code{by} is specified, by default separate panels are used to
    display each cross-section.  If \code{overlay=TRUE}, these cross-sections
    are overlaid on top of each other in a single plot.
  }
  
  \item{breaks}{
    If a continuous variable is used for the \code{by} option, the \code{breaks}
    argument controls the values at which the cross-sections are taken.
    By default, cross-sections are taken at the 10th, 50th, and 90th quantiles.
    If \code{breaks} is a single number, it specifies the number of breaks.
    If \code{breaks} is a vector of numbers, it specifies the values
    at which the cross-sections are to be taken.
  }
  
  \item{type}{
    The type of plot to be produced.  The following options are
    supported:
    \itemize{
      \item If 'conditional' is selected, the plot returned shows the
      value of the variable on the x-axis and the change in response on
      the y-axis, holding all other variables constant (by default, median
      for numeric variables and most common category for factors).
      \item If 'contrast' is selected, the plot returned shows the effect
      on the expected value of the response by moving the x variable away
      from a reference point on the x-axis (for numeric variables, this is
      taken to be the mean).
    }
    For more details, see references.
  }
  \item{trans}{
    (Optional) A function specifying a transformation for the vertical axis.
  }
  \item{scale}{
    By default, the model is plotted on the scale of the linear
    predictor.  If \code{scale='response'} for a glm, the inverse link
    function will be applied so that the model is plotted on the scale
    of the original response.
  }
  \item{xtrans}{
    (Optional) A function specifying a transformation for the horizontal axis.
    Note that, for model terms such as \code{log(x)}, visreg automatically
    plots on the original axis (see examples).
  }
  \item{alpha}{
    Alpha level (1-coverage) for the confidence band displayed in the plot
    (default: 0.05).
  }
  \item{nn}{
    Controls the smoothness of the line and confidence band.  Increasing
    this number will add to the computational burden, but produce a smoother
    plot (default: 101).
  }
  \item{cond}{
    Named list specifying conditional values of other explanatory variables.
    By default, conditional plots in visreg are constructed by filling in 
    other explanatory variables with the median (for numeric variables) or 
    most common category (for factors), but this can be overridden by specifying
    their values using \code{cond} (see examples).
  }
  \item{print.cond}{
    If \code{print.cond=TRUE}, the explanatory variable values conditioned on
    in a conditional plot are printed to the console (default: \code{FALSE}).
    If \code{print.cond=TRUE} and \code{type="contrast"}, the conditions will
    still be printed, but they have no bearing on the plot unless interactions
    are present.
  }  
  \item{whitespace}{
    When \code{xvar} is a factor, whitespace determines the amount of space in
    between factors on the x-axis.  Default is 0.2, meaning that 20 percent of
    the horizontal axis is whitespace.
  }
  \item{partial}{
    If \code{partial=TRUE} (the default), partial residuals are shown on
    the plot. 
  }
  \item{band}{
    If \code{band=TRUE} (the default), confidence bands are shown on the plot.
  }
  \item{rug}{
    By default, partial residuals are plotted.  Alternatively, a
    \code{\link{rug}} may be plotted along the horizontal axis instead.
    Setting \code{rug=TRUE} turns off partial residuals by default; if
    one wants both to be plotted, both \code{rug=TRUE} and
    \code{partial=TRUE} need to be specified.  Two types of rug plots
    are available.  If \code{rug=1} or \code{rug=TRUE}, then a basic rug
    is drawn on the bottom.  If \code{rug=2}, then separate rugs are
    drawn on the top for observations with positive residuals and on the
    bottom for observations with negative residuals.  Such plots are
    particularly useful in logistic regression (see examples).
  }
  \item{jitter}{
    Adds a small amount of noise to \code{xvar}.  Potentially useful if many
    observations have exactly the same value.  Default is FALSE.
  }
  \item{strip.names}{
    When \code{by=TRUE}, adds the name of the \code{by} variable to the
    strip at the top of each panel.  Default is \code{FALSE}.
  }
  \item{legend}{
    For overlay plots, (\code{overlay=TRUE}), should visreg create a
    legend?  If \code{legend=TRUE} (the default), a legend is placed in
    the top margin.
  }
  \item{ask}{
    By default (\code{ask=TRUE}), if multiple plots are requested (for
    example, by passing a vector to \code{xvar}) and the result would
    cause some of the graphs to be on one figure and others on a
    different figure, \code{visreg} will prompt the user before moving
    to the next figure.  The prompting can be turned off with
    \code{ask=FALSE}.
  }
  \item{line.par}{
    List of parameters (see \code{\link{par}}) to pass to \code{lines(...)}
    when lines are drawn in the plots.
  }
  \item{fill.par}{
    List of parameters (see \code{\link{par}}) to pass to \code{polygon(...)}
    when shaded confidence regions are drawn in the plots.
  }
  \item{points.par}{
    List of parameters (see \code{\link{par}}) to pass to \code{points(...)}
    when partial residuals are drawn in the plots.
  }
  \item{\dots}{
    Graphical parameters can be passed to the function to customize the
    plots.  If \code{by=TRUE}, lattice parameters can be passed, such as
    layout (see examples below).
  }
}
\value{
  In addition to providing plots, the visreg function also invisibly
  returns the data frames, estimates, confidence intervals, and
  residuals used in the creation of its plots (see last example).
}
\references{
  Breheny, P. and Burchett, W. (2012), Visualizing regression models
  using visreg.
  \url{http://web.as.uky.edu/statistics/users/pbreheny/publications/visreg.pdf}
}
\author{
  Patrick Breheny and Woodrow Burchett
}
\seealso{\code{\link{visreg2d}}}
\examples{
###################
## Linear models ##
###################

## Basic
fit <- lm(Ozone ~ Solar.R + Wind + Temp,data=airquality)
visreg(fit)
visreg(fit, "Wind", type="contrast")
visreg(fit, "Wind", type="conditional")
visreg(fit, "Wind", type="conditional", rug=TRUE)
visreg(fit,c("Solar.R","Wind","Temp"))

## Factors
airquality$Heat <- cut(airquality$Temp,3,labels=c("Cool","Mild","Hot"))
fit.heat <- lm(Ozone ~ Solar.R + Wind + Heat,data=airquality)
visreg(fit.heat,"Heat",type="contrast")
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
visreg(fit, "Wind", cond=list(Temp=50))
visreg(fit, "Wind", print.cond=TRUE)
visreg(fit, "Wind", cond=list(Temp=100))

## Interactions
fit.in1 <- lm(Ozone~ Solar.R + Wind*Heat,data=airquality)
visreg(fit.in1, "Wind", by="Heat")
visreg(fit.in1, "Heat", by="Wind")
visreg(fit.in1, "Wind", by="Heat", type="contrast")
visreg(fit.in1, "Wind", cond=list(Heat="Cool"), type="contrast")
visreg(fit.in1, "Wind", cond=list(Heat="Hot"), type="contrast")
visreg(fit.in1, "Heat", by="Wind", breaks=6)
visreg(fit.in1, "Heat", by="Wind", breaks=c(0,10,20))

## Overlay
visreg(fit.in1, "Wind", by="Heat", overlay=TRUE)
visreg(fit.in1, "Wind", by="Heat", overlay=TRUE, partial=FALSE)
visreg(fit.in1, "Wind", by="Heat", overlay=TRUE, partial=FALSE, band=FALSE)
visreg(fit.in1, "Wind", by="Heat", overlay=TRUE, partial=FALSE,
band=FALSE, type="contrast")

## Changing appearance
visreg(fit, "Wind", line=list(col="blue"), points=list(cex=1, pch=1))

######################
## Nonlinear models ##
######################

## Logistic regression
data("birthwt",package="MASS")
birthwt$race <- factor(birthwt$race,labels=c("White","Black","Other"))
birthwt$smoke <- factor(birthwt$smoke,labels=c("Nonsmoker","Smoker"))
fit <- glm(low~age+race+smoke+lwt,data=birthwt,family="binomial")
visreg(fit,"lwt",xlab="Mother's Weight",ylab="Log odds (low birthweight)")
visreg(fit, "lwt", scale="response", partial=FALSE, xlab="Mother's
Weight", ylab="P(low birthweight)")
visreg(fit, "lwt", scale="response", partial=FALSE, xlab="Mother's
Weight", ylab="P(low birthweight)", rug=2)

## Proportional hazards
require(survival)
data(ovarian)
ovarian$rx <- factor(ovarian$rx)
fit <- coxph(Surv(futime,fustat)~age+rx,data=ovarian)
visreg(fit,"age",ylab="log(Hazard ratio)")

## Robust regression
require(MASS)
fit <- rlm(Ozone ~ Solar.R + Wind*Heat,data=airquality)
visreg(fit,"Wind",cond=list(Heat="Mild"))

## And more...; anything with a 'predict' method should work

## Return raw components of plot
v <- visreg(fit,"Wind",cond=list(Heat="Mild"))
}
\keyword{models}
\keyword{regression}
\keyword{graphics}
