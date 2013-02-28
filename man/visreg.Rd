\name{visreg}
\alias{visreg}
\title{Visualization of regression functions for a single variable}
\description{
A function used to visualize regression models quickly and easily.  Default plots contain a confidence band, prediction line, and partial residuals.  Factors, transformations, conditioning, interactions, and a variety of other options are supported.
}
\usage{
visreg(fit, xvar, by, breaks=4, type=c("conditional","effect"), trans=I, scale=c("linear","response"), xtrans, alpha=.05, nn=101, cond=list(), whitespace=0.2, partial=TRUE, jitter=FALSE, strip.names=FALSE, line.par=NULL, fill.par=NULL, points.par=NULL, ...)
}
\arguments{
  
  \item{fit}{
    The fitted model object you wish to visualize.  lm, glm,
    gam, rlm, and coxph data classes are all supported.
  }

  \item{xvar}{
    The variable to be put on the x-axis of your plot, supplied as a
    character string (see examples).  Both continuous variables and
    factors are supported.
  }

  \item{by}{
    (Optional) A variable allowing you to divide your plot into
    cross-sections bases on levels of the \code{by} variable,
    particularly useful for visualizing models with interactions.
    Supplied as a character string.  Uses the lattice package.  Both
    continuous variables and factors are supported.
  }
  \item{breaks}{
    If a continuous variable is used for the 'by' option, the 'breaks'
    argument determines how many quantiles will be divided to be used
    for panels.
  }
  \item{type}{
    The type of plot to be produced.  The following options are
    supported:
    \itemize{
      \item If 'conditional' is selected, the plot returned shows the
    value of the variable on the x-axis and the change in response on
    the y-axis, holding all other variables constant (median for numeric
    variables and most common category for factors, by default).  For
    more detail, see references.
      \item If 'effect' is selected, the plot returned shows the effect
    on the expected value of the response by moving the x variable away
    from a reference point on the x-axis (median for numeric variables
    and most common category for factors, by default).  For more detail,
    see references.
    }
  }
  \item{trans}{
    Allows transformations to the response.  A function should be passed
    (see examples).
  }
  \item{scale}{
    If response is selected and a glm is passed as the fit parameter,
    the response will automatically be transformed.
  }
  \item{xtrans}{
    Allows transformations to the x-axis.  A function should be passed.
    Note that inverse transformations to explanatory variables are
    performed automatically byvisreg.
  }
  \item{alpha}{
    Alpha level for the confidence band displayed in the plot.
  }
  \item{nn}{
    Controls the smoothness of the line and confidence band.  The higher
    the number, the smoother they will appear.
  }
  \item{cond}{
    Used to set variables to specific values.  Can also be used to
    select the reference point for an effect plot.  Named lists should
    be passed.  All variables left unspecified will be filled in with
    the median/most common category (see examples).
  }
  \item{whitespace}{
    When xvar is a factor, whitespace determines the ammount of space in
    between factors on the x-axis.
  }
  \item{partial}{
    Determines if partial residuals are shown on the plot.
  }
  \item{jitter}{
    Adds a small amount of noise to \code{xvar}.  Potentially useful if many
    observations have exactly the same value.  Default is FALSE.
  }
  \item{strip.names}{
    When \code{by=TRUE}, adds the name of the \code{by} variable to the
    strip at the top of each panel.  Default is FALSE.
  }
  \item{line.par}{
    List of parameters (see \code{par}) to pass to \code{lines(...)}
    when lines are drawn in the plots.
  }
  \item{fill.par}{
    List of parameters (see \code{par}) to pass to \code{polygon(...)}
    when shaded confidence regions are drawn in the plots.
  }
  \item{points.par}{
    List of parameters (see \code{par}) to pass to \code{points(...)}
    when partial residuals are drawn in the plots.
  }
  \item{\dots}{
    Graphical parameters can be passed to the function to customize the
    plots.  If \code{by=TRUE}, lattice parameters can be passed, such as
    layout (see examples below).
  }
}
\value{
In addition to providing plots, the visreg function also invisibly returns the data frames, estimates, confidence intervals, and residuals used in the creation of its plots (see last example).
}
\references{
  Breheny, P. and Burchett, W. (2012), Visualizing regression models
  using visreg.
  \url{http://web.as.uky.edu/statistics/users/pbreheny/publications/visreg.pdf}
}
\author{
Patrick Breheny and Woodrow Burchett
}
\seealso{\code{visreg2d}}
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

## Changing appearance
visreg(fit, "Wind", line=list(col="blue"), points=list(cex=1, pch=1))

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
\keyword{models}
\keyword{regression}
\keyword{graphics}
