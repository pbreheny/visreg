\name{visreg2d}
\alias{visreg2d}
\title{Visualization of regression functions for two variables}
\description{
A function used to visualize how two variables interact to affect the
response in regression models.
}
\usage{
visreg2d(fit, xvar, yvar, type=c("conditional", "contrast", "effect"),
trans=I, scale=c("linear","response"),
plot.type=c("image","persp","rgl"), nn=ifelse(plot.type=="persp",49,99),
cond=list(), print.cond=FALSE, whitespace=0.2, ...)
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
  \item{yvar}{
    Character string specifying the variable to be put on the y-axis of
    your plot.  Both continuous variables and factors are supported.
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
  \item{plot.type}{
    The style of plot to be produced.  The following three options are
    supported:
    \itemize{
      \item \code{'image'}, a filled contour plot.
      \item \code{'persp'}, a 3 dimensional perspective plot.
      \item \code{'rgl'}, a version of the perspective plot that can be rotated.
        Note: requires the rgl package to use.
    }
  }
  \item{nn}{
    Resolution of the three dimensional plot.  Higher values will
    results in a smoother looking plot.
  }
  \item{cond}{
    Named list specifying conditional values of other explanatory variables.
    By default, conditional plots in visreg are constructed by filling in 
    other explanatory variables with the median (for numeric variables) or 
    most common category (for factors), but this can be overridden by specifying
    their values using \code{cond} (see examples).
  }
  \item{print.cond}{
    If \code{print.cond==TRUE}, the explanatory variable values conditioned on
    in a conditional plot are printed to the console (default: \code{FALSE}).
    If \code{print.cond==TRUE} and \code{type=="contrast"}, the conditions will
    still be printed, but they have no bearing on the plot unless interactions
    are present.
  }
  \item{whitespace}{
    When \code{xvar} or \code{yvar} is a factor, whitespace determines
    the amount of space in between the factors.  Default is 0.2, meaning
    that 20 percent of the axis is whitespace.
  }
  \item{\dots}{
    Graphical parameters can be passed to the function to customize the plots.
  }
}
\value{In addition to providing plots, the \code{visreg2d} function also
  invisibly returns the x, y, and Z values used in the creation of its plots.}
\references{
  Breheny, P. and Burchett, W. (2012), Visualizing regression models
  using visreg.
  \url{http://web.as.uky.edu/statistics/users/pbreheny/publications/visreg.pdf}
}
\author{
  Patrick Breheny and Woodrow Burchett
}
\seealso{\code{\link{visreg}}}
\examples{
fit <- lm(Ozone ~ Solar.R + Wind + Temp + I(Wind^2) + I(Temp^2) +
I(Wind*Temp)+I(Wind*Temp^2) + I(Temp*Wind^2) + I(Temp^2*Wind^2),
data=airquality)

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
