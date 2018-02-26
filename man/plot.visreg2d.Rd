\name{plot.visreg2d}
\alias{plot.visreg2d}
\title{Visualization of regression functions for two variables}
\description{Plot method for visualizing how two variables interact to
  affect the response in regression models.}
\usage{
\method{plot}{visreg2d}(x, plot.type=c("image","persp","rgl", "gg"),
xlab, ylab, zlab, color, print.cond=FALSE, whitespace=0.2, ...)
}
\arguments{
  \item{x}{
    A \code{\link{visreg2d}} object.
  }
  \item{plot.type}{
    The style of plot to be produced.  The following three options are
    supported:
    \itemize{
      \item \code{'image'}, a filled contour plot.
      \item \code{'gg'}, a filled contour plot using \code{ggplot2}.
      \item \code{'persp'}, a 3 dimensional perspective plot.
      \item \code{'rgl'}, a version of the perspective plot that can be
      rotated. Note: requires the rgl package to use.
    }
  }
  \item{xlab}{
    Axis label for x variable}
  \item{ylab}{
    Axis label for y variable}
  \item{zlab}{
    Axis label for outcome}
  \item{color}{
    For {\code{plot.type='persp'}} or {\code{plot.type='rgl'}}, the
    color of the surface.  For {\code{plot.type='image'}} or
    {\code{plot.type='gg'}}, a vector of colors used to establish a
    color palette.}
  \item{print.cond}{
    If \code{print.cond==TRUE}, the explanatory variable values
    conditioned on in a conditional plot are printed to the console
    (default: \code{FALSE}). If \code{print.cond==TRUE} and
    \code{type=="contrast"}, the conditions will still be printed, but
    they have no bearing on the plot unless interactions are present.
  }
  \item{whitespace}{
    When \code{xvar} or \code{yvar} is a factor, whitespace determines
    the amount of space in between the factors.  Default is 0.2, meaning
    that 20 percent of the axis is whitespace.
  }
  \item{\dots}{
    Graphical parameters can be passed to the function to customize the
    plots.
  }
}
\references{
  \itemize{
    \item \url{http://pbreheny.github.io/visreg}
    \item Breheny, P. and Burchett, W. (2017), Visualizing regression
    models using visreg.
    \url{https://journal.r-project.org/archive/2017/RJ-2017-046/index.html}
  }
}
\author{
  Patrick Breheny and Woodrow Burchett
}
\seealso{
  \url{http://pbreheny.github.io/visreg/surface.html}
  \code{\link{visreg}}
}
\examples{
fit <- lm(Ozone ~ Solar.R + Wind + Temp + I(Wind^2) + I(Temp^2) +
I(Wind*Temp)+I(Wind*Temp^2) + I(Temp*Wind^2) + I(Temp^2*Wind^2),
data=airquality)

visreg2d(fit, x="Wind", y="Temp", plot.type="image")
visreg2d(fit, x="Wind", y="Temp", plot.type="image",
         color=c("purple", "green", "red"))
visreg2d(fit, x="Wind", y="Temp", plot.type="persp")

## Requires the rgl package
\dontrun{
visreg2d(fit,x="Wind",y="Temp",plot.type="rgl")
}

## Requires the ggplot2 package
\dontrun{
visreg2d(fit, x="Wind", y="Temp", plot.type="gg")
}

}
