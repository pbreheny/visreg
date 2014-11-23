\name{plot.visreg}
\alias{plot.visreg}
\title{Visualization of regression functions}
\description{
A function for visualizing regression models quickly and easily.
Default plots contain a confidence band, prediction line, and partial
residuals.  Factors, transformations, conditioning, interactions, and a
variety of other options are supported.  The \code{plot.visreg} function
accepts a \code{visreg} or \code{visreg.list} object as calculated by
\code{visreg} and creates the plot.
}
\usage{
\method{plot}{visreg}(x, overlay=FALSE, print.cond=FALSE,
whitespace=0.2, partial=identical(x$meta$trans, I), band=TRUE,
rug=!partial, strip.names=is.numeric(x$fit[,x$meta$by]), legend=TRUE,
line.par=NULL, fill.par=NULL, points.par=NULL, ...)
}
\arguments{
  
  \item{x}{
    A \code{visreg} or \code{visreg.list} object; see
    \code{\link{visreg}}.
  }

  \item{overlay}{
    When \code{by} is specified, by default separate panels are used to
    display each cross-section.  If \code{overlay=TRUE}, these cross-sections
    are overlaid on top of each other in a single plot.
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
  
  \item{strip.names}{
    When \code{by=TRUE}, \code{strip.names=TRUE} adds the name of the
    \code{by} variable to the strip at the top of each panel.  Default
    is \code{FALSE} for factors and \code{TRUE} for numeric \code{by}
    variables.  \code{strip.names} can also be a character vector, in
    which case it replaces the strip names altogether with values chosen
    by the user.
  }
  
  \item{legend}{
    For overlay plots, (\code{overlay=TRUE}), should visreg create a
    legend?  If \code{legend=TRUE} (the default), a legend is placed in
    the top margin.
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
\references{
  Breheny, P. and Burchett, W. (2012), Visualizing regression models
  using visreg.
  \url{http://myweb.uiowa.edu/pbreheny/publications/visreg.pdf}
}
\author{
  Patrick Breheny and Woodrow Burchett
}
\seealso{
  \code{\link{visreg}}
  \code{\link{visreg2d}}
  \code{\link{visreg-faq}}
}
\examples{
fit <- lm(Ozone ~ Solar.R + Wind + Temp,data=airquality)
visreg(fit, "Wind", line=list(col="red"), points=list(cex=1, pch=1))
## See ?visreg for more examples
}
\keyword{graphics}
