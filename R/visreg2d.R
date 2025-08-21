#' Visualization of regression functions for two variables
#' 
#' A function used to visualize how two variables interact to affect the
#' response in regression models.
#' 
#' 
#' @param fit The fitted model object you wish to visualize.  Any object with
#' 'predict' and 'model.frame' methods are supported, including lm, glm, gam,
#' rlm, coxph, and many more.
#' @param xvar Character string specifying the variable to be put on the x-axis
#' of your plot.  Both continuous variables and factors are supported.
#' @param yvar Character string specifying the variable to be put on the y-axis
#' of your plot.  Both continuous variables and factors are supported.
#' @param type The type of plot to be produced.  The following options are
#' supported: \itemize{ \item If 'conditional' is selected, the plot returned
#' shows the value of the variable on the x-axis and the change in response on
#' the y-axis, holding all other variables constant (by default, median for
#' numeric variables and most common category for factors).  \item If
#' 'contrast' is selected, the plot returned shows the effect on the expected
#' value of the response by moving the x variable away from a reference point
#' on the x-axis (for numeric variables, this is taken to be the mean).  } For
#' more details, see references.
#' @param data The data frame used to fit the model.  Typically, visreg() can
#' figure out where the data is, so it is not necessary to provide this.  In
#' some cases, however, the data set cannot be located and must be supplied
#' explicitly.
#' @param trans (Optional) A function specifying a transformation for the
#' vertical axis.
#' @param scale By default, the model is plotted on the scale of the linear
#' predictor.  If \code{scale='response'} for a glm, the inverse link function
#' will be applied so that the model is plotted on the scale of the original
#' response.
#' @param nn Resolution of the three dimensional plot.  Higher values will
#' results in a smoother looking plot.
#' @param cond Named list specifying conditional values of other explanatory
#' variables. By default, conditional plots in visreg are constructed by
#' filling in other explanatory variables with the median (for numeric
#' variables) or most common category (for factors), but this can be overridden
#' by specifying their values using \code{cond} (see examples).
#' @param plot Send the calculations to `[plot.visreg2d()]`, producing
#' a plot?  Default is TRUE.
#' @param \dots Graphical parameters (e.g., `ylab`) can be passed to the
#' function to customize the plots.
#' 
#' @returns A `visreg2d` object consisting of:
#' \item{x}{Values of `xvar` to be plotted}
#' \item{y}{Values of \code{yvar} to be plotted}
#' \item{z}{Values of outcome to be plotted}
#' \item{meta}{Meta-information needed to construct plots, such as the name of the x
#'   and y variables.}
#' 
#' @author Patrick Breheny and Woodrow Burchett
#' @seealso \url{https://pbreheny.github.io/visreg/articles/surface.html} `[visreg()]`
#' @references \itemize{
#'   \item \url{https://pbreheny.github.io/visreg/}
#'   \item Breheny, P. and Burchett, W. (2017), Visualizing regression models using visreg.
#'     \url{https://journal.r-project.org/archive/2017/RJ-2017-046/index.html}
#' }
#' 
#' @examples
#' fit <- lm(Ozone ~ Solar.R + Wind + Temp + I(Wind^2) + I(Temp^2) +
#' I(Wind*Temp)+I(Wind*Temp^2) + I(Temp*Wind^2) + I(Temp^2*Wind^2),
#' data=airquality)
#' 
#' visreg2d(fit, x="Wind", y="Temp", plot.type="image")
#' visreg2d(fit, x="Wind", y="Temp", plot.type="persp")
#' 
#' ## Requires the rgl package
#' \dontrun{
#' visreg2d(fit, x="Wind", y="Temp", plot.type="rgl")
#' }
#' 
#' ## Requires the ggplot2 package
#' \dontrun{
#' visreg2d(fit, x="Wind", y="Temp", plot.type="gg")
#' }
#' @export visreg2d

visreg2d <- function(fit, xvar, yvar, type=c("conditional", "contrast"), data=NULL, trans=I, scale=c("linear","response"), nn=99, cond=list(), plot=TRUE, ...) {
  # Setup
  if (type[1]=="effect") {
    warning("Please note that type='effect' is deprecated and may not be supported in future versions of visreg.  Use type='contrast' instead.")
    type <- "contrast"
  }
  type <- match.arg(type)
  scale <- match.arg(scale)
  if (scale=="response") trans <- family(fit)$linkinv
  if (missing(xvar) | missing(yvar)) stop("Must specify and x and y variable", call.=FALSE)
  if (!identical(trans, I) & type=="contrast") warning("You are attempting to transform a contrast.  The resulting plot is not guaranteed to be meaningful.", call.=FALSE)
  
  # Set up f
  f <- setupF(fit, c(xvar, yvar), parent.frame(), data)
  if (attr(f, "needsUpdate")) fit <- update(fit, data=f)
  cond <- setupCond(cond, f)[[1]]

  # Calculate v
  v <- setupV2(fit, f, xvar, yvar, nn, cond, type, scale, trans)

  # Plot/return
  if (plot) {
    p <- plot(v, ...)
    if (!is.null(p) && inherits(p, 'gg') || inherits(p, 'list')) return(p)
  }
  invisible(v)
}
