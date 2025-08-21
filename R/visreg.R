#' Visualization of regression functions
#' 
#' A function for visualizing regression models quickly and easily. Default
#' plots contain a confidence band, prediction line, and partial residuals.
#' Factors, transformations, conditioning, interactions, and a variety of other
#' options are supported.  The \code{visreg} function performs the calculations
#' and, if \code{plot=TRUE} (the default), these calculations are passed to
#' \code{plot.visreg} for plotting.
#' 
#' See \code{\link{plot.visreg}} for plotting options, such as changing the
#' appearance of points, lines, confidence bands, etc.
#' 
#' @param fit The fitted model object you wish to visualize.  Any object with
#' 'predict' and 'model.frame' methods are supported, including lm, glm, gam,
#' rlm, coxph, and many more.
#' @param xvar Character string specifying the variable to be put on the x-axis
#' of your plot.  Both continuous variables and factors are supported.
#' @param by (Optional) A variable allowing you to divide your plot into
#' cross-sections based on levels of the \code{by} variable; particularly
#' useful for visualizing models with interactions.  Supplied as a character
#' string.  Uses the lattice package.  Both continuous variables and factors
#' are supported.
#' @param breaks If a continuous variable is used for the \code{by} option, the
#' \code{breaks} argument controls the values at which the cross-sections are
#' taken. By default, cross-sections are taken at the 10th, 50th, and 90th
#' quantiles.  If \code{breaks} is a single number, it specifies the number of
#' breaks.  If \code{breaks} is a vector of numbers, it specifies the values at
#' which the cross-sections are to be taken.  Each partial residuals appears
#' exactly once in the plot, in the panel it is closest to.
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
#' @param xtrans (Optional) A function specifying a transformation for the
#' horizontal axis.  Note that, for model terms such as \code{log(x)}, visreg
#' automatically plots on the original axis (see examples).
#' @param alpha Alpha level (1-coverage) for the confidence band displayed in
#' the plot (default: 0.05).
#' @param nn Controls the smoothness of the line and confidence band.
#' Increasing this number will add to the computational burden, but produce a
#' smoother plot (default: 101).
#' @param cond Named list specifying conditional values of other explanatory
#' variables. By default, conditional plots in visreg are constructed by
#' filling in other explanatory variables with the median (for numeric
#' variables) or most common category (for factors), but this can be overridden
#' by specifying their values using \code{cond} (see examples).
#' @param jitter Adds a small amount of noise to \code{xvar}.  Potentially
#' useful if many observations have exactly the same value.  Default is FALSE.
#' @param collapse If the \code{predict} method for \code{fit} returns a
#' matrix, should this be returns as multiple visreg objects bound together as
#' a list (\code{collapse=FALSE}) or collapsed down to a single \code{visreg}
#' object (\code{collapse=TRUE}).
#' @param plot Send the calculations to \code{\link{plot.visreg}}?  Default is
#' TRUE.
#' @param \dots Graphical parameters (e.g., \code{ylab}) can be passed to the
#' function to customize the plots.  If \code{by=TRUE}, lattice parameters can
#' be passed, such as layout (see examples below).
#' @return A \code{visreg} or \code{visregList} object (which is simply a list
#' of \code{visreg} objects).  A \code{visreg} object has three components:
#' \item{fit}{A data frame with \code{nn} rows containing the fit of the model
#' as \code{xvar} varies, along with lower and upper confidence bounds (named
#' \code{visregFit}, \code{visregLwr}, and \code{visregUpr}, respectively). The
#' fitted matrix of coefficients.} \item{res}{A data frame with \code{n} rows,
#' where \code{n} is the number of observations in the original data set used
#' to model.  This frame contains information about the residuals, named
#' \code{visregReg} and \code{visregPos}; the latter records whether the
#' residual was positive or negative.} \item{meta}{Contains meta-information
#' needed to construct plots, such as the name of the x and y variables,
#' whether there were any \code{by} variables, etc.}
#' @author Patrick Breheny and Woodrow Burchett
#' @seealso \url{https://pbreheny.github.io/visreg/} `[plot.visreg()]` `[visreg2d)]`
#' @references \itemize{
#'   \item \url{https://pbreheny.github.io/visreg/}
#'   \item Breheny, P. and Burchett, W. (2017), Visualizing regression models using visreg.
#'     \url{https://journal.r-project.org/archive/2017/RJ-2017-046/index.html}
#' }
#' @examples
#' 
#' # --- Linear models ----------------------------------------
#' 
#' ## Basic
#' fit <- lm(Ozone ~ Solar.R + Wind + Temp, data=airquality)
#' visreg(fit, "Wind", type="contrast")
#' visreg(fit, "Wind", type="conditional")
#' 
#' ## Factors
#' airquality$Heat <- cut(airquality$Temp, 3, labels=c("Cool","Mild","Hot"))
#' fit.heat <- lm(Ozone ~ Solar.R + Wind + Heat, data=airquality)
#' visreg(fit.heat, "Heat", type="contrast")
#' visreg(fit.heat, "Heat", type="conditional")
#' 
#' ## Transformations
#' fit1 <- lm(Ozone ~ Solar.R + Wind + Temp + I(Wind^2), data=airquality)
#' fit2 <- lm(log(Ozone) ~ Solar.R + Wind + Temp, data=airquality)
#' fit3 <- lm(log(Ozone) ~ Solar.R + Wind + Temp + I(Wind^2), data=airquality)
#' visreg(fit1, "Wind")
#' visreg(fit2, "Wind", trans=exp, ylab="Ozone")
#' visreg(fit3, "Wind", trans=exp, ylab="Ozone")
#' 
#' ## Conditioning
#' visreg(fit, "Wind", cond=list(Temp=50))
#' visreg(fit, "Wind", print.cond=TRUE)
#' visreg(fit, "Wind", cond=list(Temp=100))
#' 
#' ## Interactions
#' fit.in1 <- lm(Ozone~ Solar.R + Wind*Heat, data=airquality)
#' visreg(fit.in1, "Wind", by="Heat")
#' visreg(fit.in1, "Heat", by="Wind")
#' visreg(fit.in1, "Wind", by="Heat", type="contrast")
#' visreg(fit.in1, "Heat", by="Wind", breaks=6)
#' visreg(fit.in1, "Heat", by="Wind", breaks=c(0,10,20))
#' 
#' ## Overlay
#' visreg(fit.in1, "Wind", by="Heat", overlay=TRUE)
#' 
#' 
#' # --- Nonlinear models -------------------------------------
#' 
#' ## Logistic regression
#' data("birthwt", package="MASS")
#' birthwt$race <- factor(birthwt$race, labels=c("White","Black","Other"))
#' birthwt$smoke <- factor(birthwt$smoke, labels=c("Nonsmoker","Smoker"))
#' fit <- glm(low~age+race+smoke+lwt, data=birthwt, family="binomial")
#' visreg(fit, "lwt",
#'        xlab="Mother's Weight", ylab="Log odds (low birthweight)")
#' visreg(fit, "lwt", scale="response", partial=FALSE,
#'        xlab="Mother's Weight", ylab="P(low birthweight)")
#' visreg(fit, "lwt", scale="response", partial=FALSE,
#'        xlab="Mother's Weight", ylab="P(low birthweight)", rug=2)
#' 
#' ## Proportional hazards
#' require(survival)
#' data(ovarian)
#' ovarian$rx <- factor(ovarian$rx)
#' fit <- coxph(Surv(futime, fustat) ~ age + rx, data=ovarian)
#' visreg(fit, "age", ylab="log(Hazard ratio)")
#' 
#' ## Robust regression
#' require(MASS)
#' fit <- rlm(Ozone ~ Solar.R + Wind*Heat, data=airquality)
#' visreg(fit, "Wind", cond=list(Heat="Mild"))
#' 
#' ## And more...; anything with a 'predict' method should work
#' 
#' ## Return raw components of plot
#' v <- visreg(fit, "Wind", cond=list(Heat="Mild"))
#' 
#' @export visreg

visreg <- function(fit, xvar, by, breaks=3, type=c("conditional", "contrast"), data=NULL, trans=I,
                   scale=c("linear","response"), xtrans, alpha=.05, nn=101, cond=list(), jitter=FALSE, collapse=FALSE,
                   plot=TRUE, ...) {
  # Setup
  if (type[1]=="effect") {
    warning("Please note that type='effect' is deprecated and may not be supported in future versions of visreg.  Use type='contrast' instead.")
    type <- "contrast"
  }
  type <- match.arg(type)
  scale <- match.arg(scale)
  if (scale=="response") {
    if (inherits(fit, "lrm")) {
      trans <- binomial()$linkinv
    } else if (inherits(fit, "betareg")) {
      trans <- fit$link$mean$linkinv
    } else {
      trans <- family(fit)$linkinv
    }
  }
  if (!identical(trans, I) & type=="contrast") warning("You are attempting to transform a contrast.  The resulting plot is not guaranteed to be meaningful.", call.=FALSE)
  
  Data <- setupF(fit, xvar, parent.frame(), data)
  xvar <- attr(Data, "xvar")
  if (attr(Data, "needsUpdate")) {
    if (inherits(fit, 'coxph')) {
      fit <- update(fit, formula=formula(fit), data=Data, model=TRUE)
    } else {
      fit <- update(fit, formula=formula(fit), data=Data)
    }
  }
  cond <- setupCond(cond, Data, by, breaks)

  # Calculate v
  yName <- makeYName(fit, scale, trans, type)
  v <- setupV(fit, Data, xvar, nn, cond, type, trans, xtrans, alpha, jitter, by, yName, ...)
  if (collapse) v <- collapse.visregList(v)

  # Plot/return
  if (plot) {
    p <- plot(v, ...)
    if (!is.null(p) && inherits(p, 'gg')) return(p)
    if (!is.null(p) && inherits(p, 'list') && inherits(p[[1]], 'gg')) return(p)
  }
  invisible(v)
}
