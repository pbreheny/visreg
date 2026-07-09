#' Visualization of regression functions
#'
#' A function for visualizing regression models quickly and easily. Default plots contain a
#' confidence band, prediction line, and partial residuals. Factors, transformations, conditioning,
#' interactions, and a variety of other options are supported. The `visreg` function performs the
#' calculations and, if `plot=TRUE` (the default), these calculations are passed to `plot.visreg`
#' for plotting.
#'
#' See [plot.visreg()] for plotting options, such as changing the appearance of points, lines,
#' confidence bands, etc.
#'
#' @param fit The fitted model object you wish to visualize. Any object with 'predict' and
#'   'model.frame' methods are supported, including lm, glm, gam, rlm, coxph, and many more.
#' @param xvar Character string specifying the variable to be put on the x-axis of your plot. Both
#'   continuous variables and factors are supported.
#' @param by (Optional) A variable allowing you to divide your plot into cross-sections based on
#'   levels of the `by` variable; particularly useful for visualizing models with interactions.
#'   Supplied as a character string. Cross-sections are displayed as facets of a ggplot2 plot. Both
#'   continuous variables and factors are supported.
#' @param breaks If a continuous variable is used for the `by` option, the `breaks` argument
#'   controls the values at which the cross-sections are taken. By default, cross-sections are taken
#'   at the 10th, 50th, and 90th quantiles. If `breaks` is a single number, it specifies the number
#'   of breaks. If `breaks` is a vector of numbers, it specifies the values at which the
#'   cross-sections are to be taken. Each partial residuals appears exactly once in the plot, in the
#'   panel it is closest to.
#' @param type The type of plot to be produced.  The following options are supported:
#'   - If `"conditional"` is selected, the plot returned shows the value of the variable on the
#'     x-axis and the change in response on the y-axis, holding all other variables constant (by
#'     default, median for numeric variables and most common category for factors).
#'   - If `"contrast"` is selected, the plot returned shows the effect on the expected value of the
#'     response by moving the x variable away from a reference point on the x-axis (for numeric
#'     variables, this is taken to be the mean).
#'   For more details, see references.
#' @param data The data frame used to fit the model. Typically, visreg() can figure out where the
#'   data is, so it is not necessary to provide this. In some cases, however, the data set cannot be
#'   located and must be supplied explicitly.
#' @param trans (Optional) A function specifying a transformation for the vertical axis.
#' @param scale By default, the model is plotted on the scale of the linear predictor. If
#'   `scale='response'` for a glm, the inverse link function will be applied so that the model is
#'   plotted on the scale of the original response.
#' @param xtrans (Optional) A function specifying a transformation for the horizontal axis. Note
#'   that, for model terms such as `log(x)`, visreg automatically plots on the original axis (see
#'   examples).
#' @param alpha Alpha level (1-coverage) for the confidence band displayed in the plot (default:
#'   0.05).
#' @param nn Controls the smoothness of the line and confidence band. Increasing this number will
#'   add to the computational burden, but produce a smoother plot (default: 101).
#' @param cond Named list specifying conditional values of other explanatory variables. By default,
#'   conditional plots in visreg are constructed by filling in other explanatory variables with the
#'   median (for numeric variables) or most common category (for factors), but this can be
#'   overridden by specifying their values using `cond` (see examples).
#' @param jitter Adds a small amount of noise to `xvar`. Potentially useful if many observations
#'   have exactly the same value. Default is FALSE.
#' @param collapse If the `predict` method for `fit` returns a matrix, should this be returns as
#'   multiple visreg objects bound together as a list (`collapse=FALSE`) or collapsed down to a
#'   single `visreg` object (`collapse=TRUE`).
#' @param plot Send the calculations to [plot.visreg()]? Default is TRUE.
#' @param \dots Graphical parameters (e.g., `ylab`) can be passed to the function to customize the
#'   plots. If `by=TRUE`, ggplot2 faceting parameters can be passed, such as `strip_names` (see
#'   examples below).
#'
#' @returns
#' A `visreg` or `visreg_list` object (which is simply a list of `visreg` objects). A visreg` object
#' has three components:
#'
#' \describe{
#'   \item{fit}{
#'     A data frame with `nn` rows containing the fit of the model as `xvar` varies, along with
#'     lower and upper confidence bounds (named `visreg_fit`, `visreg_lwr`, and `visreg_upr`,
#'     respectively). The fitted matrix of coefficients.
#'   }
#'   \item{res}{
#'     A data frame with `n` rows, where `n` is the number of observations in the original data set
#'     used to model. This frame contains information about the residuals, named `visregReg` and
#'     `visreg_pos`; the latter records whether the residual was positive or negative.
#'   }
#'   \item{meta}{
#'     Contains meta-information needed to construct plots, such as the name of the x and y
#'     variables, whether there were any `by` variables, etc.
#'   }
#' }
#'
#' Note that if `plot = TRUE` (the default), then the `visreg` object is passed to [plot.visreg()]
#' and a `ggplot` object is returned instead.
#'
#' @seealso
#' [plot.visreg()] for plotting options, [visreg2d()] for creating two-dimensional visreg objects,
#' and the [package website](https://pbreheny.github.io/visreg/) for detailed explanations and
#' examples.
#' @references
#' - <https://pbreheny.github.io/visreg/>
#' - Breheny P and Burchett W. (2017) Visualization of regression models using
#'   visreg. *R Journal*, **9**: 56-71.
#'   \doi{10.32614/RJ-2017-046}
#'
#' @examples
#'
#' # --- Linear models ----------------------------------------
#'
#' ## Basic
#' fit <- lm(Ozone ~ Solar.R + Wind + Temp, data = airquality)
#' visreg(fit, "Wind", type = "contrast")
#' visreg(fit, "Wind", type = "conditional")
#'
#' ## Factors
#' airquality$Heat <- cut(airquality$Temp, 3, labels = c("Cool", "Mild", "Hot"))
#' fit.heat <- lm(Ozone ~ Solar.R + Wind + Heat, data = airquality)
#' visreg(fit.heat, "Heat", type = "contrast")
#' visreg(fit.heat, "Heat", type = "conditional")
#'
#' ## Transformations
#' fit1 <- lm(Ozone ~ Solar.R + Wind + Temp + I(Wind^2), data = airquality)
#' fit2 <- lm(log(Ozone) ~ Solar.R + Wind + Temp, data = airquality)
#' fit3 <- lm(log(Ozone) ~ Solar.R + Wind + Temp + I(Wind^2), data = airquality)
#' visreg(fit1, "Wind")
#' visreg(fit2, "Wind", trans = exp, ylab = "Ozone")
#' visreg(fit3, "Wind", trans = exp, ylab = "Ozone")
#'
#' ## Conditioning
#' visreg(fit, "Wind", cond = list(Temp = 50))
#' visreg(fit, "Wind", print_cond = TRUE)
#' visreg(fit, "Wind", cond = list(Temp = 100))
#'
#' ## Interactions
#' fit.in1 <- lm(Ozone ~ Solar.R + Wind * Heat, data = airquality)
#' visreg(fit.in1, "Wind", by = "Heat")
#' visreg(fit.in1, "Heat", by = "Wind")
#' visreg(fit.in1, "Wind", by = "Heat", type = "contrast")
#' visreg(fit.in1, "Heat", by = "Wind", breaks = 6)
#' visreg(fit.in1, "Heat", by = "Wind", breaks = c(0, 10, 20))
#'
#' ## Overlay
#' visreg(fit.in1, "Wind", by = "Heat", overlay = TRUE)
#'
#'
#' # --- Nonlinear models -------------------------------------
#'
#' ## Logistic regression
#' data("birthwt", package = "MASS")
#' birthwt$race <- factor(birthwt$race, labels = c("White", "Black", "Other"))
#' birthwt$smoke <- factor(birthwt$smoke, labels = c("Nonsmoker", "Smoker"))
#' fit <- glm(low ~ age + race + smoke + lwt, data = birthwt, family = "binomial")
#' visreg(fit, "lwt",
#'   xlab = "Mother's Weight", ylab = "Log odds (low birthweight)"
#' )
#' visreg(fit, "lwt",
#'   scale = "response", partial = FALSE,
#'   xlab = "Mother's Weight", ylab = "P(low birthweight)"
#' )
#' visreg(fit, "lwt",
#'   scale = "response", partial = FALSE,
#'   xlab = "Mother's Weight", ylab = "P(low birthweight)", rug = 2
#' )
#'
#' ## Proportional hazards
#' require(survival)
#' data(ovarian)
#' ovarian$rx <- factor(ovarian$rx)
#' fit <- coxph(Surv(futime, fustat) ~ age + rx, data = ovarian)
#' visreg(fit, "age", ylab = "log(Hazard ratio)")
#'
#' ## Robust regression
#' require(MASS)
#' fit <- rlm(Ozone ~ Solar.R + Wind * Heat, data = airquality)
#' visreg(fit, "Wind", cond = list(Heat = "Mild"))
#'
#' ## And more...; anything with a 'predict' method should work
#'
#' ## Return raw components of plot
#' v <- visreg(fit, "Wind", cond = list(Heat = "Mild"))
#'
#' @export
visreg <- function(
  fit,
  xvar,
  by,
  breaks = 3,
  type = c("conditional", "contrast"),
  data = NULL,
  trans = I,
  scale = c("linear", "response"),
  xtrans,
  alpha = .05,
  nn = 101,
  cond = list(),
  jitter = FALSE,
  collapse = FALSE,
  plot = TRUE,
  ...
) {
  # Setup
  if (type[1] == "effect") {
    warning(
      "Please note that type='effect' is deprecated and may not be supported in future versions of visreg.  Use type='contrast' instead."
    )
    type <- "contrast"
  }
  type <- match.arg(type)
  scale <- match.arg(scale)
  if (scale == "response") {
    if (inherits(fit, "lrm")) {
      trans <- binomial()$linkinv
    } else if (inherits(fit, "betareg")) {
      trans <- fit$link$mean$linkinv
    } else {
      trans <- family(fit)$linkinv
    }
  }
  if (!identical(trans, I) & type == "contrast") {
    warning(
      "You are attempting to transform a contrast. The resulting plot is not guaranteed to be meaningful.",
      call. = FALSE
    )
  }

  Data <- setup_frame(fit, xvar, parent.frame(), data)
  xvar <- attr(Data, "xvar")
  if (attr(Data, "needsUpdate")) {
    if (inherits(fit, "coxph")) {
      fit <- update(fit, formula = formula(fit), data = Data, model = TRUE)
    } else {
      fit <- update(fit, formula = formula(fit), data = Data)
    }
  }
  cond <- setup_cond(cond, Data, by, breaks)

  # Calculate v
  yName <- make_y_name(fit, scale, trans, type)
  v <- build_visreg(fit, Data, xvar, nn, cond, type, trans, xtrans, alpha, jitter, by, yName, ...)
  if (collapse) {
    v <- collapse.visreg_list(v)
  }

  # Plot/return
  if (plot) {
    p <- plot(v, ...)
    if (!is.null(p) && inherits(p, "gg")) {
      return(p)
    }
    if (!is.null(p) && inherits(p, "list") && inherits(p[[1]], "gg")) {
      return(p)
    }
  }
  invisible(v)
}
