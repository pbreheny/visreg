# Frequently asked questions

This page contains miscellaneous questions that I have been asked about
visreg, with answers posted here so that others can read them too.

## Q: In `visreg2d`, is it possible to superimpose data points onto the plot?

Yes; it’s probably easiest to do this using `ggplot2` to render the
surface:

``` r
fit <- lm(Ozone ~ Solar.R + Wind + Temp + I(Wind^2) + I(Temp^2) + I(Wind*Temp)+I(Wind*Temp^2) + 
          I(Temp*Wind^2) + I(Temp^2*Wind^2), data=airquality)
visreg2d(fit, x="Wind", y="Temp", plot.type="gg") +
  geom_point(aes(Wind, Temp), data=subset(airquality, !is.na(Ozone)), col='gray50')
# Warning: `aes_string()` was deprecated in ggplot2 3.0.0.
# ℹ Please use tidy evaluation idioms with `aes()`.
# ℹ See also `vignette("ggplot2-in-packages")` for more information.
# ℹ The deprecated feature was likely used in the visreg package.
#   Please report the issue at <https://github.com/pbreheny/visreg/issues>.
# This warning is displayed once every 8 hours.
# Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
# generated.
```

![](faq_files/figure-html/unnamed-chunk-2-1.png)

You can do it with base R graphics too, but it’s kind of complicated
because of how
[`filled.contour()`](https://rdrr.io/r/graphics/filled.contour.html)
works and the way in which it actually creates two plots, one main plot
and one plot for the legend:

``` r
p <- quote({
  axis(1, at = mx, labels = lx)
  axis(2, at = my, labels = ly)
  with(airquality, points(Wind, Temp, pch=19))
})
visreg2d(fit, x="Wind", y="Temp", plot.type="image", plot.axes=p)
```

![](faq_files/figure-html/unnamed-chunk-3-1.png)

Doing this through the axes is admittedly very weird – it’s not my idea;
see [`?filled.contour`](https://rdrr.io/r/graphics/filled.contour.html).

## Q: Does `visreg` work with `gamm` (mgcv) or `gamm4` objects?

Yes, although it takes a minor workaround since these objects have an
unusual structure. First, let’s simulate some data and fit a model (I’m
just using a `tibble` here for convenience, it isn’t relevant to the
question):

``` r
library(mgcv)
library(tibble)
n <- 20
Data <- tibble(
  ID = factor(rep(LETTERS[1:n], each=5)),
  x  = rnorm(n*5),
  lp = rnorm(n)[as.numeric(ID)] + x^2 - 1,
  p  = binomial()$linkinv(lp),
  y  = rbinom(n*5, 1, p)
)
fit <- gamm(y~s(x), data=Data, dist='binomial', random=list(ID=~1))
```

Now, `fit$gam` does not include the call (i.e., `fit$gam$call` is
`NULL`), which means `visreg` won’t be able to find the data:

``` r
visreg(fit$gam, 'x')
# Error in FUN(X[[i]], ...): object 'y' not found
```

So you have to include it manually:

``` r
fit$gam$data <- Data
visreg(fit$gam, 'x')
```

![](faq_files/figure-html/unnamed-chunk-6-1.png)

## Q: I’m getting the following error message: `Error in UseMethod("family") : no applicable method for 'family' applied to an object of class XXXX`.

When you specify `scale='response'`, you’re telling `visreg` to
automatically find the transformation mapping the linear predictor to
the scale of the response. It can only do that if the fitted model
object provides that transformation through the
[`family()`](https://rdrr.io/r/stats/family.html) method. Many R models
(e.g., `glm`) do this, but some (e.g., the `glmmADMB` package, `gamm`
from the `mgcv` package) don’t. If you’re working with one of these
models, you’ll have to supply that transformation yourself. For example,
with the model from the previous question, this code fails:

``` r
visreg(fit$gam, 'x', scale='response')
# Error in UseMethod("family"): no applicable method for 'family' applied to an object of class "gam"
```

Since this is a binomial model with a logistic link,
`binomial()$linkinv` provides the inverse transformation:

``` r
visreg(fit$gam, 'x', trans=binomial()$linkinv, ylab="Probability")
```

![](faq_files/figure-html/unnamed-chunk-8-1.png)

## Q: Can I have the response variable on the horizontal axis instead of the vertical axis?

You can achieve this via
[`ggplot2::coord_flip`](https://ggplot2.tidyverse.org/reference/coord_flip.html):

``` r
fit <- lm(Ozone ~ Solar.R + Wind + Temp, data=airquality)
visreg(fit, "Wind", gg=TRUE) + coord_flip()
# Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
# ℹ Please use `linewidth` instead.
# ℹ The deprecated feature was likely used in the ggplot2 package.
#   Please report the issue at <https://github.com/tidyverse/ggplot2/issues>.
# This warning is displayed once every 8 hours.
# Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
# generated.
```

![](faq_files/figure-html/unnamed-chunk-9-1.png)
