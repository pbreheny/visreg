# Cross-sectional plots

As was the case with [nonlinear
terms](https://pbreheny.github.io/visreg/articles/trans.md), the
relationship between `x` and `y` in a model with interactions also
(typically) depends on multiple coefficients and thus, a visual summary
tends to be much more readily understood than a numeric one.

For models with interactions, we must simultaneously visualize the
effect of two explanatory variables. The `visreg` package offers two
methods for doing so; this page describes what we call *cross-sectional
plots*, which plot one-dimensional relationships between the response
and one predictor for several values of another predictor, either in
separate panels or [overlaid on top of one
another](https://pbreheny.github.io/visreg/articles/overlay.md). The
package also provides methods for constructing [*surface
plots*](https://pbreheny.github.io/visreg/articles/surface.md), which
attempt to provide a picture of the regression surface over both
dimensions simultaneously.

Let’s fit a model that involves an interaction between a continuous term
and a categorical term:

``` r

airquality$Heat <- cut(airquality$Temp, 3, labels = c("Cool", "Mild", "Hot"))
fit <- lm(Ozone ~ Solar.R + Wind * Heat, data = airquality)
```

We can then use `visreg` to see how the effect of wind on ozone differs
depending on the temperature:

``` r

visreg(fit, "Wind", by = "Heat")
```

![](cross_files/figure-html/unnamed-chunk-3-1.png)

Or alternatively, see how the effect of temperature depends on the wind
level:

``` r

visreg(fit, "Heat", by = "Wind")
```

![](cross_files/figure-html/unnamed-chunk-4-1.png)

Note that, since `Wind` is a continuous variable, the panels above are
somewhat arbitrary. By default, `visreg` sets up three panels using the
10th, 50th, and 90th percentiles, but [the user can change both the
number and the location of these break points](#opt).

In all of these plots, note that each partial residuals appears exactly
once in the plot, in the panel it is closest to.

## Options

For a numeric `by` variable, the `breaks` argument controls the values
at which the cross-sections are taken. By default, cross-sections are
taken at three quantiles (10th, 50th, and 90th), but a larger number can
be specified:

``` r

visreg(fit, "Heat", by = "Wind", breaks = 4)
```

![](cross_files/figure-html/unnamed-chunk-5-1.png)

If `breaks` is a vector of numbers, it specifies the values at which the
cross-sections are to be taken:

``` r

visreg(fit, "Heat", by = "Wind", breaks = c(seq(5, 15, 5)))
```

![](cross_files/figure-html/unnamed-chunk-6-1.png)

## Graphical options

Plots are built with `ggplot2`, and the returned object has class `gg`,
so its appearance can be changed via the usual `ggplot2` components,
such as themes:

``` r

visreg(fit, "Wind", by = "Heat")
```

![](cross_files/figure-html/unnamed-chunk-7-1.png)

The appearance of points, lines, and bands is set with the `line`,
`fill`, and `points` arguments, [as described
here](https://pbreheny.github.io/visreg/articles/options.md):

``` r

visreg(fit, "Wind", by = "Heat", fill = list(fill = "#008DFF33"))
```

![](cross_files/figure-html/unnamed-chunk-8-1.png)

`visreg` sets up the facet strips internally via the `strip_names`
option:

``` r

visreg(fit, "Wind", by = "Heat", strip_names = TRUE)
```

![](cross_files/figure-html/unnamed-chunk-9-1.png)

You can also explicitly specify the labels for each strip:

``` r

visreg(fit, "Wind", by = "Heat", strip_names = c("Cold days", "Mild days", "Hot days"))
```

![](cross_files/figure-html/unnamed-chunk-10-1.png)

Other aspects of faceting, such as the number of rows/columns, can be
changed by adding a new
[`facet_wrap()`](https://ggplot2.tidyverse.org/reference/facet_wrap.html)
(or
[`facet_grid()`](https://ggplot2.tidyverse.org/reference/facet_grid.html))
to the returned plot:

``` r

visreg(fit, "Heat", by = "Wind", breaks = 4) + facet_wrap(~Wind, nrow = 1)
```

![](cross_files/figure-html/unnamed-chunk-11-1.png)
