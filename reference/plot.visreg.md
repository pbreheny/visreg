# Visualization of regression functions

Default plots contain a confidence band, prediction line, and partial
residuals. Factors, transformations, conditioning, interactions, and a
variety of other options are supported. The `plot.visreg()` function
accepts a `visreg` object as calculated by
[`visreg()`](https://pbreheny.github.io/visreg/reference/visreg.md) and
creates the plot.

## Usage

``` r
# S3 method for class 'visreg'
plot(
  x,
  overlay = FALSE,
  print_cond = FALSE,
  partial = identical(x$meta$trans, I),
  band = TRUE,
  rug = 0,
  jitter = FALSE,
  strip_names = is.numeric(x$fit[, x$meta$by]),
  top = c("line", "points"),
  line = NULL,
  fill = NULL,
  points = NULL,
  gg = TRUE,
  ...
)

# S3 method for class 'visreg_list'
plot(x, ...)

# S3 method for class 'visreg_plot_list'
print(x, ...)
```

## Arguments

- x:

  A `visreg` or `visreg_list` object; see
  [`visreg()`](https://pbreheny.github.io/visreg/reference/visreg.md).

- overlay:

  By default, when `by` is specified, separate panels are used to
  display each cross-section. If `overlay=TRUE`, these cross-sections
  are overlaid on top of each other in a single plot.

- print_cond:

  If `print_cond=TRUE`, the explanatory variable values conditioned on
  in a conditional plot are printed to the console (default: `FALSE`).
  If `print_cond=TRUE` and `type="contrast"`, the conditions will still
  be printed, but they have no bearing on the plot unless interactions
  are present.

- partial:

  If `partial=TRUE` (the default), partial residuals are shown on the
  plot.

- band:

  If `band=TRUE` (the default), confidence bands are shown on the plot.

- rug:

  Controls the appearance of a rug along the horizontal axis:

  - `rug=0`/`FALSE`: No rug (default)

  - `rug=1`/`TRUE`: Basic rug is drawn along the bottom axis

  - `rug=2`: Separate rugs drawn on the top for observations with
    positive residuals and on the bottom for observations with negative
    residuals.

- jitter:

  If `jitter=TRUE`, a small amount of random noise is added to the
  horizontal position of partial residuals (and, if drawn, the rug).
  Potentially useful if many observations have exactly the same value.
  Only applies when `xvar` is continuous; for factor `xvar`, the
  horizontal spacing of points is already controlled by `visreg` and
  this argument has no effect. Default is `FALSE`.

- strip_names:

  When `by=TRUE`, `strip_names=TRUE` adds the name of the `by` variable
  to the strip at the top of each panel. Default is `FALSE` for factors
  and `TRUE` for numeric `by` variables. `strip_names` can also be a
  character vector, in which case it replaces the strip names altogether
  with values chosen by the user.

- top:

  By default, the fitted line is plotted on top of the partial
  residuals; usually this is preferable, but it does run the risk of
  obscuring certain residuals. To change this behavior and plot the
  partial residuals on top, specify `top='points'`.

- line:

  List of parameters to pass to
  [`ggplot2::geom_line()`](https://ggplot2.tidyverse.org/reference/geom_path.html)
  (continuous `xvar`) or
  [`ggplot2::geom_crossbar()`](https://ggplot2.tidyverse.org/reference/geom_linerange.html)
  (factor `xvar`) when the fitted line is plotted.

- fill:

  List of parameters to pass to
  [`ggplot2::geom_ribbon()`](https://ggplot2.tidyverse.org/reference/geom_ribbon.html)
  (continuous `xvar`) or
  [`ggplot2::geom_crossbar()`](https://ggplot2.tidyverse.org/reference/geom_linerange.html)
  (factor `xvar`) when shaded confidence regions are plotted.

- points:

  List of parameters to pass to
  [`ggplot2::geom_point()`](https://ggplot2.tidyverse.org/reference/geom_point.html)
  (continuous `xvar`) or
  [`ggplot2::geom_jitter()`](https://ggplot2.tidyverse.org/reference/geom_jitter.html)
  (factor `xvar`) when partial residuals are plotted.

- gg:

  Deprecated, no longer has any effect.

- ...:

  Not used; present only because
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) is a generic
  function. An error is raised if any arguments are passed here.

## Value

A ggplot object, or if `x` is a
[`visreg_list()`](https://pbreheny.github.io/visreg/reference/visreg_list.md),
a list of ggplot objects.

## Details

If `x` is a
[`visreg_list()`](https://pbreheny.github.io/visreg/reference/visreg_list.md),
`plot.visreg_list()` is dispatched instead; it simply calls
`plot.visreg()` on each element in turn, passing along the same
arguments to each element.

## References

Breheny P and Burchett W. (2017) Visualization of regression models
using visreg. *R Journal*, **9**: 56-71.
[doi:10.32614/RJ-2017-046](https://doi.org/10.32614/RJ-2017-046)

## See also

[visreg2d](https://pbreheny.github.io/visreg/reference/visreg2d.md) for
creating `visreg` objects, and the [options
vignette](https://pbreheny.github.io/visreg/surface.html) for examples.

## Examples

``` r
fit <- lm(Ozone ~ Solar.R + Wind + Temp, data = airquality)
visreg(fit, "Wind", line = list(color = "red"), points = list(size = 1, shape = 1))


# Changing appearance
visreg(fit, "Wind", line = list(color = "red"), points = list(size = 1, shape = 1))

```
