# Visualization of regression functions

A function for visualizing regression models quickly and easily. Default
plots contain a confidence band, prediction line, and partial residuals.
Factors, transformations, conditioning, interactions, and a variety of
other options are supported. The `plot.visreg()` function accepts a
`visreg` or `visregList` object as calculated by
[`visreg()`](https://pbreheny.github.io/visreg/reference/visreg.md) and
creates the plot.

## Usage

``` r
# S3 method for class 'visreg'
plot(
  x,
  overlay = FALSE,
  print.cond = FALSE,
  whitespace = 0.2,
  partial = identical(x$meta$trans, I),
  band = TRUE,
  rug = ifelse(partial, 0, 2),
  strip.names = is.numeric(x$fit[, x$meta$by]),
  legend = TRUE,
  top = c("line", "points"),
  gg = FALSE,
  line.par = NULL,
  fill.par = NULL,
  points.par = NULL,
  ...
)
```

## Arguments

- x:

  A `visreg` or `visregList` object; see
  [`visreg()`](https://pbreheny.github.io/visreg/reference/visreg.md).

- overlay:

  By default, when `by` is specified, separate panels are used to
  display each cross-section. If `overlay=TRUE`, these cross-sections
  are overlaid on top of each other in a single plot.

- print.cond:

  If `print.cond=TRUE`, the explanatory variable values conditioned on
  in a conditional plot are printed to the console (default: `FALSE`).
  If `print.cond=TRUE` and `type="contrast"`, the conditions will still
  be printed, but they have no bearing on the plot unless interactions
  are present.

- whitespace:

  When `xvar` is a factor, `whitespace` determines the amount of space
  in between factors on the x-axis. Default is 0.2, meaning that 20
  percent of the horizontal axis is whitespace.

- partial:

  If `partial=TRUE` (the default), partial residuals are shown on the
  plot.

- band:

  If `band=TRUE` (the default), confidence bands are shown on the plot.

- rug:

  By default, partial residuals are plotted. Alternatively, a
  [`rug()`](https://rdrr.io/r/graphics/rug.html) may be plotted along
  the horizontal axis instead. Setting `rug=TRUE` turns off partial
  residuals by default; if one wants both to be plotted, both `rug=TRUE`
  and `partial=TRUE` need to be specified. Two types of rug plots are
  available. If `rug=1` or `rug=TRUE`, then a basic rug is drawn on the
  bottom. If `rug=2`, then separate rugs are drawn on the top for
  observations with positive residuals and on the bottom for
  observations with negative residuals. Such plots are particularly
  useful in logistic regression (see examples).

- strip.names:

  When `by=TRUE`, `strip.names=TRUE` adds the name of the `by` variable
  to the strip at the top of each panel. Default is `FALSE` for factors
  and `TRUE` for numeric `by` variables. `strip.names` can also be a
  character vector, in which case it replaces the strip names altogether
  with values chosen by the user.

- legend:

  For overlay plots, (`overlay=TRUE`), should visreg create a legend? If
  `legend=TRUE` (the default), a legend is placed in the top margin.

- top:

  By default, the fitted line is plotted on top of the partial
  residuals; usually this is preferable, but it does run the risk of
  obscuring certain residuals. To change this behavior and plot the
  partial residuals on top, specify `top='points'`.

- gg:

  By default (`gg=FALSE`), `visreg` will use the **lattice** package to
  render the plot if multiple panels are required. If `gg=TRUE`, it will
  use the **ggplot2** package instead, provided that it is installed.

- line.par:

  List of parameters (see
  [`par()`](https://rdrr.io/r/graphics/par.html)) to pass to
  `lines(...)` or
  [`ggplot2::geom_line()`](https://ggplot2.tidyverse.org/reference/geom_path.html)
  when lines are plotted.

- fill.par:

  List of parameters (see
  [`par()`](https://rdrr.io/r/graphics/par.html)) to pass to
  `polygon(...)` or
  [`ggplot2::geom_polygon()`](https://ggplot2.tidyverse.org/reference/geom_polygon.html)
  when shaded confidence regions are plotted.

- points.par:

  List of parameters ([`par()`](https://rdrr.io/r/graphics/par.html)) to
  pass to `points(...)` or
  [`ggplot2::geom_point()`](https://ggplot2.tidyverse.org/reference/geom_point.html)
  when partial residuals are plotted.

- ...:

  Graphical parameters can be passed to the function to customize the
  plots. If `by=TRUE`, lattice parameters can be passed, such as
  `layout` (see examples below).

## References

Breheny P and Burchett W. (2017) Visualization of regression models
using visreg. *R Journal*, **9**: 56-71.
[doi:10.32614/RJ-2017-046](https://doi.org/10.32614/RJ-2017-046)

## See also

<https://pbreheny.github.io/visreg/articles/options.html>,
[`visreg()`](https://pbreheny.github.io/visreg/reference/visreg.md),
[`visreg2d()`](https://pbreheny.github.io/visreg/reference/visreg2d.md)

## Author

Patrick Breheny and Woodrow Burchett

## Examples

``` r
fit <- lm(Ozone ~ Solar.R + Wind + Temp,data=airquality)
visreg(fit, "Wind", line=list(col="red"), points=list(cex=1, pch=1))


## Changing appearance
visreg(fit, "Wind", line=list(col="red"), points=list(cex=1, pch=1))

## See ?visreg and https://pbreheny.github.io/visreg for more examples
```
