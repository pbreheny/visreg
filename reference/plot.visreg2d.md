# Visualization of regression functions for two variables

Plot method for visualizing how two variables interact to affect the
response in regression models, as a `ggplot2` raster/contour plot.

## Usage

``` r
# S3 method for class 'visreg2d'
plot(
  x,
  xlab = NULL,
  ylab = NULL,
  zlab = NULL,
  color,
  print_cond = FALSE,
  whitespace = 0.2,
  plot.type = NULL,
  ...
)
```

## Arguments

- x:

  A
  [`visreg2d()`](https://pbreheny.github.io/visreg/reference/visreg2d.md)
  object.

- xlab:

  Axis label for x variable

- ylab:

  Axis label for y variable

- zlab:

  Label for the color legend

- color:

  A vector of colors used to establish the color palette for the
  fill/legend.

- print_cond:

  If `print_cond==TRUE`, the explanatory variable values conditioned on
  in a conditional plot are printed to the console (default: `FALSE`).
  If `print_cond==TRUE` and `type=="contrast"`, the conditions will
  still be printed, but they have no bearing on the plot unless
  interactions are present.

- whitespace:

  When `xvar` or `yvar` is a factor, `whitespace` determines the amount
  of space in between the factors. Default is 0.2, meaning that 20
  percent of the axis is whitespace.

- plot.type:

  Deprecated, no longer has any effect. See
  [vignette](https://pbreheny.github.io/visreg/articles/surface.html).

- ...:

  Not used.

## Value

A `ggplot2` object.

## Details

For 3-dimensional surface plots, see
[`persp.visreg2d()`](https://pbreheny.github.io/visreg/reference/persp.visreg2d.md)
(static) or
[`rgl::persp3d()`](https://dmurdoch.github.io/rgl/dev/reference/persp3d.html)
(interactive, requires the `rgl` package).

## References

Breheny P and Burchett W. (2017) Visualization of regression models
using visreg. *R Journal*, **9**: 56-71.
[doi:10.32614/RJ-2017-046](https://doi.org/10.32614/RJ-2017-046)

## See also

[`visreg2d()`](https://pbreheny.github.io/visreg/reference/visreg2d.md)
for creating two-dimensional `visreg` objects,
[`persp.visreg2d()`](https://pbreheny.github.io/visreg/reference/persp.visreg2d.md)
for static 3D surface plots, and the [surface plots
vignette](https://pbreheny.github.io/visreg/articles/surface.html) for
examples and details.

## Examples

``` r
fit <- lm(
  Ozone ~ Solar.R + Wind + Temp + I(Wind^2) + I(Temp^2) +
    I(Wind * Temp) + I(Wind * Temp^2) + I(Temp * Wind^2) + I(Temp^2 * Wind^2),
  data = airquality
)

visreg2d(fit, x = "Wind", y = "Temp")

visreg2d(fit, x = "Wind", y = "Temp", color = c("purple", "green", "red"))

```
