# Static 3D perspective plot for a visreg2d object

[`persp()`](https://rdrr.io/r/graphics/persp.html) method for
[`visreg2d()`](https://pbreheny.github.io/visreg/reference/visreg2d.md)
objects: draws a static 3-dimensional perspective plot of the fitted
surface, using base R's
[`graphics::persp()`](https://rdrr.io/r/graphics/persp.html).

## Usage

``` r
# S3 method for class 'visreg2d'
persp(
  x,
  xlab = NULL,
  ylab = NULL,
  zlab = NULL,
  color = "#2fa4e7",
  whitespace = 0.2,
  ...
)

# S3 method for class 'visreg_list'
persp(x, ...)

persp3d.visreg_list(x, ...)
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

  Axis label for outcome

- color:

  The color of the surface.

- whitespace:

  When `xvar` or `yvar` is a factor, `whitespace` determines the amount
  of space in between the factors. Default is 0.2, meaning that 20
  percent of the axis is whitespace.

- ...:

  Additional graphical parameters, passed to
  [`graphics::persp()`](https://rdrr.io/r/graphics/persp.html).

## Value

The viewing transformation matrix, invisibly; see
[`graphics::persp()`](https://rdrr.io/r/graphics/persp.html).

## Details

For an interactive version that can be rotated with the mouse, see
[`rgl::persp3d()`](https://dmurdoch.github.io/rgl/dev/reference/persp3d.html)
(requires the `rgl` package). For a 2-dimensional raster/contour plot,
see
[`plot.visreg2d()`](https://pbreheny.github.io/visreg/reference/plot.visreg2d.md).

## References

Breheny P and Burchett W. (2017) Visualization of regression models
using visreg. *R Journal*, **9**: 56-71.
[doi:10.32614/RJ-2017-046](https://doi.org/10.32614/RJ-2017-046)

## See also

[`visreg2d()`](https://pbreheny.github.io/visreg/reference/visreg2d.md)
for creating two-dimensional `visreg` objects,
[`plot.visreg2d()`](https://pbreheny.github.io/visreg/reference/plot.visreg2d.md)
for the 2-dimensional raster/contour plot, and the [surface plots
vignette](https://pbreheny.github.io/visreg/articles/surface.html) for
examples and details.

## Examples

``` r
fit <- lm(
  Ozone ~ Solar.R + Wind + Temp + I(Wind^2) + I(Temp^2) +
    I(Wind * Temp) + I(Wind * Temp^2) + I(Temp * Wind^2) + I(Temp^2 * Wind^2),
  data = airquality
)

v <- visreg2d(fit, x = "Wind", y = "Temp", plot = FALSE)
persp(v)

```
