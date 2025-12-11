# Visualization of regression functions for two variables

Plot method for visualizing how two variables interact to affect the
response in regression models.

## Usage

``` r
# S3 method for class 'visreg2d'
plot(
  x,
  plot.type = c("image", "persp", "rgl", "gg"),
  xlab,
  ylab,
  zlab,
  color,
  print.cond = FALSE,
  whitespace = 0.2,
  ...
)
```

## Arguments

- x:

  A
  [`visreg2d()`](https://pbreheny.github.io/visreg/reference/visreg2d.md)
  object.

- plot.type:

  The style of plot to be produced. The following options are supported:

  - `image`: a filled contour

  - `gg`: a filled contour plot using ggplot2

  - `persp`: a 3 dimensional perspective plot

  - `rgl`: a version of the perspective plot that can be rotated
    (requires the rgl package to be installed)

- xlab:

  Axis label for x variable

- ylab:

  Axis label for y variable

- zlab:

  Axis label for outcome

- color:

  For `plot.type='persp'` or `plot.type='rgl'`, the color of the
  surface. For `plot.type='image'` or `plot.type='gg'`, a vector of
  colors used to establish a color palette.

- print.cond:

  If `print.cond==TRUE`, the explanatory variable values conditioned on
  in a conditional plot are printed to the console (default: `FALSE`).
  If `print.cond==TRUE` and `type=="contrast"`, the conditions will
  still be printed, but they have no bearing on the plot unless
  interactions are present.

- whitespace:

  When `xvar` or `yvar` is a factor, `whitespace` determines the amount
  of space in between the factors. Default is 0.2, meaning that 20
  percent of the axis is whitespace.

- ...:

  Graphical parameters can be passed to the function to customize the
  plots.

## References

Breheny P and Burchett W. (2017) Visualization of regression models
using visreg. *R Journal*, **9**: 56-71.
[doi:10.32614/RJ-2017-046](https://doi.org/10.32614/RJ-2017-046)

## See also

https://pbreheny.github.io/visreg/surface.html,
[`visreg()`](https://pbreheny.github.io/visreg/reference/visreg.md)

## Author

Patrick Breheny and Woodrow Burchett

## Examples

``` r
fit <- lm(Ozone ~ Solar.R + Wind + Temp + I(Wind^2) + I(Temp^2) +
I(Wind*Temp)+I(Wind*Temp^2) + I(Temp*Wind^2) + I(Temp^2*Wind^2),
data=airquality)

visreg2d(fit, x="Wind", y="Temp", plot.type="image")

visreg2d(fit, x="Wind", y="Temp", plot.type="image",
         color=c("purple", "green", "red"))

visreg2d(fit, x="Wind", y="Temp", plot.type="persp")


## Requires the rgl package
# \donttest{
visreg2d(fit,x="Wind",y="Temp",plot.type="rgl")
#> Loading required namespace: rgl
#> Warning: RGL: unable to open X11 display
#> Warning: 'rgl.init' failed, will use the null device.
#> See '?rgl.useNULL' for ways to avoid this warning.
# }

## Requires the ggplot2 package
# \donttest{
visreg2d(fit, x="Wind", y="Temp", plot.type="gg")
#> Warning: `aes_string()` was deprecated in ggplot2 3.0.0.
#> ℹ Please use tidy evaluation idioms with `aes()`.
#> ℹ See also `vignette("ggplot2-in-packages")` for more information.
#> ℹ The deprecated feature was likely used in the visreg package.
#>   Please report the issue at <https://github.com/pbreheny/visreg/issues>.

# }
```
