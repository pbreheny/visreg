# Join multiple visreg objects together in a list

This function takes multiple visreg objects, from separate calls to
[`visreg()`](https://pbreheny.github.io/visreg/reference/visreg.md), and
joins them together in a single object. The single object will be of
type `visreg_list` unless `collapse=TRUE` is specified, in which case
the list will be collapsed back down into a single `visreg` object.

## Usage

``` r
visreg_list(..., labels, collapse = FALSE)
```

## Arguments

- ...:

  `visreg` objects, as produced by calls to
  [`visreg()`](https://pbreheny.github.io/visreg/reference/visreg.md).

- labels:

  A character vector with length corresponding to the number of `visreg`
  objects passed to the function that provides labels for the different
  objects in subsequent plots. Only has an effect if `collapse=TRUE`.

- collapse:

  If `TRUE`, the resulting object will be collapsed down into a single
  `visreg` object. If `FALSE`, the resulting object will be a
  `visreg_list`.

## Value

A `visreg` or `visreg_list` object, depending on the value of
`collapse`.

## See also

[`visreg()`](https://pbreheny.github.io/visreg/reference/visreg.md),
[`plot.visreg()`](https://pbreheny.github.io/visreg/reference/plot.visreg.md)

## Examples

``` r
fit <- lm(Ozone ~ Solar.R + Wind + Temp, data = airquality)
v1 <- visreg(fit, "Wind", plot = FALSE, alpha = 0.2)
v2 <- visreg(fit, "Wind", plot = FALSE, alpha = 0.01)
vv1 <- visreg_list(v1, v2, collapse = FALSE)
vv2 <- visreg_list(v1, v2,
  collapse = TRUE,
  labels = c("Confidence: 0.80", "Confidence: 0.99")
)
plot(vv1)


plot(vv2)

```
