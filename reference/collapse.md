# Collapse a `visreg_list` into a single `visreg` object

Takes a `visreg_list`, as produced by
[`visreg_list()`](https://pbreheny.github.io/visreg/reference/visreg_list.md)
or [`visreg()`](https://pbreheny.github.io/visreg/reference/visreg.md),
and collapses it down into a single `visreg` object. This is equivalent
to calling
[`visreg_list()`](https://pbreheny.github.io/visreg/reference/visreg_list.md)
with `collapse = TRUE` in the first place; it is provided separately so
that a `visreg_list` can be collapsed after the fact, e.g.
`visreg_list(v1, v2) |> collapse()`.

## Usage

``` r
collapse(x, labels)
```

## Arguments

- x:

  A `visreg_list` object.

- labels:

  A character vector with length corresponding to the number of `visreg`
  objects in `x` that provides labels for the different objects in
  subsequent plots. If missing, the response variable names are used (or
  `Y1`, `Y2`, ... if these are not unique).

## Value

A `visreg` object.

## See also

[`visreg_list()`](https://pbreheny.github.io/visreg/reference/visreg_list.md),
[`visreg()`](https://pbreheny.github.io/visreg/reference/visreg.md)

## Examples

``` r
fit <- lm(Ozone ~ Solar.R + Wind + Temp, data = airquality)
v1 <- visreg(fit, "Wind", plot = FALSE, alpha = 0.2)
v2 <- visreg(fit, "Wind", plot = FALSE, alpha = 0.01)
labs <- c("Confidence: 0.80", "Confidence: 0.99")
vv1 <- visreg_list(v1, v2, collapse = FALSE) |> collapse(labels = labs)
vv2 <- visreg_list(v1, v2, collapse = TRUE, labels = labs)
identical(vv1, vv2)
#> [1] TRUE
```
