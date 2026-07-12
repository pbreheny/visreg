# visreg: Visualization of Regression Models

**visreg** is an R package for displaying the results of a fitted model
in terms of how a predictor variable `x` affects an outcome `y`. The
implementation of **visreg** takes advantage of object-oriented
programming in R, meaning that it works with virtually any type of
formula-based model in R provided that the model class provides a
[`predict()`](https://rdrr.io/r/stats/predict.html) method: `lm`, `glm`,
`gam`, `rlm`, `nlme`, `lmer`, `coxph`, `svm`, `randomForest` and many
more.

## Installation

To install the latest release version from CRAN:

``` r

install.packages("visreg")
```

To install the latest development version from GitHub:

``` r

remotes::install_github("pbreheny/visreg")
```

Note that version 3.0 of visreg introduced a number of [breaking
changes](https://pbreheny.github.io/visreg/articles/migrating-to-3-0.html);
if you wish to install the “legacy” version of visreg, version 2.8.1 was
the final CRAN release before these changes took place:

``` r

remotes::install_version("visreg", "2.8.1")
```

## Usage

The basic usage is that you fit a model, for example:

``` r

fit <- lm(Ozone ~ Solar.R + Wind + Temp, data=airquality)
```

and then you pass it to `visreg`:

``` r

visreg(fit, "Wind")
```

![img](https://pbreheny.github.io/visreg/articles/web/index_files/figure-html/unnamed-chunk-3-1.png)

A more complex example, which uses the `gam()` function from **mgcv**:

``` r

airquality$Heat <- cut(airquality$Temp, 3, labels=c("Cool", "Mild", "Hot"))
fit <- gam(Ozone ~ s(Wind, by=Heat, sp=0.1), data=airquality)
visreg(fit, "Wind", "Heat", gg=TRUE, ylab="Ozone")
```

![img](https://pbreheny.github.io/visreg/articles/web/index_files/figure-html/unnamed-chunk-4-1.png)

## More information

For more information on **visreg** syntax and how to use it, see:

- The online documentation at <https://pbreheny.github.io/visreg/>
  contains many examples of visreg plots and the code to create them.
- [Breheny P and Burchett W (2017). Visualization of Regression Models
  Using visreg. *The R Journal*, 9:
  56-71.](https://doi.org/10.32614/RJ-2017-046)

The website focuses more on syntax, options, and user interface, while
the paper goes into more depth regarding the statistical details.
