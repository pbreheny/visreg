<!-- badges: start -->
[![GitHub version](https://img.shields.io/endpoint?url=https://raw.githubusercontent.com/pbreheny/visreg/master/.version.json&style=flat&logo=github)](https://github.com/pbreheny/visreg)
[![CRAN version](https://img.shields.io/cran/v/visreg?logo=R)](https://cran.r-project.org/package=visreg)
[![downloads](http://cranlogs.r-pkg.org/badges/visreg)](https://cran.r-project.org/package=visreg)
[![codecov.io](https://codecov.io/github/pbreheny/visreg/coverage.svg?branch=master)](https://codecov.io/github/pbreheny/visreg?branch=master)
[![R-CMD-check](https://github.com/pbreheny/visreg/workflows/R-CMD-check/badge.svg)](https://github.com/pbreheny/visreg/actions)
<!-- badges: end -->

# visreg: Visualization of Regression Models

**visreg** is an R package for displaying the results of a fitted model in terms of how a predictor variable `x` affects an outcome `y`.  The implementation of **visreg** takes advantage of object-oriented programming in R, meaning that it works with virtually any type of formula-based model in R provided that the model class provides a `predict()` method:  `lm`, `glm`, `gam`, `rlm`, `nlme`, `lmer`, `coxph`, `svm`, `randomForest` and many more.

## Installation

To install the latest release version from CRAN:

```r
install.packages("visreg")
```

To install the latest development version from GitHub:

```r
remotes::install_github("pbreheny/visreg")
```

## Usage

The basic usage is that you fit a model, for example:

```r
fit <- lm(Ozone ~ Solar.R + Wind + Temp, data=airquality)
```

and then you pass it to `visreg`:

```r
visreg(fit, "Wind")
```

<p align="center">
<img alt="img" src="https://pbreheny.github.io/visreg/articles/web/index_files/figure-html/unnamed-chunk-3-1.png" style="width: 50%;">
</p>

A more complex example, which uses the `gam()` function from **mgcv**:

```r
airquality$Heat <- cut(airquality$Temp, 3, labels=c("Cool", "Mild", "Hot"))
fit <- gam(Ozone ~ s(Wind, by=Heat, sp=0.1), data=airquality)
visreg(fit, "Wind", "Heat", gg=TRUE, ylab="Ozone")
```

<p align="center">
<img alt="img" src="https://pbreheny.github.io/visreg/articles/web/index_files/figure-html/unnamed-chunk-4-1.png" style="margin:auto; width: 80%;">
</p>

## More information

For more information on **visreg** syntax and how to use it, see:

* The online documentation at <https://pbreheny.github.io/visreg> contains many examples of visreg plots and the code to create them.
* [Breheny P and Burchett W (2017).  Visualization of Regression Models Using visreg. *The R Journal*, 9: 56-71.](https://journal.r-project.org/archive/2017/RJ-2017-046/index.html)

The website focuses more on syntax, options, and user interface, while the paper goes into more depth regarding the statistical details.
