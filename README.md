[![GitHub version](https://img.shields.io/endpoint?url=https://raw.githubusercontent.com/pbreheny/visreg/master/.version.json&style=flat&logo=github)](https://github.com/pbreheny/visreg)
[![CRAN version](http://www.r-pkg.org/badges/version/visreg)](https://cran.r-project.org/package=visreg)
[![downloads](http://cranlogs.r-pkg.org/badges/visreg)](https://cran.r-project.org/package=visreg)
[![codecov.io](https://codecov.io/github/pbreheny/visreg/coverage.svg?branch=master)](https://codecov.io/github/pbreheny/visreg?branch=master)
[![Travis build status](https://api.travis-ci.org/pbreheny/visreg.svg?branch=master)](https://travis-ci.org/pbreheny/visreg)

# Visualization of Regression Models

`visreg` provides a number of plotting functions for visualizing fitted regression models: regression functions, confidence bands, partial residuals, interactions, and more.  `visreg` is compatible with virtually all formula-based models in R that provide a `predict` method: `lm`, `glm`, `gam`, `rlm`, `nlme`, `lmer`, `coxph`, `svm`, `randomForest` and many more.

The basic usage is that you fit a model, for example:

```r
fit <- lm(Ozone ~ Solar.R + Wind + Temp, data=airquality)
```

and then you pass it to `visreg`:

```r
visreg(fit, "Wind")
```

<p align="center">
<img alt="img" src="http://pbreheny.github.io/visreg/img/index-wind-1.png">
</p>

A more complex example, using the `mgcv` package:

```r
airquality$Heat <- cut(airquality$Temp, 3, labels=c("Cool", "Mild", "Hot"))
fit <- gam(Ozone ~ s(Wind, by=Heat, sp=0.1), data=airquality)
visreg(fit, "Wind", "Heat", gg=TRUE, ylab="Ozone")
```

<p align="center">
<img alt="img" src="http://pbreheny.github.io/visreg/img/index-mgcv-1.png" style="margin:auto;">
</p>

For details on `visreg` syntax and how to use it, see:

* The online documentation at <http://pbreheny.github.io/visreg> contains many examples of visreg plots and the code to create them.
* [Breheny P and Burchett W (2017).  Visualization of Regression Models Using visreg. *The R Journal*, 9: 56-71.](https://journal.r-project.org/archive/2017/RJ-2017-046/index.html)

The website focuses more on syntax, options, and user interface, while the paper goes into more depth regarding the statistical details.

If you have a question or feature request, please [submit an issue](https://github.com/pbreheny/visreg/issues).

## Installation

To install the latest release version from CRAN:

```r
install.packages("visreg")
```

To install the latest development version from GitHub:

```r
remotes::install_github("pbreheny/visreg")
```
