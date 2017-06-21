[![version](http://www.r-pkg.org/badges/version/visreg)](https://cran.r-project.org/package=visreg)
[![downloads](http://cranlogs.r-pkg.org/badges/visreg)](https://cran.r-project.org/package=visreg)
[![codecov.io](https://codecov.io/github/pbreheny/visreg/coverage.svg?branch=master)](https://codecov.io/github/pbreheny/visreg?branch=master)

# Visualization of Regression Models

`visreg` provides a number of plotting functions for visualizing fitted regression models: regression functions, confidence bands, partial residuals, interactions, and more.  `visreg` is compatible with virtually all formula-based models in R that provide a `predict` method: `lm`, `glm`, `gam`, `rlm`, `nlme`, `lmer`, `coxph`, `svm`, `randomForest` and many more.

<img alt="example image" src="http://pbreheny.github.io/visreg/img/transformations-unnamed-chunk-5-1.png" style="margin:auto;">

For details on `visreg` syntax and how to use it, see:

* The online documentation at <http://pbreheny.github.io/visreg> contains many examples of visreg plots and the code to create them.
* [Breheny P and Burchett W. (2013).  Visualizing regression models using visreg.](http://myweb.uiowa.edu/pbreheny/publications/visreg.pdf)

The website focuses more on syntax, options, and user interface, while the paper goes into more depth regarding the statistical details.

If you have a question or feature request, please [submit an issue](https://github.com/pbreheny/visreg/issues).

To install:

* the latest released version: `install.packages("visreg")`
* the latest version (requires `devtools`): `install_github("pbreheny/visreg")`
