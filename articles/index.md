# 

``` r

fit <- lm(Ozone ~ Solar.R + Wind + Temp, data=airquality)
```

and then you pass it to `visreg`:

``` r

visreg(fit, "Wind")
```

![](index_files/figure-html/unnamed-chunk-3-1.png)

A more complex example, which uses the
[`gam()`](https://rdrr.io/pkg/mgcv/man/gam.html) function from **mgcv**:

``` r

library(mgcv)
# Loading required package: nlme
# This is mgcv 1.9-4. For overview type '?mgcv'.
airquality$Heat <- cut(airquality$Temp, 3, labels=c("Cool", "Mild", "Hot"))
fit <- gam(Ozone ~ s(Wind, by=Heat, sp=0.1), data=airquality)
visreg(fit, "Wind", "Heat", gg=TRUE, ylab="Ozone")
# Loading required namespace: ggplot2
```

![](index_files/figure-html/unnamed-chunk-4-1.png)
