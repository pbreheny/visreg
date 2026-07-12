# visreg and ggplot2

`visreg` plots are built with `ggplot2`. For example:

``` r

fit <- lm(Ozone ~ Solar.R + Wind + Temp, data = airquality)
visreg(fit, "Wind")
```

![](gg_files/figure-html/unnamed-chunk-2-1.png)

[Graphical options](https://pbreheny.github.io/visreg/articles/options)
regarding the appearance of points, lines, and bands are specified via
the `line`, `fill`, and `points` arguments:

``` r

visreg(fit, "Wind",
  line = list(color = "red"),
  fill = list(fill = "green"),
  points = list(size = 2, shape = 1)
)
```

![](gg_files/figure-html/unnamed-chunk-3-1.png)

`visreg` returns a `gg` object, so you can use `ggplot2` to add
additional layers to the graph. For example, we could add a smoother:

``` r

visreg(fit, "Wind") + geom_smooth(method = "loess", col = "#FF4E37", fill = "#FF4E37")
```

![](gg_files/figure-html/unnamed-chunk-4-1.png)

Or we could modify the theme:

``` r

visreg(fit, "Wind") + theme_bw()
```

![](gg_files/figure-html/unnamed-chunk-5-1.png)
