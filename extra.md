
A number of options for [surface plots](surface.html) are also provided, such as `rgl` plots:

```r
library(splines)
fit <- lm(Ozone ~ Solar.R +ns(Wind, df=2)*ns(Temp, df=2), data=airquality)
visreg2d(fit, "Wind", "Temp", plot.type="rgl")
```

```{=html}
<div class="container" style="width: 100%">
  <div class="row-fluid">
    <iframe class="span12" 
	    style="border: none; height: 484px; width: 100%"
	    src="articles/rgl.html">
    </iframe>
  </div>
</div>
```

If your browser supports it, you should be able to interact with above figure (click and drag to rotate).

If you have a question or feature request, please [submit an issue](https://github.com/pbreheny/visreg/issues).

## More information

The documentation provided here focuses on syntax, options, and user interface. For more details on the statistical methodology and implementation, see:

* [Breheny P and Burchett W (2017).  Visualization of Regression Models Using visreg. *The R Journal*, 9: 56-71.](https://journal.r-project.org/archive/2017/RJ-2017-046/index.html)
