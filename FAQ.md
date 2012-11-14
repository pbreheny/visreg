## Frequently asked questions

1. Can visreg can be used for GLMMs (i.e., from the lme4 package)?
The underlying basis on which visreg operates is by using the predict method to plot predictions from the model.  Predictions for mixed models are complicated.  In principle, it is possible to make predictions from a GLMM (albeit with caveats), but in practice, there is no 'predict' method provided by the lme4 package.  If Doug Bates (the author of lme4) or someone else writes a predict function for lme4, then it will work with visreg; otherwise not.
1. What is the difference between 'conditional' and 'effect' plots?
Suppose our data looked like:
SBP    Sex    Age
140    M    56
135    F    47
...
we fit a model with
fit <- lm(SBP~Sex+Age)
and we want to plot the relationship between Age and SBP.  A 'conditional' plot illustrates the relationship between the two, conditional on the sex being, say, Male (the default in visreg is to choose the most common category).  This is similar to what the effects package provides, although its default approach is to illustrate the relationship between Age and SBP for the 'average' sex, here a hypothetical half-man half-woman.  If you want such a conditional plot in visreg, you can obtain it, but you have to specify this explicitly using the 'cond' argument.
The 'effect' plot in visreg, on the other hand, illustrates the effect on SBP of a *change* in age -- the default in visreg is to use the mean age as the reference point for this change.  Since the above model does not have an interaction, this effect will be the same for men and women, and thus does not require you to specify a sex for the plot.
Both conditional and effect plots answer subtly different questions, and both are useful in different situations.
