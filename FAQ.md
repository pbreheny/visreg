## Frequently asked questions

1. Can visreg can be used for mixed models (i.e., from the 'nlme' or 'lme4' packages)?

   Sort of.  The underlying basis on which visreg operates is by using the predict method to plot predictions from the model.  Predictions for mixed models are complicated.  In particular, there is no 'se.fit' option provided by the 'predict' methods in the 'nlme' and 'lme4' packages, so you cannot obtain confidence bands for conditional plots.  Nevertheless, 'visreg' will produce reasonable plots of estimated coefficients and partial residuals.

   Keep in mind, however, that, depending on what sort of predictions (BLUPs) you are interested in, random effects may or may not be incorporated into the plotted linear combinations of coefficients.  For example, in 'nlme' this is controlled by the 'level' argument.  Passing the appropriate 'level' to predict through 'visreg' is the user's responsibility; I cannot hope to automatically decide this for any possible model.

   If you are running into difficulty using 'visreg' with mixed models, feel free to e-mail me; comparatively little effort has been spent on testing 'visreg' with mixed models, and there may still be bugs to work out.

1. What is the difference between 'conditional' and 'contrast' plots?

   Suppose our data looked like:

   SBP    Sex    Age  
   140    M    56  
   135    F    47  
   ...  

   we fit a model with  

   fit <- lm(SBP~Sex+Age)  

   and we want to plot the relationship between Age and SBP.  A 'conditional' plot illustrates the relationship between the two, conditional on the sex being, say, Male (the default in visreg is to choose the most common category).

   The 'contrast' plot in visreg, on the other hand, illustrates the effect on SBP of a *change* in age -- the default in visreg is to use the mean age as the reference point for this change.  Since the above model does not have an interaction, this effect will be the same for men and women, and thus does not require you to specify a sex for the plot.  

   Both conditional and contrast plots answer subtly different questions, and both are useful in different situations.  

1. How do I use visreg for a model with offset terms?

   By default, visreg is set up to provide conditional plots in which all other terms are set to their median value (or most common category).  This includes offset terms.  It is not uncommon, however, to want to see results with the offset included.  To obtain these results, one needs to specify the offset among the arguments to cond.  For example, using the Insurance data from the MASS package:

   utils::data(Insurance, package="MASS")  
   fit <- glm(Claims ~ District + Group + Age + offset(log(Holders)), data = Insurance, family = poisson)  
   visreg(fit, "Group", scale="response")  

   This will provide the model's predictions for the expected number of claims given the median number of holders (here, 136).  To obtain the expected number of claims per holder, we need to specify Holders=1 in cond:

   visreg(fit, "Group", scale="response", cond=list(Holders=1))

   Note also that to ensure proper functionality with all of visreg's options, the use of the offset() function, rather than the offset= argument, is recommended.