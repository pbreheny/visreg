# visreg 2.8.0
  * New: cond argument now accepts variables that aren't in the model; fixes #67
  * Change: xtrans now uses more sensible spacing; fixes #118
  * Change: se.fit no longer turned off for lme4 merMod objects
  * Fixed: collapseVisregList bug #86
  * Fixed: Bug if function call applied to data
  * Fixed: contrast plots work for betareg
  * Fixed: strip.names now passed to overlay when gg=TRUE; fixes #104
  * Fixed: Now finds data in package namespace; #112
  * Fixed: Several compatibility issues with glmmTMB models resolved
  * Fixed: betareg issue
  * Fixed: mixed models no longer crash when used with random intercept only
  * Internal: Now uses Roxygen

# visreg 2.7.0 (2020-06-04)
  * Changed: Plots are now constructed in a consistent order across all
    varieties (lattice, base, gg, overlay, etc.), with lines on top (fixes #50)
  * New: Option to change the default layering with top="points"
  * Fixed: Resolved namespace issue with factor plots using lattice
  * Fixed: Explicitly convert to factor (no longer automatic as of R 4.0)
  * Fixed: strip.names and rug colors now work correctly for ggplot (fixes #85)
  
# visreg 2.6.1 (2020-03-13)
  * Fixed: Better handling of S4 formulas and data
  * Fixed: xlim no longer used in setting up visreg object; fixes #81
  * Changed: Added warning if user attempts to combine contrast and
    transformation; fixes #78
  * Internal: Various internal changes for cleaner, more reliable code
  * New version numbering system

# visreg 2.6-0 (2019-11-27)
  * New: visreg(), visreg2d() now accept data= option for explicit data passing
  * Fixed: coxph models now work when update() is required (#76)
  * Fixed: Improved formula parsing (now uses all.vars)
  * Changed: plot.visreg2d() now returns transformation matrix for persp plots
  * Internal: TravisCI now used for automatic testing
  * Internal: More consistent handling of matrix outcome (mlm)

# visreg 2.5-1 (2019-06-26)
  * Fixed: Restoring compatibility with current version of quantreg
  * Fixed: Restoring compatibility with current version of survey

# visreg 2.5-0 (2018-02-26)
  * New: Overlay now works for gg plots
  * New: gg option for visreg2d
  * New: Improved support for betareg models
  * New: Support for polr models
  * Changed: visreg2d now split into calculation and plotting functions
  * Changed: vignette now HTML instead of PDF
  * Fixed: Formula parser now handles nested parentheses
  * Fixed: Multi-reponse models with missing residuals() methods
  * Fixed: Rugs now work with gg plots
  * Fixed: Formula parsing for strata() in coxph models

# visreg 2.4-1 (2017-06-23)
  * New: Can now specify reference level for contrast plots by including x
    variable in 'cond' list (thank you GitHub@jealie for implementing this!)
  * Changed: Default for transformed outcomes now rug=2
  * Changed: Default color for persp plot
  * Changed: visreg(fit) no longer plots constant objects
  * Changed: Appearance of strip names for numeric 'by' variables
  * Fixed: plot.visreg wasn't plotting gg objects by default
  * Fixed: Bug in formula parsing w/ complicated interactions
  * Fixed: Bugs in ggFactorPlot
  * Fixed: print.cond warning using warning() instead of printing text

# visreg 2.4-0 (2017-06-09)
  * New: gg=TRUE works for regular plots, not just cross-sectional plots
  * New: gg=TRUE returns a gg object, which can then be manipulated further
  * Fixed: bug in coxph models with type='contrast'

# visreg 2.3-0 (2016-07-30)
  * New: Added support for multi-response models such multinom, with
    accompanying visregList() function and collapse option for further
    control/options
  * New: Added option to use ggplot2 instead of lattice to produce multi-panel
    plots
  * New: Support for glmmADMB models
  * Documentation: Added quick-start vignette
  * Fixed: Bug in which user options were not being passed to axis() when
    the horizontal axis was a factor
  * Fixed: Bug in rounding for legend in overlay plots with numeric by
    variable
  * Changed: Partial residuals are now on top of lines so that they cannot be
    hidden by them
  * Internal: Reorganized and greatly expanded suite of tests
  * Fixed: Now handles mlm objects with no y names
  * Fixed: Now handles gamm4 models
  * Fixed: Some issues with svm models (package: e1071)

# visreg 2.2-2 (2016-02-06)
  * Fixed: bug involving an incompatibility between formulas with a . and
    formulas with an s() in them (2.2-1 extended visreg to formulas with
    a ., but this broke compatibility with formulas with s() and similar
    terms)

# visreg 2.2-1 (2016-01-05)
  * New: Improved support for packages that do not provide a residuals() generic
  * Changed: Deviance residuals now used for 'coxph' (survival) models instead
    of martingale residuals
  * Internal: Changes to NAMESPACE for compatibility with development versions
    of R
  * Fixed: Now compatible with 'quantreg' and 'betareg' packages

# visreg 2.2-0 (2015-04-22)
  * New: Added support for random forests from the 'randomForest' package
  * New: Added support for models from the 'rms' package

# visreg 2.1-1 (2015-02-25)
  * New: plot.visreg can now return trellis objects so that you can
    arrange multiple trellis plots to your liking.
  * Fixed: Contrast plots now work for lme4 models
  * Fixed: Survival examples no longer depend on splines

# visreg 2.1-0 (2014-11-27)
  * New: visreg now returns simple, structured objects that a user can modify
    and inspect prior to plotting
  * Internal: Major overhaul to visreg's internal structure
  * Internal: visreg() now separated from plot.visreg()
  * Fixed: Improved formula parsing so that it now works properly for
    te() and ti() models (mgcv), pspline models (survival), more 
    complex formulas from lme4, and more (thank you to Andrew
    Vitale for giving me a good, complex model formula to test
    against)
  * Fixed: strip.names now works for factors and shingles for both overlay and
    lattice plots (thank you to Dan Silver for pointing this out).

# visreg 2.0-6 (2015-08-26)
  * Changed: Rugs are now plotted by default instead of partial
    residuals when a transformation has been applied
  * Fixed: Issue with S4 models not being compatible with
    as.formula(); visreg now refrains from calling as.formula (thank you to
    Nick Livingston for bringing this issue to my attention)

# visreg 2.0-5 (2014-05-30)
  * Fixed: Bug arising when passing plot options such as 'xlim'
    when used in conjunction with lme4 models; thank you to
    Vincent Maire for pointing this bug out

# visreg 2.0-4 (2013-10-29)
  * New: Added compatibility with models from the 'lme4' package; tThank you to
    Jack Hogg for pointing out this incompatibility (same limitations regarding
    error bands as 'lme' models)

# visreg 2.0-3 (2013-09-27)
  * New: Added compatibility with 'lme' models; conditional models
    still cannot include error bands due to lack of this feature
    in predict.lme, but the package will produce reasonable plots
    of means and partial residuals (Thank you to Jason Rohr for 
    pointing out this incompatibility and the bugs below)
  * New: Pption to manually relabel panel strips
  * Fixed: Bug in passing 'scales' to latticePlot
  * Fixed: Bug when trying to panel by every unique value of 'by' variable

# visreg 2.0-2 (2013-08-28)
  * New: Compatible with output from the 'survey' package; thank
    you to Marco Pomati for pointing this incompatibility out
  * Internal: Fixed calling of generics to be compatible with changes in
    R version 3.1
  * Fixed: Bug for mlm models in which users received an incorrect warning
    about residuals
  * Fixed: Improved generalizability of fillFrame with factors; was incompatible
    with some instances of predict()

# visreg 2.0-1 (2013-08-10)
  * Fixed: Bug for spline models of variables with '.' in their names
  * Fixed: Bug for survival models in which the Surv object is created outside
    the call to visreg
  * Fixed: Levels of a factor no longer dropped from the model; thank you to
    Johannes Kutsam for pointing this out
  * Fixed: Scoping issue resolved -- can now call visreg, visreg2d from a
    function without fit or data present in the global environment; thank you
    to Johannes Kutsam for pointing this out

# visreg 2.0-0 (2013-05-23)
  * New: Added 'overlay' option
  * New: Extrapolation now allowed
  * New: print.cond option, with warning when user tries to plot a main effect
    in the presence of interactions
  * New: 'rug' option
  * New: 'band' option to suppress confidence bands
  * New: 'ask' option to turn off prompting when starting a new page of plots
  * Changed: Changed how 'breaks' are handled for continuous 'by' variables,
    both in setup and appearance, with option to directly specify where breaks
    should occur
  * Fixed: Bug for models which were called with 'subset' option
  * Fixed: Minor bug in passing lattice options
  * Internal: Substantial internal refactoring to separate calculations /
    model manipulations from plotting
  * Internal: Expanded suite of tests

# visreg 1.2-1 (2013-02-21)
  * Fixed: Bug in calculation of confidence bands for quasilikelihood models;
    thank you to Ariel Muldoon for pointing this out
  * Fixed: Side-effect which caused trellis settings to be changed when visreg
    changed them internally

# visreg 1.2-0 (2013-01-20)
  * New: Support for models from the 'gam' package
  * New: Improved support for 'coxph' models from 'survival' package
  * Fixed: Bug in 'removeFormulaFormatting' to allow further compatibility for
    splines with use of 'cond'
  * Fixed: Bug in use of ns/bs with type='effect'

# visreg 1.1-1 (2012-11-14)
  * Fixed: Bug in 'removeFormulaFormatting'; now compatible with ns() and bs()
    from the 'splines' package

# visreg 1.1-0 (2012-09-28)
  * New: Finer control of plots with line.par, fill.par, points.par
  * New: Support for multiple response (class 'mlm') models
  * Fixed: Bug in 'ask' behavior when multiple plots created
  * Fixed: Bug with formatting names for locfit objects
  * Fixed: Bug with visregFactorPanel when partial=FALSE
  * Changed: Default for ylabels now las=1 (always horizontal); thank you to
    Doug Bates for this advice

# visreg 1.0-0 (2012-05-31)
  * New: Now on CRAN!
  * New: Improved support for contrast plots:
    * Extensive revision to implementation
    * 'x' can now be categorical
    * interactions/conditioning allowed
  * New: Improved support for visreg2d
    * Interface/options consistent with visreg
    * Internal refactoring to share code between visreg and visreg2d
    * Works now with glm/rlm/gam/etc.
  * New: Option to display panel strip names T/F
  * New: Improved support for gam/locfit models
  * Changed: Default (contrast/conditional) for coxph models and
  * Changed: Default 'ylim' now depends on 'partial' argument
  * Changed: Added some padding to xlim/ylim for lattice plots
  * Changed: Default for 'ylab': Delta added for effect plot
  * Deprecated: 'fill' option; always median fill from now on
  * Fixed: Bug in 'ask' behavior when multple plots created
  * Fixed: Bug in coxph contrast plots
  * Fixed: Patched 'get_all_vars' bug when variables are in environment rather
    than data frame

# visreg 0.4-0 (2012-02-18)
  * Internal: New 'setupCond' function to handle setting up of cond when 'by'
    variables are present
  * Internal: New functions 'visregPlot' and 'visregLatticePlot' to separate
    work done by the two types of plots ('by' present or absent)
  * Internal: New function getXY for shared code
  * Changed: Handling of whitespace when 'x' is a factor
  * Changed: Default for 'ylab' changed to always be name of response variable;
    virtually impossible in the presense of transformations, link functions,
    etc., to automatically choose an appropriate name
  * Fixed: Plotting options now pass to lattice correctly
  * Fixed: ylim for lattice plots
  * Fixed: xtrans for lattice
  * Changed: Manner in which cross-sections are taken when 'by' is numeric;
    number of observations in each cross-section is now more even

# visreg 0.3-0 (2011-12-03)
  * New: Added 'jitter' option
  * New: Improved handling of trellis parameters
  * Internal: Reorganized frame construction again, using 'get_all_vars';
    now works with missing data and various other strange situations where f(x)
    is in the model but x isn't

# visreg 0.2-0 (2011-08-23)
  * New: Added 'by' argument to support interactions
  * New: visreg() now returns 'x' and 'y' invisibly
  * New: Added 'scale' option for GLMs
  * Internal: Reorganized frame construction, using 'extract.frame'
  * Changed: Default behavior of CI widths to make compatible with coxph models
  * Changed: Resolved type/type2 conflict in visreg2d with 'plot.type' option
  * Documentation: Added documentation

# visreg 0.1-0 (2011-07-22)
  * New: Added glm suport
  * New: Added factor support in 1d version
  * New: Added "terms" option for 2d plots
  * New: Added 'cond' option for setting specific predictor variables
  * New: 'partial' and 'xtrans' options
  * Changed: The most common instance of each factor is now chosen by default
    if the model includes factors
  * Fixed: Bug for transformations for 2d plots
  * Fixed: Bug that occurs when model is fit without supplying a data frame
    (i.e., when all variables are in the global environment)
  * Fixed: Bug that occurs when a variable is class 'logical'

# visreg 0.0-3 (2011-07-02)
  * New: 'trans' option for transforming response variable
  * Intenal: Changed internal data frame evaluation, allowing the ability to
    lookup variables in the original data frame when constructing derived
    variables

# visreg 0.0-2 (2011-06-24)
  * New: fill='median' option

# visreg 0.0-1 (2011-06-09)
  * New: 'type' option to visreg, allowing both conditional and contrast plots
  * New: fill='zero' option
  * Changed: Default y label in visreg

# visreg 0.0-0 (2011-05-12)
  * New: Package infrastructure established
  * New: visreg.R, visreg2d.R: Polynomial terms handled correctly
