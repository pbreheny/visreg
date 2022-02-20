\name{visreg-faq}
\alias{visreg-faq}
\alias{visregfaq}
\alias{visregFAQ}
\alias{visreg.faq}
\alias{visreg_faq}
\title{Frequently Asked Questions for visreg}
\description{This page tries to answer some of the questions that I get
  asked most often about how to use the visreg package.  If you have a
  question that does not appear below, I can be reached at
  <patrick-breheny@uiowa.edu>.}
\section{Frequent asked questions}{
  \enumerate{
    \item \bold{What is the difference between 'conditional' and
      'contrast' plots?} 

    Suppose our data looked like:

    \tabular{rrr}{
      SBP \tab Sex \tab Age\cr
      140 \tab   M \tab  56\cr
      135 \tab   F \tab  47\cr
      ...\cr}
    
    we fit a model with  

    \code{fit <- lm(SBP~Sex+Age)}

    and we want to plot the relationship between Age and SBP.  A
    'conditional' plot illustrates the relationship between the two,
    conditional on the sex being, say, Male (the default in visreg is to
    choose the most common category). 

    The 'contrast' plot in visreg, on the other hand, illustrates the
    effect on SBP of a \emph{change} in age -- the default in visreg is
    to use the mean age as the reference point for this change.  Since
    the above model does not have an interaction, this effect will be
    the same for men and women, and thus does not require you to specify
    a sex for the plot.   

    Both conditional and contrast plots answer subtly different
    questions, and both are useful in different situations.

    \item \bold{Can visreg can be used for mixed models (i.e., from the
      'nlme' or 'lme4' packages)?}

    Sort of.  The underlying basis on which visreg operates is by using
    the predict method to plot predictions from the model.  Predictions
    for mixed models are complicated.  In particular, there is no
    \code{se.fit} option provided by the \code{predict} methods in the
    \code{nlme} and \code{lme4} packages, so you cannot obtain
    confidence bands for conditional plots.  Nevertheless, \code{visreg}
    will produce plots of estimated coefficients and partial residuals.

    In addition, there may be certain nesting structures among the
    covariates that \code{visreg} cannot be aware of; for example, if
    you are trying to plot the effect of age for various individuals,
    fixing sex at \code{sex=Male}, this may involve setting the sex of
    female subjects to \code{Male} for the sake of the plot.  Whether
    such a plot has any meaning, you will have to judge for yourself.
    In general, contrast plots are more trustworthy than than
    conditional plots, given the intricacies of setting up conditions in
    a hierarchical model.
    
    Keep in mind that depending on what sort of predictions (BLUPs) you
    are interested in, you may need to manually control the inclusion
    of random effects in your predictions.  By default, visreg includes
    no random effects (i.e., \code{level=0} for \code{nlme} models and
    \code{re.form=NA} for \code{lme4} models).  If you are including a
    random effect as a \code{by} variable in \code{visreg}, you most
    likely want to add those effects back in, and you will have to do so
    manually, by directly specifying the appropriate \code{level} or
    \code{re.form} argument to \code{predict} (see \code{?predict.nlme}
    or \code{?predict.merMod}).  Handling this appropriately is the
    user's responsibility; I cannot hope to automatically decide this
    for all possible mixed models that could be passed to visreg.

    As mentioned above, you cannot obtain confidence bands for
    conditional plots.  In the words of the authors of the \code{lme4}
    package, "There is no option for computing standard errors of
    predictions because it is difficult to define an efficient method
    that incorporates uncertainty in the variance parameters"; hence no
    \code{se.fit} option.  You can, however, get confidence bands for
    \code{'contrast'} plots.  In a contrast plot, the random effects
    cancel and the above issue is avoided.

    If you are running into difficulty using \code{visreg} with mixed
    models, feel free to e-mail me; mixed models have been less
    extensively tested with \code{visreg} than fixed-effect models, and
    there may still be bugs to work out.

    \item \bold{How do I use visreg for a model with offset terms?}

    By default, visreg is set up to provide conditional plots in which
    all other terms are set to their median value (or most common
    category).  This includes offset terms.  It is not uncommon,
    however, to want to see results with the offset included.  To obtain
    these results, one needs to specify the offset among the arguments
    to cond. For example, using the Insurance data from the MASS
    package:

    \code{utils::data(Insurance, package="MASS")}\cr
    \code{fit <- glm(Claims ~ District + Group + Age +
      offset(log(Holders)), data = Insurance, family = poisson)}\cr
    \code{visreg(fit, "Group", scale="response")}\cr

    This will provide the model's predictions for the expected number of
    claims given the median number of holders (here, 136).  To obtain
    the expected number of claims per holder, we need to specify
    Holders=1 in cond:

    \code{visreg(fit, "Group", scale="response", cond=list(Holders=1))}

    Note also that to ensure proper functionality with all of visreg's
    options, the use of the \code{offset()} function, rather than the
    \code{offset=} argument, is recommended.

    \item \bold{Why doesn't visreg work with models I fit with package
      XXX?}

    \code{visreg()} relies on being able to call certain generic
    functions to interface with the fitted model object that is passed
    to it.  Specifically, if \code{fit} is the fit of a model that is
    passed to \code{visreg}, the following have to work:

    \code{model.frame(fit)}
    \code{formula(fit)}

    If they do not, there is nothing I can really do on the
    \code{visreg} end to get it to work -- the author of the package
    would need to add support for those generic functions to make it
    more portable.  If the above lines of code \emph{do} work and
    \code{visreg} still fails, please let me know -- perhaps there is a
    bug somewhere that I can fix.
}}
\author{
  Patrick Breheny and Woodrow Burchett
  
  Maintainer: Patrick Breheny <patrick-breheny@uiowa.edu>}
\references{
  \itemize{
    \item \url{https://pbreheny.github.io/visreg}
    \item Breheny, P. and Burchett, W. (2017), Visualizing regression
    models using visreg.
    \url{https://journal.r-project.org/archive/2017/RJ-2017-046/index.html}
  }
}
