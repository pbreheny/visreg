\name{subset.visreg}
\alias{subset.visreg}
\title{Subset a visreg object}
\description{
Subset a visreg object so that only a portion of the full
model is plotted.
}
\usage{
\method{subset}{visreg}(x, sub, ...)
}
\arguments{

  \item{x}{
    A \code{\link{visreg}} object.
  }

  \item{sub}{
    Logical expression indicating elements to keep, as in
    \code{\link{subset}}.
  }
  
  \item{\dots}{
    Not used.
  }
}
\examples{
# Fit a model and construct a visreg object
airquality$Heat <- cut(airquality$Temp,3,labels=c("Cool","Mild","Hot"))
fit <- lm(Ozone~ Solar.R + Wind*Heat,data=airquality)
v <- visreg(fit, "Wind", by="Heat", plot=FALSE)

# Plot only certain levels
vv <- subset(v, Heat \%in\% c("Cool", "Hot"))
plot(vv)

# Plot only up to wind 15 mph
vv <- subset(v, Wind < 15)
plot(vv)
}
