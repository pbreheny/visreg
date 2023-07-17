suppressPackageStartupMessages(library(gbm))
if (interactive()) library(tinytest)

# Continuous outcome
Data <- airquality[complete.cases(airquality),]
fit <- gbm(Ozone ~ ., data=Data, distribution='gaussian')
expect_warning(visreg(fit, 'Wind'))
visreg(fit, 'Wind', partial=FALSE, rug=FALSE)

# Supply our own residuals() function
registerS3method('residuals', 'gbm', function(fit) Data$Ozone - fit$fit)
visreg(fit, 'Wind')
fit <- gbm(Ozone ~ ., data=Data, n.trees=10000, distribution='gaussian')
visreg(fit, 'Wind')

# Binary outcome
data("birthwt", package="MASS")
Data <- birthwt[,1:9]
Data$race <- factor(Data$race,labels=c("White","Black","Other"))
Data$smoke <- factor(Data$smoke,labels=c("Nonsmoker","Smoker"))
fit <- gbm(low ~ age + lwt, data=Data, n.trees = 10000, distribution='bernoulli')
registerS3method('residuals', 'gbm', function(fit) Data$low)
visreg(fit, 'lwt', trans=binomial()$linkinv, ylim=0:1, rug=2)
