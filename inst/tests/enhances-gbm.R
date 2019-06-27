library(gbm)

# Continuous outcome
require(gbm)
Data <- airquality[complete.cases(airquality),]
fit <- gbm(Ozone ~ ., data=Data)
visreg(fit, 'Wind')
visreg(fit, 'Wind', partial=FALSE)
residuals.gbm <- function(fit) Data$Ozone - fit$fit  # supplying a resid() function
visreg(fit, 'Wind')
fit <- gbm(Ozone ~ ., data=Data, n.trees=10000)
visreg(fit, 'Wind')

# Binary outcome
data("birthwt", package="MASS")
Data <- birthwt[,1:9]
Data$race <- factor(Data$race,labels=c("White","Black","Other"))
Data$smoke <- factor(Data$smoke,labels=c("Nonsmoker","Smoker"))
fit <- gbm(low ~ age + lwt, data=Data, n.trees = 10000)
residuals.gbm <- function(fit) Data$low
visreg(fit, 'lwt', trans=binomial()$linkinv, ylim=0:1, rug=2)
