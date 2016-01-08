source('~/dev/.visreg.setup.R')

# quantreg
require(quantreg)
fit <- rq(Ozone ~ ., data=airquality)
visreg(fit, 'Wind')
fit <- rq(Ozone ~ ., data=airquality, tau=0.1)
visreg(fit, 'Wind')

# betareg
require(betareg)
data("GasolineYield", package = "betareg")
fit <- betareg(yield ~ batch + temp, data = GasolineYield)
visreg(fit, 'temp')

# Random forest
require(randomForest)
fit <- randomForest(Ozone ~ ., data=airquality, na.action=na.omit)
visreg(fit, "Temp")

# Random forest - binary
data("birthwt",package="MASS")
Data <- birthwt[,1:9]
Data$race <- factor(Data$race,labels=c("White","Black","Other"))
Data$smoke <- factor(Data$smoke,labels=c("Nonsmoker","Smoker"))
Data$low <- factor(Data$low)
fit <- randomForest(low ~ ., data=Data)
visreg(fit, "lwt", ylab="P(Low birth weight", xlab="Mom's weight")

# gbm
require(gbm)
Data <- airquality[complete.cases(airquality),]
fit <- gbm(Ozone ~ ., data=Data)
visreg(fit, 'Wind')
visreg(fit, 'Wind', partial=FALSE)
residuals.gbm <- function(fit) Data$Ozone - fit$fit  # supplying a resid() function
visreg(fit, 'Wind')
fit <- gbm(Ozone ~ ., data=Data, n.trees=10000)
gbm.perf(fit)
visreg(fit, 'Wind')
fit <- gbm(Ozone ~ ., data=Data, n.trees=20000, cv.folds = 10)
gbm.perf(fit)
visreg(fit, 'Wind', n.trees=gbm.perf(fit))
visreg(fit, 'Wind')

# gbm, binary outcome
data("birthwt",package="MASS")
Data <- birthwt[,1:9]
Data$race <- factor(Data$race,labels=c("White","Black","Other"))
Data$smoke <- factor(Data$smoke,labels=c("Nonsmoker","Smoker"))
fit <- gbm(low ~ age + lwt, data=Data, n.trees = 10000)
residuals.gbm <- function(fit) Data$low
visreg(fit, 'lwt', trans=binomial()$linkinv, ylim=0:1, rug=2)
