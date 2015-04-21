require(randomForest)
require(visreg)

## Regression
fit <- randomForest(Ozone ~ ., data=airquality, na.action=na.omit)
visreg(fit, "Temp")
visreg(fit, "Wind", by="Temp", layout=c(3,1))
visreg(fit, "Wind", by="Temp", overlay=TRUE)

## Classification
data("birthwt",package="MASS")
Data <- birthwt[,1:9]
Data$race <- factor(Data$race,labels=c("White","Black","Other"))
Data$smoke <- factor(Data$smoke,labels=c("Nonsmoker","Smoker"))
Data$low <- factor(Data$low)
fit <- randomForest(low ~ ., data=Data)
visreg(fit, "lwt", xlab="Mom's weight")
visreg(fit, "lwt", by="smoke", xlab="Mom's weight", overlay=TRUE)

## 2D
fit <- randomForest(Ozone ~ ., data=airquality, na.action=na.omit)
visreg2d(fit, "Wind", "Temp")
visreg2d(fit, "Wind", "Temp", plot="rgl")
