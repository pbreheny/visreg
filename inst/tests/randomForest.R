require(randomForest)
require(visreg)

## Regression
fit <- randomForest(Ozone ~ ., data=airquality, na.action=na.omit)
visreg(fit, "Temp")
visreg(fit, "Wind", by="Temp")
visreg(fit, "Wind", by="Temp", overlay=TRUE)

## Classification
data("birthwt",package="MASS")
Data <- birthwt[,1:9]
Data$race <- factor(Data$race,labels=c("White","Black","Other"))
Data$smoke <- factor(Data$smoke,labels=c("Nonsmoker","Smoker"))
Data$low <- factor(Data$low)
fit <- randomForest(low ~ ., data=Data)
visreg(fit, "lwt", ylab="P(Low birth weight", xlab="Mom's weight")
visreg(fit, "lwt", by="race")
