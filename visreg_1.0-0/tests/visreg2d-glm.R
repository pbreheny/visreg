require(visreg)
data("birthwt",package="MASS")
birthwt$race <- factor(birthwt$race,labels=c("White","Black","Other"))
birthwt$smoke <- factor(birthwt$smoke,labels=c("Nonsmoker","Smoker"))

## Basic
fit <- glm(low~age+race+smoke+lwt,data=birthwt,family="binomial")
visreg2d(fit,"age","lwt")
visreg2d(fit,"age","lwt",scale="response")

## Transformation of X
fit <- glm(low~age+race+smoke+log(lwt),data=birthwt,family="binomial")
visreg2d(fit,"age","lwt")
visreg2d(fit,"age","lwt",type="effect")
visreg2d(fit,"age","lwt",scale="response")

## Cond
visreg2d(fit,"age","lwt",scale="response",cond=list(smoke='Smoker'))

## Factors
visreg2d(fit,"age","race",cond=list(smoke='Smoker'))
