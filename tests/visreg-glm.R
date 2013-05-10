require(visreg)
data("birthwt",package="MASS")
birthwt$race <- factor(birthwt$race,labels=c("White","Black","Other"))
birthwt$smoke <- factor(birthwt$smoke,labels=c("Nonsmoker","Smoker"))

## Basic
fit <- glm(low~age+race+smoke+lwt,data=birthwt,family="binomial")
visreg(fit,"age")
visreg(fit,"lwt")
visreg(fit,"race")
visreg(fit, "race", cond=list(smoke='Smoker'))
visreg(fit,"smoke")

## Transformation of X
fit <- glm(low~age+I(age^2)+race+smoke+lwt,data=birthwt,family="binomial")
visreg(fit,"age")
visreg(fit,"age",type="effect")

## Plot on response scale
fit <- glm(low~age+race+smoke+lwt,data=birthwt,family="binomial")
visreg(fit,"age",scale="response")
visreg(fit,"lwt",scale="response")

## Cond
visreg(fit, "lwt", scale="response", cond=list(smoke='Smoker'))
visreg(fit, "lwt", scale="response", by="smoke")
