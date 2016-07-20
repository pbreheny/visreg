fit <- lm(Ozone ~ Solar.R + poly(Wind, 4) + poly(Temp, 4), airquality)
visreg(fit, "Wind")
visreg(fit, "Temp")

if (require(lme4)) {
  data(Orthodont, package="nlme")
  Orthodont$nsex <- as.numeric(Orthodont$Sex=="Male")
  fit <- lmer(distance ~ age + (age|Subject) + (0+nsex|Subject) + (0 + nsex:age|Subject), data=Orthodont)
  visreg(fit, "age")
  #print(visreg:::parseFormula(formula(fit)[3]))
}

if (require(locfit)) {
  data(ethanol, package="locfit")
  fit <- locfit(NOx~lp(E, nn=0.5), data=ethanol)
  visreg(fit, "E")
  fit <- locfit(NOx~lp(E,C,nn=0.5,scale=0), data=ethanol)
  visreg(fit, "E", by="C")
}

if (require(splines)) {
  fit <- lm(Ozone ~ Solar.R + bs(Wind, 4) + ns(Temp, 4), airquality)
  visreg(fit, "Wind")
  visreg(fit, "Temp")
}

if (require(survival)) {
  fit <- coxph(Surv(time, status) ~ ph.ecog + pspline(age,4), cancer)
  visreg(fit, "age")
  visreg(fit, "age", type="contrast")
}

if (require(mgcv)) {
  dat <- gamSim(2,n=200,dist="normal",scale=0.1)$data
  fit <- gam(y~s(x)+s(z), data=dat)
  visreg(fit, "x")
  fit <- gam(y~s(x, z),data=dat)
  visreg(fit, "x")
  visreg(fit, "z")
  fit <- gam(y~te(x,z,k=7),data=dat, method="REML")
  visreg(fit, "x", by="z")
  fit <- gam(y~ti(x,z,k=7), data=dat, method="REML")
  visreg(fit, "x", by="z")
  visreg2d(fit, "x", "z")
}
