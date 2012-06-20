require(survival)
data(ovarian)
ovarian$rx <- factor(ovarian$rx)

fit <- coxph(Surv(futime,fustat)~age+rx,data=ovarian)

visreg(fit,"age")
visreg(fit,"rx")
visreg(fit,"age",trans=exp)
visreg(fit,"age",trans=exp,ylim=c(0,20))

fit <- coxph(Surv(futime,fustat)~age*rx,data=ovarian)

par(mfrow=c(1,2))
visreg(fit,"age",cond=list(rx="1"))
visreg(fit,"age",cond=list(rx="2"))
visreg2d(fit,x="age",y="rx")
