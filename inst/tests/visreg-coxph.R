# Setup
require(survival)
data(ovarian)
ovarian$rx <- factor(ovarian$rx)

# Basic
fit <- coxph(Surv(futime,fustat)~age+rx,data=ovarian)
visreg(fit,"age")
visreg(fit,"age", type="contrast")
visreg(fit,"rx")
visreg(fit, "age", trans=exp)
visreg(fit,"age",trans=exp,ylim=c(0,20))

# Interaction
fit <- coxph(Surv(futime,fustat)~age*rx,data=ovarian)
par(mfrow=c(1,2))
visreg(fit, "age", cond=list(rx="1"))
visreg(fit, "age", cond=list(rx="2"))
visreg(fit,"age",by="rx")
visreg2d(fit, x="age", y="rx")

# Splines
require(splines)
fit <- coxph(Surv(futime,fustat)~ns(age,4)+rx,data=ovarian)
par(mfrow=c(1,1))
visreg(fit, "age")
visreg(fit, "rx")
visreg(fit, "age", type="contrast")
visreg(fit, "rx", type="contrast")

# Strata
ovarian$Group <- factor(ovarian$rx)
fit <- coxph(Surv(futime,fustat) ~ age+strata(Group), data=ovarian)
visreg(fit, "age")
visreg(fit, "age", type='contrast')
