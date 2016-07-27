if (require(ggplot2)) {
  airquality$Heat <- cut(airquality$Temp, 3, labels=c("Cool","Mild","Hot"))
  fit <- lm(Ozone ~ Wind*Heat, data=airquality)
  visreg(fit, "Wind", by="Heat", gg=TRUE)
  visreg(fit, "Wind", by="Heat", gg=TRUE, strip.names=TRUE)
  visreg(fit, "Wind", by="Heat", gg=TRUE, type="contrast")
  visreg(fit, "Wind", by="Heat", gg=TRUE, partial=FALSE)
  visreg(fit, "Wind", by="Heat", gg=TRUE, band=FALSE)
  visreg(fit, "Wind", by="Heat", gg=TRUE, partial=FALSE, band=FALSE)

  visreg(fit, "Heat", by="Wind", gg=TRUE)
  visreg(fit, "Heat", by="Wind", gg=TRUE, partial=FALSE)

  visreg(fit, "Wind", by="Heat", gg=TRUE, xlab="XXX", ylab="YYY")
  visreg(fit, "Wind", by="Heat", gg=TRUE, line=list(col="blue", size=5), points=list(col="red", size=3), fill=list(fill="yellow", col="green"))
  visreg(fit, "Heat", by="Wind", gg=TRUE, xlab="XXX", ylab="YYY")
  visreg(fit, "Heat", by="Wind", gg=TRUE, line=list(col="blue", size=5), points=list(col="red", size=3), fill=list(fill="yellow", col="green"))
}
