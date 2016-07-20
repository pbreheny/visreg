if(require(nnet)) {
  airquality$Heat <- cut(airquality$Temp,3,labels=c("Cool","Mild","Hot"))
  fit <- multinom(Heat ~ Wind + Ozone, airquality)
  visreg(fit, "Ozone", collapse=TRUE, overlay=TRUE, ylab="Probability", ylim=c(0,1), partial=FALSE, rug=2)
}
