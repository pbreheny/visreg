if(require(loess)) {
  fit <- loess(Ozone ~ Wind, data = airquality)
  visreg(fit, 'Wind')
}
