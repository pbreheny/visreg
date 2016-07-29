if(require(e1071)) {
  fit <- svm(Ozone ~ ., airquality)
  visreg(fit, "Temp")

  fit <- svm(Species ~ ., data = iris, probability=TRUE)
  visreg(fit, "Petal.Length", collapse=TRUE, partial=FALSE, overlay=TRUE)
}
