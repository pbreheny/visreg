suppressPackageStartupMessages(library(nnet))

airquality$Heat <- cut(airquality$Temp,3,labels=c("Cool","Mild","Hot"))
capture.output(fit <- multinom(Heat ~ Wind + Ozone, airquality)) |> invisible()
visreg(fit, "Ozone", collapse=TRUE, overlay=TRUE, ylab="Probability", ylim=c(0,1), partial=FALSE, rug=2)
v <- visreg(fit, "Ozone", plot=FALSE)
plot(v[[2]], ylab="Probability", ylim=c(0,1), partial=FALSE, rug=2)
