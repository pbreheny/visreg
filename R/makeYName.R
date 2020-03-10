makeYName <- function(fit, scale, trans, type) {
  if (scale=="response" | (class(fit)[1] %in% c("lm", "aov") & identical(trans, I))) {
    if (type=="contrast") {
      yName <- as.expression(substitute(list(Delta) * x, list(x=as.character(formula(fit)[2]))))
    } else {
      yName <- as.character(formula(fit)[2])
    }
  } else if ("mlm" %in% class(fit)) {
    if (type=="contrast") {
      yName <- sapply(colnames(fit$residuals), function(y) {as.expression(substitute(list(Delta) * x, list(x=y)))})
    } else {
      yName <- colnames(fit$residuals)
    }
  } else if ("randomForest" %in% class(fit)) {
    if (fit$type=="regression") yName <- as.character(formula(fit)[2])
    if (fit$type=="classification") yName <- paste0("Pr(", as.character(formula(fit)[2]), ")")
  } else {
    yName <- NULL
  }
  yName
}
