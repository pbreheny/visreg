removeFormulaFormatting <- function(form) {
  form <- as.character(form)
  f <- if (grepl("\\+",form)) unlist(strsplit(form, "\\+")) else form
  n.f <- length(f)
  for (i in 1:n.f) {
    f[i] <- gsub(" ", "", f[i])
    f[i] <- gsub("\\blp\\(([^,]+).*\\)", "\\1", f[i])
    f[i] <- gsub("\\bs\\(([^,]+).*\\)", "\\1", f[i])
    f[i] <- gsub("\\te\\(([^,]+).*\\)", "\\1", f[i])
    f[i] <- gsub("\\bbs\\(([^,]+).*\\)", "\\1", f[i])
    f[i] <- gsub("\\bns\\(([^,]+).*\\)", "\\1", f[i])
    f[i] <- gsub("\\bpspline\\(([^,]+).*\\)", "\\1", f[i])
  }
  val <- paste(f,collapse=" + ")
  val <- gsub("()","",val)
  val
}
