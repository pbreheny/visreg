removeFormulaFormatting <- function(form)
{
  form <- as.character(form)
  f <- if (grepl("\\+",form)) unlist(strsplit(form, "\\+")) else form
  n.f <- length(f)
  for (i in 1:n.f) {
    f[i] <- gsub(" ", "", f[i])
    f[i] <- gsub("\\blp\\(([[:alnum:]]+).*\\)", "\\1", f[i])
    f[i] <- gsub("\\bs\\(([[:alnum:]]+).*\\)", "\\1", f[i])
    f[i] <- gsub("\\bbs\\(([[:alnum:]]+).*\\)", "\\1", f[i])
    f[i] <- gsub("\\bns\\(([[:alnum:]]+).*\\)", "\\1", f[i])
  }
  val <- paste(f,collapse=" + ")
  val <- gsub("()","",val)
  val
}
