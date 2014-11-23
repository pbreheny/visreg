parseFormula <- function(form) {
  form <- gsub("\\|", "\\+", as.character(form))
  #form <- gsub("\\(([^\\|\\)]+)\\|([^\\)]+)\\)", "\\1 + \\2", as.character(form))
  ##form <- gsub("\\(([^\\|\\)]+)\\|([^\\)]+)\\)", "\\1 + \\2", as.character(form))
  f <- if (grepl("\\+",form)) unlist(strsplit(form, "\\+")) else form
  n.f <- length(f)
  for (i in 1:n.f) {
    f[i] <- gsub(" ", "", f[i])
    f[i] <- gsub("\\bbs\\(([^,]+).*\\)", "\\1", f[i])
    f[i] <- gsub("\\bns\\(([^,]+).*\\)", "\\1", f[i])
    f[i] <- gsub("\\bpspline\\(([^,]+).*\\)", "\\1", f[i])
    f[i] <- gsub("\\bpoly\\(([^,]+).*\\)", "\\1", f[i])
    if (substr(f[i], 1, 3) %in% c("te(", "ti(", "lp(") || substr(f[i], 1, 2) %in% c("s(")) {
      f[i] <- gsub(".*\\(([^\\)]+).*\\)", "\\1", f[i])
      fi <- unlist(strsplit(f[i], ","))
      fi <- fi[!grepl("=", fi)]
      f[i] <- paste(fi, collapse=" + ")
    }
  }
  f <- f[f!=""]
  val <- paste(f,collapse=" + ")
  val <- gsub("()","",val)
  val
}
