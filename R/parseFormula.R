parseFormula <- function(form) {
  form <- gsub("\\|", "\\+", as.character(form))
  form <- gsub("\\*", "\\+", as.character(form))
  f <- if (grepl("\\+",form)) unlist(strsplit(form, "\\+")) else form
  n.f <- length(f)
  for (i in 1:n.f) {
    f[i] <- gsub(" ", "", f[i])
    if (substr(f[i], 1, 3) %in% c("te(", "ti(", "lp(") || substr(f[i], 1, 2) %in% c("s(")) {
      matched <- gregexpr("\\((?>[^()]|(?R))*\\)", f[i], perl = T)
      fi <- substring(f[i], matched[[1]]+1, matched[[1]] + attr(matched[[1]], "match.length")-2)
      fi <- gsub("\\([^\\)]+.*\\)", "", fi)
      fi <- unlist(strsplit(fi, ","))
      fi <- fi[!grepl("=", fi) | grepl("by\\s*=", fi)]
      fi <- gsub("by\\s*=", "", fi)
      f[i] <- paste(fi, collapse=" + ")
    }
    f[i] <- gsub("\\b[^\\(]*\\(([^,]+).*\\)", "\\1", f[i])
  }
  f <- f[f!=""]
  val <- paste(f, collapse=" + ")
  val <- gsub("()","",val)
  val
}
