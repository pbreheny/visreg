removeFormulaFormatting <- function(form)
  {
    form <- as.character(form)
    if (grepl("\\+",form)) f <- unlist(strsplit(form, "\\+"))
    else f <- form
    n.f <- length(f)
    for (i in 1:n.f)
      {
        f[i] <- gsub("lp\\(([[:alnum:]]+).*\\)", "\\1", f[i])
        f[i] <- gsub("s\\(([[:alnum:]]+).*\\)", "\\1", f[i])
      }
    val <- paste(f,collapse=" + ")
    val <- gsub("()","",val)
    return(val)
  }
