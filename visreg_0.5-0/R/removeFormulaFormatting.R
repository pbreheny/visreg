removeFormulaFormatting <- function(form)
  {
    if (class(fit)[1]=="gam")
      {
        form <- gsub("s\\((.+),.*\\)","\\1",form)
        form <- gsub("s\\((.+)\\)","\\1",form)
      }
    form <- gsub("()","",form)
    return(form)
  }
