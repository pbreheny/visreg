setupF <- function(fit, xvar, call.env) {
  if (isS4(fit)) {
    CALL <- fit@call
    ENV <- NULL
  } else {
    CALL <- fit$call
    ENV <- environment(fit$terms)
  }
  if ("data" %in% names(fit) && is.data.frame(fit$data)) {
    Data <- fit$data
    env <- NULL
  } else if (is.null(CALL$data)) {
    env <- NULL
    Data <- NULL
  } else if (exists(as.character(CALL$data), call.env)) {
    env <- call.env
    Data <- eval(CALL$data, envir=env)
  } else if (exists(as.character(CALL$data), ENV)) {
    Data <- eval(CALL$data, envir=ENV)
  } else {
    stop("visreg cannot find the data set used to fit your model; try attaching it to the fit with fit$data <- myData")
  }
  form <- formula(fit)
  av <- get_all_vars(form, Data)    # https://bugs.r-project.org/bugzilla3/show_bug.cgi?id=14905
  #av <- av[,!is.na(names(av))]     # Breaks mlm if dimnames(Y) not set
  if ("mlm" %in% class(fit) && is.null(colnames(coef(fit)))) {
    lhs <- fit$terms[[2L]]
    ny <- ncol(coef(fit))
    if (mode(lhs) == "call" && lhs[[1L]] == "cbind") {
      ynames <- as.character(lhs)[-1L]
    } else {
      ynames <- paste0("Y", seq_len(ny))
    }
    names(av) <- c(ynames, utils::head(names(av)[-1], n=ncol(av) - length(ynames)))
  }
  f <- as.data.frame(av)

  if (class(CALL$random)=="call") {
    rf <- as.data.frame(as.list(get_all_vars(CALL$random, Data)))
    rf <- rf[,setdiff(names(rf), names(f)),drop=FALSE]
    f <- cbind(f, rf)
  }
  if ("subset" %in% names(CALL)) {
    s <- CALL$subset
    subset <- eval(substitute(s), Data, env)
    f <- f[which(subset==TRUE),,drop=FALSE]
  }
  suppressWarnings(f <- f[!apply(is.na(f), 1, any),,drop=FALSE])

  ## Handle some variable type issues
  needsUpdate <- FALSE
  f <- droplevels(f)
  frameClasses <- sapply(f, class)
  if (any(frameClasses=="character")) needsUpdate <- TRUE
  if (any(frameClasses=="Surv")) needsUpdate <- TRUE
  if (any(frameClasses=="logical")) {
    needsUpdate <- TRUE
    for (j in 1:ncol(f)) if (class(f[,j])[1]=="logical") f[,j] <- as.numeric(f[,j])
  }
  if (missing(xvar)) {
    all_x <- strsplit(parseFormula(formula(fit)[3]), ' + ', fixed=TRUE)[[1]]
    inModel <- sapply(names(f), function(x) x %in% all_x)
    const <- sapply(f, function(x) all(x==x[1]))
    xvar <- names(f)[!const & inModel]
  }
  if (length(xvar)==0) stop("The model has no predictors; visreg has nothing to plot.", call.=FALSE)
  for (i in 1:length(xvar)){if (!is.element(xvar[i],names(f))) stop(paste(xvar[i], "not in model"))}

  attr(f, "needsUpdate") <- needsUpdate
  attr(f, "xvar") <- xvar
  f
}
