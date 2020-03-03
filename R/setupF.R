setupF <- function(fit, xvar, call.env, data) {
  CALL <- if (isS4(fit)) fit@call else fit$call
  if (!is.null(data)) {
    Data <- data
  } else if (!is.null(CALL) && ('data' %in% names(CALL)) && exists(as.character(CALL$data), call.env)) {
    env <- call.env
    Data <- eval(CALL$data, envir=env)
  } else if (isS4(fit)) {
    FRAME <- try(fit@frame, silent=TRUE)
    DATA <- try(fit@data, silent=TRUE)
    if (!inherits(DATA, 'try-error')) {
      Data <- DATA
    } else if (!inherits(FRAME, 'try-error')) {
      Data <- FRAME
    } else {
      stop("visreg cannot find the data set used to fit your model; supply it using the 'data=' option")
    }
  } else {
    ENV <- environment(fit$terms)
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
      env <- ENV
      Data <- eval(CALL$data, envir=ENV)
    } else {
      stop("visreg cannot find the data set used to fit your model; supply it using the 'data=' option")
    }
  }
  form <- formula(fit)
  if (!is.null(Data)) names(Data) <- gsub('offset\\((.*)\\)', '\\1', names(Data))
  if (inherits(fit, 'mlm') && fit$terms[[2L]] != 'call') {
    ff <- form
    ff[[2]] <- NULL
    av <- get_all_vars(ff, Data)      # If mlm with matrix as Y, outside of data frame framework
  } else {
    av <- get_all_vars(form, Data)    # https://bugs.r-project.org/bugzilla3/show_bug.cgi?id=14905
  }
  f <- as.data.frame(av)

  if (class(CALL$random)=="call") {
    rf <- as.data.frame(as.list(get_all_vars(CALL$random, Data)))
    rf <- rf[,setdiff(names(rf), names(f)),drop=FALSE]
    f <- cbind(f, rf)
  }
  if ("subset" %in% names(CALL) & !('averaging' %in% class(fit))) {
    s <- CALL$subset
    subset <- eval(substitute(s), Data, env)
    f <- f[which(subset==TRUE),,drop=FALSE]
  }
  suppressWarnings(f <- f[!apply(is.na(f), 1, any),,drop=FALSE])

  # Handle some variable type issues
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
