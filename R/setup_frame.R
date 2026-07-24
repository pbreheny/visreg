setup_frame <- function(fit, xvar, call_env, data) {
  fit_call <- if (isS4(fit)) fit@call else fit$call

  # Locate the data used to fit the model
  located <- locate_source_data(fit, fit_call, call_env, data)
  source_data <- located$data
  data_env <- located$env

  # Build the model frame
  if (inherits(fit, "glmmTMB")) {
    form <- fit$modelInfo$allForm$combForm
  } else {
    form <- formula(fit)
  }
  if (!is.null(source_data)) {
    names(source_data) <- gsub("offset\\((.*)\\)", "\\1", names(source_data))
  }
  if (inherits(fit, "mlm") && fit$terms[[2L]] != "call") {
    # mlm with a matrix response: strip the response so get_all_vars() can
    # find the predictors outside of the usual data-frame-column framework
    rhs_formula <- form
    rhs_formula[[2]] <- NULL
    raw_vars <- get_all_vars(rhs_formula, source_data)
  } else {
    raw_vars <- get_all_vars(form, source_data) # https://bugs.r-project.org/bugzilla3/show_bug.cgi?id=14905
  }
  model_frame <- as.data.frame(raw_vars)

  # nlme-style models specify random effects in a separate formula; pull in
  # any variables referenced there that aren't already in the model frame
  if (inherits(fit_call$random, "call")) {
    random_vars <- as.data.frame(as.list(get_all_vars(fit_call$random, source_data)))
    random_vars <- random_vars[, setdiff(names(random_vars), names(model_frame)), drop = FALSE]
    if (nrow(random_vars) > 0) model_frame <- cbind(model_frame, random_vars)
  }

  # Capture variable labels (e.g. from Hmisc::label() or labelled::var_label())
  # now, since row subsetting below drops arbitrary column attributes
  var_labels <- vapply(model_frame, function(col) {
    lab <- attr(col, "label")
    if (is.null(lab) || length(lab) != 1 || !is.character(lab)) NA_character_ else lab
  }, character(1))

  # Restrict to rows the original model actually used
  if ("subset" %in% names(fit_call) && !inherits(fit, "averaging")) {
    keep <- eval(fit_call$subset, source_data, data_env)
    model_frame <- model_frame[which(keep), , drop = FALSE]
  }
  suppressWarnings(model_frame <- model_frame[!apply(is.na(model_frame), 1, any), , drop = FALSE])

  # Coerce variable types that predict()/update()
  needs_update <- FALSE
  model_frame <- droplevels(model_frame)
  frame_classes <- sapply(model_frame, class)
  if (any(frame_classes == "Surv")) {
    needs_update <- TRUE
  }
  if (any(frame_classes == "character")) {
    needs_update <- TRUE
    for (j in seq_len(ncol(model_frame))) {
      if (typeof(model_frame[, j]) == "character") model_frame[, j] <- factor(model_frame[, j])
    }
  }
  if (any(frame_classes == "logical")) {
    needs_update <- TRUE
    for (j in seq_len(ncol(model_frame))) {
      if (typeof(model_frame[, j]) == "logical") model_frame[, j] <- as.double(model_frame[, j])
    }
  }
  # When needs_update is TRUE, update() will be called with model_frame as
  # the data. If the original call referenced weight/offset columns by name
  # that aren't in the formula (so not in model_frame), add them now so
  # update() can find them.
  if (needs_update && !is.null(source_data)) {
    for (extra_arg in c("weights", "offset")) {
      extra_call <- fit_call[[extra_arg]]
      if (!is.null(extra_call)) {
        extra_name <- tryCatch(as.character(extra_call), error = function(e) character(0))
        if (
          length(extra_name) == 1 &&
            extra_name %in% names(source_data) &&
            !extra_name %in% names(model_frame)
        ) {
          model_frame[[extra_name]] <- source_data[rownames(model_frame), extra_name]
        }
      }
    }
  }

  # Determine/validate xvar
  if (missing(xvar)) {
    formula_terms <- strsplit(parse_formula(formula(fit)[3]), " + ", fixed = TRUE)[[1]]
    in_model <- sapply(names(model_frame), function(x) x %in% formula_terms)
    is_constant <- sapply(model_frame, function(x) all(x == x[1]))
    xvar <- names(model_frame)[!is_constant & in_model]
  }
  if (length(xvar) == 0) {
    stop("The model has no predictors; visreg has nothing to plot.", call. = FALSE)
  }
  for (i in seq_along(xvar)) {
    if (!is.element(xvar[i], names(model_frame))) {
      stop(paste(xvar[i], "not in model"), call. = FALSE)
    }
  }

  attr(model_frame, "needs_update") <- needs_update
  attr(model_frame, "xvar") <- xvar
  attr(model_frame, "var_labels") <- var_labels
  model_frame
}
