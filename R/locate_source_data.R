# Find the data frame a model was originally fit on, plus the environment in
# which a subset= expression from the original call should be evaluated.
#
# Returns list(data = <data.frame or NULL>, env = <environment or NULL>).
locate_source_data <- function(fit, fit_call, call_env, data) {
  if (!is.null(data)) {
    return(list(data = data, env = call_env))
  }
  if (
    !is.null(fit_call) &&
      "data" %in% names(fit_call) &&
      (exists(tail(as.character(fit_call$data), 1), call_env) ||
        head(as.character(fit_call$data), 1) == "::")
  ) {
    return(list(data = eval(fit_call$data, envir = call_env), env = call_env))
  }
  if (isS4(fit)) {
    # Not every S4 model class stores the full source data; some keep only
    # the model frame. Prefer @data (the fuller object) when both exist. A
    # missing slot is an expected, recoverable condition here, so probing
    # with try() and falling through is appropriate.
    s4_data <- try(fit@data, silent = TRUE)
    if (!inherits(s4_data, "try-error")) {
      return(list(data = s4_data, env = call_env))
    }
    s4_frame <- try(fit@frame, silent = TRUE)
    if (!inherits(s4_frame, "try-error")) {
      return(list(data = s4_frame, env = call_env))
    }
    stop(
      "visreg cannot find the data set used to fit your model; supply it using the 'data=' option",
      call. = FALSE
    )
  }
  terms_env <- environment(fit$terms)
  if ("data" %in% names(fit) && is.data.frame(fit$data)) {
    return(list(data = fit$data, env = NULL))
  }
  if (is.null(fit_call$data)) {
    return(list(data = NULL, env = NULL))
  }
  if (exists(as.character(fit_call$data), terms_env)) {
    return(list(data = eval(fit_call$data, envir = terms_env), env = terms_env))
  }
  stop(
    "visreg cannot find the data set used to fit your model; supply it using the 'data=' option",
    call. = FALSE
  )
}
