setup_data <- function(fit, f, name, nn, cond, whitespace, ...) {
  # Set up n-row data frame for residuals
  x_res <- f[, name]
  df_res <- data.frame(x_res)
  names(df_res) <- name
  df <- fill_frame(f, df_res, cond)

  rhs_form <- formula(fit)
  rhs_form[2] <- NULL
  simple_rhs <- paste(all.vars(rhs_form), collapse = " + ")
  simple_form <- as.formula(paste("~", simple_rhs))
  simple_res <- model.frame(simple_form, df)
  cond_names <- setdiff(names(simple_res), name)
  cond_names <- intersect(cond_names, names(df))
  frame_res <- cbind(simple_res, df[, setdiff(names(df), names(simple_res)), drop = FALSE])

  ## Set up nn-row data frame for prediction
  dots <- list(...)
  if (is.factor(x_res)) {
    x_fit <- factor(levels(x_res), levels = levels(x_res))
  } else {
    x_fit <- c(seq(min(x_res), max(x_res), length = nn))
  }
  df_fit <- data.frame(x_fit)
  names(df_fit) <- name
  df <- fill_frame(f, df_fit, cond)
  simple_fit <- model.frame(simple_form, df)
  frame_fit <- cbind(simple_fit, df[, setdiff(names(df), names(simple_fit)), drop = FALSE])

  list(
    x_res = x_res,
    x_fit = x_fit,
    frame_res = frame_res,
    frame_fit = frame_fit,
    factor = is.factor(x_res),
    name = name,
    cond = frame_res[1, cond_names, drop = FALSE]
  )
}
