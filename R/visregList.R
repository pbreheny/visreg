visregList <- function(..., labels, collapse=TRUE) {
  out <- structure(list(...), class="visregList")
  if (collapse) out <- collapse(out, labels=labels)
  out
}
