visregList <- function(..., labels, collapse=TRUE) {
  out <- structure(list(...), class="visregList")
  if (collapse) out <- collapse.visregList(out, labels=labels)
  out
}
