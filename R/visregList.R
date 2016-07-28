visregList <- function(..., labels, collapse=FALSE) {
  out <- structure(list(...), class="visregList")
  if (collapse) out <- collapse.visregList(out, labels=labels)
  out
}
