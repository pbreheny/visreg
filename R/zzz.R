.onLoad <- function(libname, pkgname) {
  if (requireNamespace("rgl", quietly = TRUE)) {
    registerS3method("persp3d", "visreg2d", persp3d.visreg2d, envir = asNamespace("rgl"))
    registerS3method("persp3d", "visreg_list", persp3d.visreg_list, envir = asNamespace("rgl"))
  }
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "visreg 3.0 includes breaking changes. For migration details, see:\n",
    "https://pbreheny.github.io/visreg/articles/migrating-to-3-0.html"
  )
}
