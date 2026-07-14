.onLoad <- function(libname, pkgname) {
  register_s3_method("rgl", "persp3d", "visreg2d", persp3d.visreg2d)
  register_s3_method("rgl", "persp3d", "visreg_list", persp3d.visreg_list)
}

# Register an S3 method for suggests-only package without forcing it to load. In
# particular, this is an issue with rgl. If rgl is already loaded, register now;
# if not, arrange for registration to happen if/when the user loads rgl
# themselves.
register_s3_method <- function(pkg, generic, class, fun) {
  if (pkg %in% loadedNamespaces()) {
    registerS3method(generic, class, fun, envir = asNamespace(pkg))
  }
  setHook(
    packageEvent(pkg, "onLoad"),
    function(...) registerS3method(generic, class, fun, envir = asNamespace(pkg))
  )
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "visreg 3.0 includes breaking changes. For migration details, see:\n",
    "https://pbreheny.github.io/visreg/articles/migrating-to-3-0.html"
  )
}
