.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "visreg 3.0 includes breaking changes. For migration details, see:\n",
    "https://pbreheny.github.io/visreg/articles/migrating-to-3-0.html"
  )
}
