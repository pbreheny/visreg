.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "visreg 3.0 is a breaking release: base R/lattice plotting has been ",
    "removed (visreg is now ggplot2 only), and a few arguments were renamed.\n",
    "See https://pbreheny.github.io/visreg/articles/migrating-to-3-0.html ",
    "for details, or install the last 2.x release with ",
    "remotes::install_version(\"visreg\", version = \"2.8.1\") to opt out."
  )
}
