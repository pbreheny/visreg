if (requireNamespace("tinytest", quietly=TRUE)) {
  if (length(unclass(packageVersion("visreg"))[[1]]) == 4 | Sys.getenv('R_FORCE_TEST') == 'TRUE') {
    tinytest::test_package("visreg", pattern="^[^_].*\\.[rR]$")
  }
}
