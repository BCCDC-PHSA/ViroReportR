.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "================================================\n",
    "Welcome to vriforecasting! \n",
    "Please run `pkgdown::build_site(lazy = TRUE)` in your console \n",
    "to access documentation on the package website \n",
    "================================================"
  )
}
