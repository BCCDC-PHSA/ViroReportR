.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "\n-- ViroReportR -----------------------------------------------\n",
    "Run `pkgdown::build_site(lazy = TRUE)` to access documentation\n",
    "--------------------------------------------------------------"
  )
}