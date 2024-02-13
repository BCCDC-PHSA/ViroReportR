.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "================================================\n",
    "Welcome to vriforecasting! \n",
    "Please run `pkgdown::build_site(lazy = TRUE)` in your console \n",
    "to access documentation on the package website \n",
    "Viral respiratory disease report for the current season can be generated \n",
    "by running generate_forecast_report()"
    "================================================"
    )
}
