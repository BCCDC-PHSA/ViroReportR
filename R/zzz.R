.onAttach <- function(libname, pkgname) {
  cli::cli_h1("ViroReportR")
  cli::cli_text("Please run {.code pkgdown::build_site(lazy = TRUE)} in your console")
  cli::cli_text("to access documentation on the package website")
  cli::cli_rule()
}
