# The package's own page and what its namespace needs. Drafted with AI assistance (Claude), 2026-11-25.

#' ossf: the steps from a vessel's pings to its fishing
#'
#' Three kinds of function, told apart on each help page:
#'
#' - **ossf's own**, written for it and kept simple, in base R where it can: [os_add_fishing()], [os_add_hauls()],
#'   [os_flag_ping_polygon()], [os_add_trip_value()], [os_add_time_utc()], [os_score()].
#' - **Copies from ramb**, taken as they are with everything they call (`data-raw/copy_from_ramb.R`); only the names
#'   differ (`rb_` is `os_`). They run in DuckDB as ramb's do, and give ramb's results. A copy is never edited here.
#' - **Maps**: [os_map()], the pings after any stage of the flow, drawn by ogis where it is installed.
#'
#' @keywords internal
#' @importFrom Rcpp sourceCpp
#' @importFrom rlang :=
#' @useDynLib ossf, .registration = TRUE
"_PACKAGE"
