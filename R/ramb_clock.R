# Copied from ramb 2026.11.24 (b777ac4), R/clock.R, by data-raw/copy_from_ramb.R: do not edit here.
# The text is ramb's, with the names changed (rb_ to os_); a change is made in ramb and the copy made again.

# A data frame is numbered for .os_as_given(); either is put on `con` and its columns renamed to the grammar's.
.os_clock_in <- function(x, map, con, call = rlang::caller_env()) {
  twice <- map[duplicated(map) | duplicated(map, fromLast = TRUE)]
  if (length(twice)) {
    rlang::abort(
      c(
        sprintf(
          "%s all name the column `%s`.",
          paste0("`", names(twice), "`", collapse = " and "),
          twice[1]
        ),
        i = "Give each its own column (a copy, if one column must serve both)."
      ),
      call = call
    )
  }
  if (!inherits(x, "tbl_lazy")) {
    x <- .os_with_rid(x)
  }
  .os_as_lazy(.os_std_in(x, map), con)
}

.os_warn_unharmonised <- function(x, fn) {
  cn <- colnames(x)
  if (!"stream" %in% cn || "lag_basis" %in% cn) {
    return(invisible(x))
  }
  n <- if (inherits(x, "tbl_lazy")) {
    dplyr::pull(dplyr::summarise(x, n = dplyr::n_distinct(.data$stream)), "n")
  } else {
    length(unique(stats::na.omit(x$stream)))
  }
  if (n >= 2) {
    rlang::warn(
      c(
        sprintf(
          "`x` holds %d streams whose clocks are not harmonized, and %s() compares pings across them.",
          n,
          fn
        ),
        i = "Harmonize them first (step 0): ramb::rb_create_ping_lags(), ramb::rb_add_ping_own_lag(), ramb::rb_add_ping_time(); a lag between two clocks reads as distance.",
        if (fn == "os_flag_ping_impossible") c(i = "Or judge each stream apart: os_flag_ping_impossible(stream = stream).")
      ),
      class = "ramb_unharmonised"
    )
  }
  invisible(x)
}

