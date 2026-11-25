# Copied from ramb 2026.11.24 (b777ac4), R/utils.R, by data-raw/copy_from_ramb.R: do not edit here.
# The text is ramb's, with the names changed (rb_ to os_); a change is made in ramb and the copy made again.

# The maintainer's first rule (2026-10-27): a function that takes a table returns the construct it was given (a
# data.frame, tibble, data.table or sf object, grouped as it came), with the rows in the order they came in.
# `out` is the result made from `x`. `rid`, if given, names a column of `out` holding each row's position in `x`
# (.os_with_rid()): the rows are put back in that order, an sf `x`'s geometry is put back on them, and the column
# is dropped. A lazy `x` passes `out` through, since SQL has no row order; a lazy `out` from a data frame is
# collected. A table of its own made from sf (no `rid`, no geometry) comes back as the sf object's data frame or
# tibble.
.os_as_given <- function(out, x, rid = NULL) {
  if (inherits(x, "tbl_lazy")) {
    return(out)
  }
  if (inherits(out, "tbl_lazy")) {
    out <- dplyr::collect(out)
  }
  geom <- if (inherits(out, "sf")) {
    attr(out, "sf_column")
  } else {
    attr(x, "sf_column")
  }
  out <- tibble::as_tibble(dplyr::ungroup(out))
  if (!is.null(rid)) {
    if (!rid %in% names(out)) {
      stop("internal: the row id was lost", call. = FALSE)
    }
    out <- out[order(out[[rid]]), ]
    if (inherits(x, "sf") && !inherits(out[[geom]], "sfc")) {
      out[[geom]] <- sf::st_geometry(x)[out[[rid]]]
    }
    out[[rid]] <- NULL
  }
  if (inherits(x, "grouped_df") && all(dplyr::group_vars(x) %in% names(out))) {
    return(dplyr::group_by(
      out,
      dplyr::across(dplyr::all_of(dplyr::group_vars(x)))
    ))
  }
  if (inherits(x, "data.table")) {
    # by reference: `out` is the function's own, so no copy is made
    data.table::setDT(out)
  } else if (!inherits(x, "tbl_df")) {
    out <- as.data.frame(out)
  }
  if (!is.null(geom) && inherits(out[[geom]], "sfc")) {
    out <- sf::st_as_sf(out, sf_column_name = geom)
  }
  out
}

# A data frame as a function hands it to DuckDB: its rows numbered in `.rb_rid` for .os_as_given(), a plain data
# frame (a data.table is copied, never changed by reference), and an sf object without its geometry, which
# .os_as_given() puts back.
.os_with_rid <- function(x) {
  if (inherits(x, "sf")) {
    x <- sf::st_drop_geometry(x)
  }
  x <- as.data.frame(x)
  x$.rb_rid <- seq_len(nrow(x))
  x
}

