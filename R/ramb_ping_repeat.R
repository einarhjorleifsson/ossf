# Copied from ramb 2026.11.24 (b777ac4), R/ping_repeat.R, by data-raw/copy_from_ramb.R: do not edit here.
# The text is ramb's, with the names changed (rb_ to os_); a change is made in ramb and the copy made again.

#' Flag a ping that repeats another ping of its vessel at another time
#'
#' A device that writes a stretch of its track a second time, or a server that stores it again, leaves pings with
#' the same values as earlier ones at another time. Each ping gets the time to the nearest other ping of its vessel
#' with the same values in `cols`: positive where that ping lies ahead, negative where it lies behind, `NA` where there
#' is none. A run of pings that all carry the same shift is a stretch written twice, and the shift says by how much.
#'
#' A label, not a removal: which of two pings is the copy is not in the values, so both carry the shift (the original
#' with the sign of the copy's direction, the copy with the other) and nothing is set aside; `ping_flag` is untouched.
#' A device lying still writes the same values minute after minute, so a ping that repeats the ping before it in
#' `cols`, in its vessel's time order, is no repeat: it is neither labelled nor matched. No threshold of speed,
#' distance or time is used. Its column is not [os_flag_ping_duplicate()]'s `ping_repeat_dt_s`, so neither overwrites
#' the other.
#'
#' A repeat is only as telling as `cols` makes it: with the position alone, a vessel that returns to a mooring repeats
#' its earlier visit. A counter the device keeps (an odometer, engine hours) cannot repeat on a vessel that moved, so
#' include it where the data have one.
#'
#' @param x A data frame of pings as the device wrote them, one vessel or many. Data frames only: collect a lazy
#'   table first.
#' @param cols <[`tidy-select`][dplyr::dplyr_tidy_select]> The columns a repeat must share. `NULL`, the default,
#'   takes every column but the time; leave a row id out, since no two rows share it.
#' @param vid,time The vessel and time columns, if named otherwise.
#'
#' @return `x`, the same type with the same rows in the same order, with `ping_repeat_shift_s` added: seconds to the
#'   nearest other ping of the vessel with the same values in `cols`, positive when it lies ahead and negative when it
#'   lies behind, `NA` where there is none.
#' @family flag pings
#' @export
os_flag_ping_repeat <- function(x, cols = NULL, vid = vid, time = time) {
  vid <- rlang::as_name(rlang::enquo(vid))
  time <- rlang::as_name(rlang::enquo(time))
  .os_check_input(
    x,
    "os_flag_ping_repeat",
    cols = c(vid = vid, time = time),
    types = c(time = "time")
  )
  if (inherits(x, "tbl_lazy")) {
    stop("`x` must be a data frame: collect() a lazy table first.", call. = FALSE)
  }
  cols <- rlang::enquo(cols)
  cols <- if (rlang::quo_is_null(cols)) {
    setdiff(names(x), time)
  } else {
    names(dplyr::select(as.data.frame(x)[0, , drop = FALSE], !!cols))
  }
  if (inherits(x, "sf")) {
    cols <- setdiff(cols, attr(x, "sf_column"))
  }
  # the values a repeat must share, as one text per ping (made outside the data mask, where `vid` is a column)
  key <- do.call(
    paste,
    c(lapply(as.data.frame(x)[union(vid, cols)], as.character), sep = "\u001f")
  )
  x |>
    dplyr::mutate(.row = dplyr::row_number(), .key = !!key) |>
    dplyr::arrange(.data[[vid]], .data[[time]]) |>
    # a device lying still repeats the ping before it: no repeat
    dplyr::mutate(
      .still = .key == dplyr::lag(.key, default = ""),
      .by = dplyr::all_of(vid)
    ) |>
    # the pings with the same values, in time order: the seconds back to the one before and on to the one after
    dplyr::mutate(
      .back = as.numeric(difftime(
        .data[[time]],
        dplyr::lag(.data[[time]]),
        units = "secs"
      )),
      .on = as.numeric(difftime(
        dplyr::lead(.data[[time]]),
        .data[[time]],
        units = "secs"
      )),
      .by = c(.key, .still)
    ) |>
    dplyr::mutate(
      ping_repeat_shift_s = dplyr::case_when(
        .still ~ NA_real_,
        is.na(.back) ~ .on,
        is.na(.on) ~ -.back,
        .on < .back ~ .on,
        TRUE ~ -.back
      )
    ) |>
    dplyr::arrange(.row) |>
    dplyr::select(-c(.row, .key, .still, .back, .on))
}

