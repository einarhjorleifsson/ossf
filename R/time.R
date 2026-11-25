# A clock reading in a local zone, put on UTC with the reading kept. ossf's own, in base R.
#
# Made general from a case study's step prepare, at the maintainer's word (2026-11-25). Drafted with AI assistance
# (Claude), 2026-11-25; not yet checked by a person.

#' Put local clock readings on UTC, the reading kept, the clock changes labelled
#'
#' A device or a file often writes the time as the local clock showed it. This puts each reading on UTC in the given
#' zone, keeps the reading beside it, and labels the two hours a year where a local reading says too little: the hour
#' the clock skips in spring (no such reading should exist) and the hour it repeats in autumn (each reading in it
#' happens twice).
#'
#' A reading in the repeated hour is placed by the order of the rows, vessel by vessel: the rows are taken as written,
#' so the first pass through the hour is summer time and, once the reading steps back within it, the second pass is
#' winter time. Pass the rows in the order they were written. A device that wrote nothing in the first pass has no
#' step back to show, and its readings are placed in summer time: the label says the hour is in doubt. A reading in the skipped hour is placed by the offset
#' before the change (one hour later in UTC than a correct clock would have written it) and labelled.
#'
#' @param x A data frame.
#' @param time The column of clock readings: date-times whose printed value is the reading (as a file's time without
#'   a zone is read, in UTC), or text such as `"2023-03-26 02:30:00"`.
#' @param tz The zone the clock was set to, as R names it (`"Europe/Rome"`).
#' @param by The column of the device or vessel, for the order through a repeated hour, or `NULL`.
#'
#' @return `x`, the same rows in the same order: `time` replaced by the UTC time, `<time>_written` the reading as
#'   given (unchanged, in its own type), and `<time>_clock`: `"ok"`, `"skipped hour"`, `"repeated hour"` or `NA` with no reading. The clock changes
#'   within the span, with the rows written in each changed hour, are the attribute `"clock_changes"`.
#' @export
#' @examples
#' x <- data.frame(vid = 1, time = c("2023-10-29 01:30:00", "2023-10-29 02:30:00", "2023-10-29 02:10:00",
#'                                   "2023-10-29 02:40:00", "2023-10-29 03:10:00"))
#' os_add_time_utc(x, "time", "Europe/Rome", by = "vid")
os_add_time_utc <- function(x, time, tz, by = NULL) {
  if (!is.data.frame(x)) stop("`x` must be a data frame.", call. = FALSE)
  if (length(setdiff(c(time, by), names(x)))) stop("`x` lacks: ", paste(setdiff(c(time, by), names(x)), collapse = ", "), call. = FALSE)
  if (!is.character(tz) || length(tz) != 1 || !tz %in% OlsonNames()) stop("`tz` must be one zone R knows (OlsonNames()).", call. = FALSE)
  added <- paste0(time, c("_written", "_clock"))
  if (length(intersect(added, names(x)))) stop("`x` already has: ", paste(intersect(added, names(x)), collapse = ", "), call. = FALSE)
  w <- x[[time]]
  reading <- if (inherits(w, "POSIXt")) format(w, "%Y-%m-%d %H:%M:%OS6", tz = attr(as.POSIXlt(w), "tzone")[1]) else as.character(w)
  # the reading as if on UTC: seconds of the clock face, from which each offset is taken away
  face <- as.numeric(as.POSIXct(reading, tz = "UTC", tryFormats = c("%Y-%m-%d %H:%M:%OS", "%Y-%m-%dT%H:%M:%OS", "%Y-%m-%d %H:%M", "%Y-%m-%d")))
  n <- length(face)
  ok <- !is.na(face)
  utc <- rep(NA_real_, n)
  clock <- ifelse(ok, "ok", NA_character_)
  changes <- NULL
  if (any(ok)) {
    # the zone's offset on every hour of the span (and a day either side), and where it changes
    h <- seq(floor(min(face[ok]) / 86400) * 86400 - 2 * 86400, ceiling(max(face[ok]) / 86400) * 86400 + 2 * 86400, by = 3600)
    off <- as.POSIXlt(as.POSIXct(h, origin = "1970-01-01", tz = "UTC"), tz = tz)$gmtoff
    # a reading's UTC is its face less the offset in force at that UTC: found in two steps, exact outside the
    # changed hours, which are placed below
    off_at <- function(t) stats::approx(h, off, xout = t, method = "constant", rule = 2, f = 0)$y
    utc[ok] <- face[ok] - off_at(face[ok] - off_at(face[ok]))
    chg <- which(diff(off) != 0)
    if (length(chg)) {
      t_chg <- h[chg + 1]                                       # the change, in UTC
      lo <- pmin(off[chg], off[chg + 1])
      hi <- pmax(off[chg], off[chg + 1])
      start <- t_chg + lo                                       # the changed hour's first reading on the clock face
      spring <- off[chg + 1] > off[chg]
      grp <- if (is.null(by)) rep(1L, n) else x[[by]]
      rows <- integer(length(chg))
      for (k in seq_along(chg)) {
        inside <- ok & face >= start[k] & face < start[k] + (hi[k] - lo[k])
        rows[k] <- sum(inside)
        if (spring[k]) {
          clock[inside] <- "skipped hour"
          utc[inside] <- face[inside] - off[chg[k]]
        } else {
          clock[inside] <- "repeated hour"
          # by device, in the rows' order: summer time until the reading steps back within the hour, winter after
          for (i in split(which(inside), grp[inside])) {
            back <- c(FALSE, diff(face[i]) < 0)
            utc[i] <- face[i] - ifelse(cumsum(back) > 0, lo[k], hi[k])
          }
        }
      }
      changes <- data.frame(change_utc = as.POSIXct(t_chg, origin = "1970-01-01", tz = "UTC"),
                            kind = ifelse(spring, "spring: an hour skipped", "autumn: an hour repeated"),
                            hour_written = format(as.POSIXct(start, origin = "1970-01-01", tz = "UTC"), "%Y-%m-%d %H:%M"),
                            rows_written = rows)
      changes <- changes[changes$change_utc >= min(utc[ok]) - 86400 & changes$change_utc <= max(utc[ok]) + 86400, , drop = FALSE]
    }
  }
  x[[added[1]]] <- w
  x[[time]] <- as.POSIXct(utc, origin = "1970-01-01", tz = "UTC")
  x[[added[2]]] <- clock
  attr(x, "clock_changes") <- changes
  x
}
