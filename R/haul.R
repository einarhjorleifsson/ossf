# Hauls from the fishing calls: events built afterwards, the calls never changed ------------------------------------------
#
# The fifth of the few principles (fishycode's session of 2026-11-25, after the ablation): hauls as events are built
# from the calls afterwards, and never change them. The one rule that joins them is the pause guard designed against a
# hauler sensor that day: a pause is an interruption of the work, so it is no longer than the work on either side of it.
# Its length is the ground the vessel covers. Measured in time instead, the guard joined the strings of a static gear
# across the steaming between them, short in time but long in ground. No value is given.
#
# Drafted with AI assistance (Claude), 2026-11-25; not yet checked by a person.

#' Hauls from a vessel's fishing calls
#'
#' Groups the pings a call marks as fishing into hauls (on a towed gear, tows), without changing the call. A run of
#' consecutive fishing pings is part of a haul. Two runs are one haul when the stretch between them, a pause, is no
#' longer than the shorter of the two runs: a pause is an interruption of the work, where the vessel stays at its
#' gear, so it covers less ground than the work on either side of it. A longer stretch ends the haul, and the next run
#' starts a new one. Nothing else: no shortest haul, no longest gap. Every run of fishing pings is in a haul, a run of
#' one ping included; the hauls table gives each haul's pings, ground and time, so a user may keep what suits the
#' question.
#'
#' A stretch's length is the ground it covers along the track: each ping takes half the distance to the ping before
#' it and half the distance to the ping after it, within its vessel and group (at the group's first and last ping,
#' the one half there is), and a stretch is the sum over its pings. A stop covers no ground, so it stays in its haul
#' however long it lasts; steaming to the next string covers more ground than the string and ends the haul, however
#' short it is. Measured in time, the second would be joined. The rule holds at any ping rate. A ping with no position
#' covers no ground: the distance runs from the ping before it to the ping after it that have one.
#'
#' The call is never changed: `call` comes back as it went in. The whole haul, its pauses included, is the pings
#' with a `haul_id`. A ping whose call is `NA` is no fishing ping; it may lie in a pause.
#'
#' Tested on static gear only (pots, nets and longlines, hauled). On a towed gear, the shooting
#' and hauling back between two tows on the same ground covers less ground than the tows, so the two tows are one haul.
#'
#' @param pings A data frame of pings with `vid`, `time` (`POSIXct`), `lon` and `lat` (decimal degrees) and the
#'   `call` column. Rows may be in any order; pings at the same time keep the order given.
#' @param by The name of a column that groups a vessel's pings (a trip), or `NULL`. A haul never reaches across
#'   groups. A ping with no group (`NA`) is in no haul.
#' @param call The name of a logical column of `pings`, `TRUE` where the ping is fishing: by default `"fishing"`,
#'   from [os_add_fishing()]; any other call (a sensor, a label) works the same way.
#'
#' @return `pings`, rows in the order given, with two columns added: `haul_id` (an integer, numbered by vessel in
#'   time order; `NA` in no haul) and `haul_role` (`"working"`, a fishing ping; `"pause"`, a ping of a pause inside
#'   a haul; `NA` in no haul). The attribute `"hauls"` is a data frame, a row a haul: `vid`, the `by` column,
#'   `haul_id`, `start` and `end` (its first and last ping), `pings`, `working_pings`, `pause_pings`, `pauses` (how
#'   many), `distance_m` and `working_m` (the ground its pings, and its working pings, cover), `duration_s` and
#'   `working_s` (the time they stand for, half the step before and half the step after each ping) and `from` (the
#'   call it was built from).
#' @export
#'
#' @examples
#' # a vessel steams out, hauls a string, steams to the next and hauls it with a stop on the way, and steams home,
#' # a ping a minute
#' set.seed(1)
#' kn <- c(rep(8, 30), rep(1.2, 40), rep(8, 8), rep(1.2, 25), rep(0.1, 10), rep(1.2, 25), rep(8, 30))
#' kn <- kn * exp(rnorm(length(kn), 0, 0.1))
#' pings <- data.frame(
#'   vid = 1,
#'   time = as.POSIXct("2024-01-01", tz = "UTC") + 60 * seq_along(kn),
#'   lon = 13 + cumsum(kn * 1852 / 60) / 80000,
#'   lat = 43.6
#' )
#' # a range with a lower bound, so the stop is a pause: it covers no ground and stays in its haul
#' range <- data.frame(vid = 1, speed_min_kn = 0.5, speed_max_kn = 2.5)
#' out <- os_add_hauls(os_add_fishing(pings, range = range))
#' attr(out, "hauls")
#' table(out$haul_role, useNA = "ifany")
#' # the whole haul as the call
#' out$fishing_haul <- !is.na(out$haul_id)
os_add_hauls <- function(pings, by = NULL, call = "fishing") {
  if (!(is.character(call) && length(call) == 1 && !is.na(call))) stop("`call` must be one column name.", call. = FALSE)
  need <- c("vid", "time", "lon", "lat", call, by)
  miss <- setdiff(need, names(pings))
  if (length(miss)) stop("`pings` lacks: ", paste(miss, collapse = ", "), call. = FALSE)
  if (!inherits(pings$time, "POSIXct")) stop("`pings$time` must be POSIXct.", call. = FALSE)
  if (!is.logical(pings[[call]])) stop("`pings$", call, "` must be logical.", call. = FALSE)
  made <- intersect(c("haul_id", "haul_role"), names(pings))
  if (length(made)) stop("`pings` already has: ", paste(made, collapse = ", "), call. = FALSE)

  n <- nrow(pings)
  t <- as.numeric(pings$time)
  grp <- if (is.null(by)) rep(1L, n) else pings[[by]]
  work <- pings[[call]] %in% TRUE
  judged <- !is.na(pings$vid) & !is.na(t) & !is.na(grp)
  haul <- rep(NA_integer_, n)
  role <- rep(NA_character_, n)
  hauls <- list()

  for (v in unique(pings$vid[judged])) {
    iv <- which(judged & pings$vid == v)
    found <- list()
    for (i in split(iv, grp[iv])) {
      i <- i[order(t[i])]
      h <- .os_hauls_group(t[i], pings$lon[i], pings$lat[i], work[i])
      if (nrow(h$hauls)) found[[length(found) + 1]] <- c(h, list(i = i))
    }
    if (!length(found)) next
    # number the vessel's hauls in time order, across its groups
    start <- unlist(lapply(found, function(f) f$hauls$start))
    rank <- integer(length(start))
    rank[order(start)] <- seq_along(start)
    offset <- 0L
    for (f in found) {
      k <- nrow(f$hauls)
      id <- rank[offset + seq_len(k)]
      haul[f$i] <- id[f$haul]
      role[f$i] <- f$role
      f$hauls$haul <- id
      f$hauls$row <- f$i[1]
      hauls[[length(hauls) + 1]] <- f$hauls
      offset <- offset + k
    }
  }

  tab <- if (length(hauls)) do.call(rbind, hauls) else
    cbind(.os_hauls_group(numeric(0), numeric(0), numeric(0), logical(0))$hauls, row = integer(0))
  tab <- tab[order(pings$vid[tab$row], tab$haul), ]
  tz <- attr(pings$time, "tzone")
  tz <- if (is.null(tz)) "" else tz
  out <- data.frame(vid = pings$vid[tab$row])
  if (!is.null(by)) out[[by]] <- pings[[by]][tab$row]
  out$haul_id <- tab$haul
  out$start <- as.POSIXct(tab$start, origin = "1970-01-01", tz = tz)
  out$end <- as.POSIXct(tab$end, origin = "1970-01-01", tz = tz)
  out <- cbind(out, tab[c("pings", "working_pings", "pause_pings", "pauses", "distance_m", "working_m", "duration_s", "working_s")])
  out$from <- rep(call, nrow(out))
  rownames(out) <- NULL

  pings$haul_id <- haul
  pings$haul_role <- role
  attr(pings, "hauls") <- out
  pings
}

# Each of a group's pings, in time order, takes half the step before it and half the step after it ------------------------
# `step` holds the step from each ping to the next, so one fewer than the pings.
.os_halves <- function(step) {
  if (!length(step)) return(rep(0, length(step) + 1L))
  (c(0, step) + c(step, 0)) / 2
}

# The hauls of one group, its pings in time order ---------------------------------------------------------------------------
# The stretches of working and other pings alternate. A stretch of other pings between two working ones is a pause in
# the haul where the ground it covers is no more than that of the shorter working stretch beside it. One pass over the
# stretches as they are: a join never changes the stretches another pause is held against.
.os_hauls_group <- function(t, lon, lat, work) {
  n <- length(t)
  dt <- if (n) .os_halves(diff(t)) else numeric(0)
  # a ping with no position takes no ground: the step it lies in is shared by the two pings that bound it
  ground <- rep(0, n)
  k <- which(!is.na(lon) & !is.na(lat))
  if (length(k) > 1) {
    a <- k[-length(k)]
    b <- k[-1]
    ground[k] <- .os_halves(.os_dist_m(lon[a], lat[a], lon[b], lat[b]))
  }
  r <- rle(work)
  m <- length(r$values)
  last <- cumsum(r$lengths)
  first <- last - r$lengths + 1L
  sum_over <- function(x) { cx <- c(0, cumsum(x)); cx[last + 1L] - cx[first] }
  len <- sum_over(ground)
  kind <- r$values
  join <- rep(FALSE, m)
  inner <- which(!kind & seq_len(m) > 1L & seq_len(m) < m)
  join[inner] <- len[inner] <= pmin(len[inner - 1L], len[inner + 1L])
  inside <- kind | join
  idx <- cumsum(inside & c(TRUE, !inside[-m]))
  idx[!inside] <- NA_integer_
  haul <- rep(idx, r$lengths)
  role <- ifelse(is.na(haul), NA_character_, ifelse(work, "working", "pause"))
  ih <- which(inside)
  h <- idx[ih]
  sum_by <- function(x) if (length(h)) as.vector(rowsum(x, h, reorder = TRUE)) else x[0]
  secs <- sum_over(dt)
  hauls <- data.frame(
    haul = unique(h),
    start = t[first[ih][!duplicated(h)]],
    end = t[last[ih][!duplicated(h, fromLast = TRUE)]],
    pings = sum_by(r$lengths[ih]),
    working_pings = sum_by(r$lengths[ih] * kind[ih]),
    pause_pings = sum_by(r$lengths[ih] * !kind[ih]),
    pauses = sum_by(as.integer(!kind[ih])),
    distance_m = sum_by(len[ih]),
    working_m = sum_by(len[ih] * kind[ih]),
    duration_s = sum_by(secs[ih]),
    working_s = sum_by(secs[ih] * kind[ih])
  )
  list(haul = haul, role = role, hauls = hauls)
}
