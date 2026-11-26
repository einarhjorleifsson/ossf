# Setting from a vessel's track: the later haul as the setting's witness ---------------------------------------------------
#
# A passive gear is hauled where it was set, so the haul is the witness for the setting, as a hauler sensor is for the
# hauling. The fundamentals are those of the hauling: a speed range from the best source there is (os_add_fishing(),
# here measured from the positions on a later haul's line), events by the ground rule (os_add_hauls()), nothing dropped,
# every value with its basis. Worked out on 2026-11-25 on a small-scale static-gear case study and on the Icelandic
# longliners of 2018 (fishycode's study $FISHYDATA/archive/2026-11-25-setting-iceland-longline/), counts only:
#  - against the logged set start in Iceland (the set times that are not the logbook app's own arithmetic), the set's
#    start within the hour on 78-80 %, iapesca's finder (as ramb recodes it) 64 %, the vessel's usual soak 62 %;
#  - the reach is the gear's scale: 50-100 m on the case study's strings, 500 m and more on the Icelandic lines. The
#    share of hauls whose set is as long as the haul peaks at the reach the outside witnesses favour, in both, so the
#    reach is picked by it;
#  - distances are to a line, not position to position: at pings minutes apart a line passes between two positions.
# Tried and not taken: the closest earlier pass with no reach (an earlier day's pass on the same ground), the valley of
# the distances (no clear valley), a recurrence plot on the ground covered (as good on long lines at a wide reach,
# weaker on short strings); and a clause for the run in to a haul (a setting event on the line of the haul that next
# begins in its group, with no more time before it than the event took, is no set): on the case study it took out two
# thirds of the events of a position or two and raised the agreement with another soak method from 87.7 to 90.1 %, but
# in Iceland, where a line is hauled soon after it is set, it took out the sets themselves (79.4 to 31.2 %). So it is no
# part of the rule: it is the argument `run_in`, off unless asked for (the user, 2026-11-25: "Should we actually have an
# option argument for the function, the run in-rule"; "Yes, add it"). Whether the data can switch it was measured (the
# time from a setting event to its haul: two kinds on the case study, one broad spread in Iceland, where the short ones
# are sets) and is open: a valley is found in both, so it needs a test of whether there are two kinds at all.
#
# Drafted with AI assistance (Claude), 2026-11-25; not yet checked by a person.

#' Setting from a vessel's track, the later haul as its witness
#'
#' Finds where and when a passive gear (lines, nets, pots) was set, from the track and the hauls. A gear is hauled
#' where it was set, so a position that lies on the line of a later haul is a witness for the setting. The steps are
#' those of the hauling:
#' \enumerate{
#'   \item The witness. A position in no haul is marked where it lies within the reach of the line of a haul of the
#'     same vessel that starts after it, no more than `back_s` later. A haul's line is its positions joined in time
#'     order, so at pings minutes apart a line that passes between two positions is still met.
#'   \item The setting speed range, by vessel: [os_add_fishing()] measures it from the marked positions (the speeds
#'     at which a position is more often marked than not). Where it measures none (too few marked positions, or a
#'     vessel that sets at its steaming speed), the marked positions themselves are the call.
#'   \item Setting events: [os_add_hauls()] groups the positions in the range, in no haul, into events by the ground
#'     they cover. An event is kept where half or more of its positions are marked.
#'   \item Each haul is asked what lies under it, position by position: what the vessel last laid there before the
#'     haul began, within the reach and `back_s`: a setting event, or an earlier haul (gear put back as it is hauled
#'     is set by the haul before). The answer is the attribute `"under"`.
#' }
#' The reach is the gear's scale: tens of metres for short strings set close together, a kilometre for long lines.
#' Too narrow, a set is missed or found in part; too wide, the neighbouring gear is taken in. A line is as long
#' hauled as set, so with several values in `reach_m` each vessel takes the one at which most of its hauls have a set
#' between 0.8 and 1.25 of the haul's length (the narrower on a tie). One value is used as it is.
#'
#' Nothing is dropped: every row comes back, in the order given. A haul's own positions are never setting. The hauls
#' may come from any source: [os_add_hauls()] on the track, or a logbook's haul windows. A logged haul window that
#' begins at the set (some do) hides that set, since it then lies inside the haul. The last position or two of the
#' run in to a haul lie on its line as well, and come out as a setting event of their own: `"sets"` gives each
#' event's positions and ground, and `"under"` how much of a haul an event lies under, for the user to hold them by;
#' `run_in = TRUE` takes them out where the gear allows it.
#'
#' Given, not taken from the data, and so named here: `back_s` (ten days); `run_in`; the half of an event's positions for it to
#' be kept; the half of a haul's positions for its outcome; the 0.8 to 1.25 of the length fit; the candidate reaches.
#'
#' @param pings A data frame of pings with `vid`, `time` (`POSIXct`), `lon` and `lat` (decimal degrees) and the
#'   `haul` column. Rows may be in any order.
#' @param by The name of a column that groups a vessel's pings (a trip), or `NULL`. Speeds, events and lines never
#'   reach across groups; a set may lie in an earlier group than its haul. A ping with no group (`NA`) is not judged.
#' @param haul The name of a column that numbers the vessel's hauls, `NA` in no haul: by default `"haul_id"`, from
#'   [os_add_hauls()].
#' @param reach_m The reach in metres: one value, or several for each vessel to take the one where its sets and hauls
#'   are most often of one length. By default 25, 50, 100, 250, 500 and 1,000 m; each value is a pass over the data.
#' @param back_s How long before a haul its set may lie, in seconds.
#' @param run_in Whether the run in to a haul is taken out of the setting: `FALSE` (the default) or `TRUE`. With
#'   `TRUE`, a setting event is the run in, and no set, where half or more of it lies on the line of the haul that
#'   next begins in its group and no more time passes between its end and that haul's start than the event itself
#'   took; its positions are not setting, with the basis `"none: the run in to a haul"`. For gear that soaks long
#'   after it is set and is hauled on another visit (pots, nets): there a pass along the gear just before the haul is
#'   the vessel running in to it. Not for gear hauled soon after it is set (long lines): there such a pass is the set
#'   itself. On the two datasets the function was made on, `TRUE` raised the agreement with an outside soak from 88
#'   to 90 % on pots and nets, and lowered the sets found right from 79 to 31 % on longlines hauled the same trip.
#'   The time between a setting event and its haul shows which a dataset is: nearly all under five minutes on the
#'   first, spread evenly from minutes to hours on the second.
#'
#' @return `pings`, rows in the order given, with four columns added: `on_later_haul` (logical; `NA` in a haul),
#'   `setting` (logical), `setting_basis` (`"measured"`, the vessel's setting speed range measured from the marked
#'   positions; `"on a later haul's line"`, no range measured; or `"none: "` and why) and `set_id` (an integer, the
#'   setting events numbered by vessel in time order; `NA` where not setting). Three attributes:
#'   \describe{
#'     \item{`"ranges"`}{a row a vessel: `reach_m` and how it came (`reach_basis`), the share of hauls whose set fits
#'       the haul's length at it (`length_fit`), the setting speed range and its basis, the hauls and the setting
#'       events.}
#'     \item{`"sets"`}{a row a setting event: `vid`, the `by` column, `set_id`, `start`, `end`, `pings`, the share of
#'       them on a later haul (`on_later_haul`), and `distance_m`.}
#'     \item{`"under"`}{a row a haul: `vid`, the `haul` column, `pings`, the shares of its positions with a setting
#'       event, an earlier haul or nothing last laid under them, `under` (the kind that holds half or more:
#'       `"a setting event"`, `"an earlier haul"`, `"nothing"`, else `"mixed"`), the setting event under most of its
#'       positions (`set_id`, with `set_share`, the share of the haul's positions it lies under, `set_start` and
#'       `set_end`, that event's first and last time under the haul, and
#'       `set_same_group`: whether it lies in the haul's own group), `soak_s` (the median time since what was last
#'       laid under its positions), and the ground the haul and that part of the set cover (`haul_m`, `set_m`).}
#'   }
#' @export
#'
#' @examples
#' # a vessel sets a line east at 6 kn, steams a round north of it back to where the set began, hauls the line
#' # at 1.5 kn, and steams home; a ping a minute
#' leg <- function(kn, min, east, north) data.frame(kn = rep(kn, min), east = east, north = north)
#' tr <- rbind(leg(9, 20, 0, 1), leg(6, 30, 1, 0), leg(9, 20, 0, 1), leg(9, 20, -1, 0), leg(9, 20, 0, -1),
#'             leg(1.5, 120, 1, 0), leg(9, 20, 0, -1))
#' m <- tr$kn * 1852 / 60
#' pings <- data.frame(
#'   vid = 1,
#'   time = as.POSIXct("2024-01-01", tz = "UTC") + 60 * seq_len(nrow(tr)),
#'   lon = 13 + cumsum(m * tr$east) / (111320 * cos(43.6 * pi / 180)),
#'   lat = 43.6 + cumsum(m * tr$north) / 110540
#' )
#' out <- os_add_setting(os_add_hauls(os_add_fishing(pings)), reach_m = 100)
#' table(out$setting)
#' attr(out, "under")
os_add_setting <- function(pings, by = NULL, haul = "haul_id", reach_m = c(25, 50, 100, 250, 500, 1000), back_s = 240 * 3600, run_in = FALSE) {
  if (!(is.character(haul) && length(haul) == 1 && !is.na(haul))) stop("`haul` must be one column name.", call. = FALSE)
  need <- c("vid", "time", "lon", "lat", haul, by)
  miss <- setdiff(need, names(pings))
  if (length(miss)) stop("`pings` lacks: ", paste(miss, collapse = ", "), call. = FALSE)
  if (!inherits(pings$time, "POSIXct")) stop("`pings$time` must be POSIXct.", call. = FALSE)
  if (!(is.numeric(reach_m) && length(reach_m) && !anyNA(reach_m) && all(reach_m > 0))) stop("`reach_m` must be one or more numbers of metres, over 0.", call. = FALSE)
  if (!(is.numeric(back_s) && length(back_s) == 1 && !is.na(back_s) && back_s > 0)) stop("`back_s` must be one number of seconds, over 0.", call. = FALSE)
  if (!(is.logical(run_in) && length(run_in) == 1 && !is.na(run_in))) stop("`run_in` must be TRUE or FALSE.", call. = FALSE)
  made <- intersect(c("on_later_haul", "setting", "setting_basis", "set_id"), names(pings))
  if (length(made)) stop("`pings` already has: ", paste(made, collapse = ", "), call. = FALSE)
  reach_m <- sort(unique(reach_m))

  n <- nrow(pings)
  t <- as.numeric(pings$time)
  grp <- if (is.null(by)) rep(1L, n) else pings[[by]]
  judged <- !is.na(pings$vid) & !is.na(t) & !is.na(pings$lon) & !is.na(pings$lat) & !is.na(grp)
  on_line <- rep(NA, n)
  setting <- rep(NA, n)
  basis <- rep("none: no vessel, time, position or group", n)
  set_id <- rep(NA_integer_, n)
  ranges <- sets <- under <- list()
  tz <- attr(pings$time, "tzone")
  tz <- if (is.null(tz)) "" else tz
  as_time <- function(x) as.POSIXct(x, origin = "1970-01-01", tz = tz)

  for (v in unique(pings$vid[judged])) {
    iv <- which(judged & pings$vid == v)
    iv <- iv[order(t[iv])]
    hid <- pings[[haul]][iv]
    if (all(is.na(hid))) {
      setting[iv] <- FALSE
      basis[iv] <- "none: the vessel has no haul"
      next
    }
    lat0 <- mean(pings$lat[iv])
    d <- list(i = iv, t = t[iv], x = pings$lon[iv] * 111320 * cos(lat0 * pi / 180), y = pings$lat[iv] * 110540, g = grp[iv], hid = hid,
              time = pings$time[iv], lon = pings$lon[iv], lat = pings$lat[iv])
    # the ground covered along the track, within a group
    step <- c(0, sqrt(diff(d$x)^2 + diff(d$y)^2))
    step[c(FALSE, d$g[-1] != d$g[-length(iv)])] <- 0
    d$gr <- cumsum(step)
    d$g0 <- stats::ave(d$t, d$g, FUN = min)                       # when the position's group begins
    inh <- !is.na(hid)
    d$h0 <- d$h1 <- rep(NA_real_, length(iv))
    d$h0[inh] <- stats::ave(d$t[inh], hid[inh], FUN = min)
    d$h1[inh] <- stats::ave(d$t[inh], hid[inh], FUN = max)

    best <- NULL
    fits <- numeric(0)
    for (r in reach_m) {
      one <- .os_setting_reach(d, r, back_s, grouped = !is.null(by), run_in = run_in)
      fits <- c(fits, one$fit)
      if (is.null(best) || (!is.na(one$fit) && (is.na(best$fit) || one$fit > best$fit))) best <- c(one, list(reach = r))
    }
    on_line[iv] <- best$wit
    setting[iv] <- best$setting
    set_id[iv] <- best$set_id
    basis[iv] <- ifelse(inh, "none: in a haul", ifelse(best$run_in, "none: the run in to a haul", best$basis))
    ranges[[length(ranges) + 1]] <- data.frame(
      vid = v, reach_m = best$reach, reach_basis = if (length(reach_m) == 1) "given" else "picked: the sets and hauls most often of one length",
      length_fit = best$fit, speed_min_kn = best$range[1], speed_max_kn = best$range[2], basis = best$basis,
      hauls = length(unique(hid[inh])), sets = nrow(best$sets))
    if (nrow(best$sets)) {
      s <- data.frame(vid = rep(v, nrow(best$sets)))
      if (!is.null(by)) s[[by]] <- best$sets$g
      sets[[length(sets) + 1]] <- cbind(s, set_id = best$sets$set_id, start = as_time(best$sets$start), end = as_time(best$sets$end),
                                        best$sets[c("pings", "on_later_haul", "distance_m")])
    }
    u <- best$under
    uu <- data.frame(vid = rep(v, nrow(u)))
    uu[[haul]] <- u$hid
    under[[length(under) + 1]] <- cbind(uu, u[c("pings", "share_set", "share_haul", "share_nothing", "under", "set_id", "set_share")], set_start = as_time(u$set_start),
                                        set_end = as_time(u$set_end), u[c("set_same_group", "soak_s", "haul_m", "set_m")])
  }
  bind <- function(x) if (length(x)) { o <- do.call(rbind, x); rownames(o) <- NULL; o } else NULL
  pings$on_later_haul <- on_line
  pings$setting <- setting
  pings$setting_basis <- basis
  pings$set_id <- set_id
  attr(pings, "ranges") <- bind(ranges)
  attr(pings, "sets") <- bind(sets)
  attr(pings, "under") <- bind(under)
  pings
}

# One vessel at one reach: the witness, the range, the events, and what lies under each haul --------------------------------
# `d` holds the vessel's judged pings in time order (see os_add_setting()).
.os_setting_reach <- function(d, r, back_s, grouped, run_in = FALSE) {
  n <- length(d$t)
  inh <- !is.na(d$hid)
  ih <- which(inh)
  # 1. the witness: a position in no haul, on the line of a haul that starts after it
  H <- .os_line_points(d$hid[ih], d$g[ih], d$t[ih], d$x[ih], d$y[ih], r / 2)
  k <- match(H$key, d$hid[ih])
  H$h0 <- d$h0[ih][k]; H$h1 <- d$h1[ih][k]; H$g0 <- d$g0[ih][k]
  io <- which(!inh)
  wit <- rep(NA, n)
  wit[io] <- FALSE
  pr <- .os_near(d$t[io], d$x[io], d$y[io], H$x, H$y, H$h0, r, back_s, function(a, b, at, ...) H$h0[b] > at[a] & H$h0[b] - at[a] <= back_s)
  wit[io[unique(pr$a)]] <- TRUE
  on <- unique(data.frame(i = io[pr$a], key = H$key[pr$b]))    # which later hauls' lines each marked position lies on
  # 2. the setting speed range, measured from the marked positions; none measured: the marked positions are the call
  df <- data.frame(vid = 1L, time = d$time, lon = d$lon, lat = d$lat, g = d$g, wit = wit)
  f <- os_add_fishing(df, by = "g", witness = "wit")
  rg <- attr(f, "ranges")
  measured <- identical(rg$basis[1], "measured")
  sp <- if (measured) f$fishing %in% TRUE & !inh else wit %in% TRUE
  range <- if (measured) c(rg$speed_min_kn[1], rg$speed_max_kn[1]) else c(NA_real_, NA_real_)
  basis <- if (measured) "measured" else "on a later haul's line"
  # 3. the setting events, kept where half or more of their positions are marked
  df$sp <- sp
  e <- os_add_hauls(df[c("vid", "time", "lon", "lat", "g", "sp")], by = "g", call = "sp")
  ev <- attr(e, "hauls")
  eid <- e$haul_id
  eid[!sp] <- NA_integer_
  share <- if (any(sp)) tapply(wit[sp] %in% TRUE, eid[sp], mean) else numeric(0)
  kept <- as.integer(names(share)[share >= 0.5])
  # `run_in`: the run in to a haul is no set of it: an event half or more on the line of the haul that next begins in
  # its group, with no more time between its end and that haul's start than the event itself took
  gone <- integer(0)
  if (run_in && length(kept)) {
    evk <- ev[match(kept, ev$haul_id), ]
    e1 <- as.numeric(evk$end)
    hs <- unique(data.frame(g = d$g[ih], key = d$hid[ih], h0 = d$h0[ih]))
    for (j in seq_along(kept)) {
      nx <- hs[hs$g == evk$g[j] & hs$h0 >= e1[j], ]
      if (!nrow(nx)) next
      nx <- nx[which.min(nx$h0), ]
      if (nx$h0 - e1[j] > evk$working_s[j]) next
      pos <- which(eid %in% kept[j])
      if (mean(pos %in% on$i[on$key == nx$key]) >= 0.5) gone <- c(gone, kept[j])
    }
  }
  is_run_in <- sp & eid %in% gone
  kept <- setdiff(kept, gone)
  new <- match(eid, sort(kept))                                 # the kept events numbered in time order
  setting <- sp & !is.na(new)
  set_id <- ifelse(setting, new, NA_integer_)
  is <- which(setting)
  sets <- data.frame(set_id = integer(0), g = d$g[0], start = numeric(0), end = numeric(0), pings = integer(0), on_later_haul = numeric(0), distance_m = numeric(0))
  if (length(is)) {
    first <- is[!duplicated(set_id[is])]
    o <- order(set_id[first])
    sets <- data.frame(set_id = set_id[first][o], g = d$g[first][o], start = as.vector(tapply(d$t[is], set_id[is], min)), end = as.vector(tapply(d$t[is], set_id[is], max)),
                       pings = as.vector(tapply(is, set_id[is], length)), on_later_haul = as.vector(tapply(wit[is] %in% TRUE, set_id[is], mean)),
                       distance_m = as.vector(tapply(d$gr[is], set_id[is], function(z) max(z) - min(z))))
  }
  # 4. each haul by what was last laid under it
  hk <- d$hid[ih]
  t_set <- t_haul <- rep(NA_real_, length(ih))
  set_pairs <- NULL
  if (length(is)) {
    S <- .os_line_points(set_id[is], d$g[is], d$t[is], d$x[is], d$y[is], r / 2)
    pr <- .os_near(d$t[ih], d$x[ih], d$y[ih], S$x, S$y, S$t, r, back_s, function(a, b, at, ...) S$t[b] < d$h0[ih][a] & at[a] - S$t[b] <= back_s)
    if (length(pr$a)) {
      o <- order(pr$a, -S$t[pr$b]); top <- !duplicated(pr$a[o])
      t_set[pr$a[o][top]] <- S$t[pr$b[o][top]]
      set_pairs <- data.frame(a = pr$a, key = S$key[pr$b], t = S$t[pr$b])
    }
  }
  pr <- .os_near(d$t[ih], d$x[ih], d$y[ih], H$x, H$y, H$t, r, back_s, function(a, b, at, ...) {
    earlier <- if (grouped) H$g0[b] < d$g0[ih][a] else H$h1[b] < d$h0[ih][a]
    earlier & H$key[b] != hk[a] & at[a] - H$t[b] <= back_s & H$t[b] < at[a] })
  if (length(pr$a)) {
    o <- order(pr$a, -H$t[pr$b]); top <- !duplicated(pr$a[o])
    t_haul[pr$a[o][top]] <- H$t[pr$b[o][top]]
  }
  last <- ifelse(is.na(t_set) & is.na(t_haul), "nothing", ifelse(is.na(t_haul) | (!is.na(t_set) & t_set >= t_haul), "set", "haul"))
  t_last <- pmax(t_set, t_haul, na.rm = TRUE)
  ids <- sort(unique(hk))
  per <- function(x, fun) as.vector(tapply(x, factor(hk, ids), fun))
  under <- data.frame(hid = ids, pings = per(hk, length), share_set = per(last == "set", mean), share_haul = per(last == "haul", mean), share_nothing = per(last == "nothing", mean))
  under$under <- ifelse(under$share_set >= 0.5, "a setting event", ifelse(under$share_haul >= 0.5, "an earlier haul", ifelse(under$share_nothing >= 0.5, "nothing", "mixed")))
  under$soak_s <- per(d$t[ih] - t_last, function(z) if (all(is.na(z))) NA_real_ else stats::median(z, na.rm = TRUE))
  under$haul_m <- per(d$gr[ih], function(z) max(z) - min(z))
  under$set_id <- NA_integer_; under$set_start <- under$set_end <- under$set_m <- under$set_share <- NA_real_; under$set_same_group <- NA
  if (!is.null(set_pairs)) {
    # the setting event under most of the haul's positions, the later on a tie; its first and last time under the haul
    # a haul's position is counted once an event, however many of the event's line points lie within reach of it
    set_pairs <- set_pairs[order(set_pairs$a, set_pairs$key, set_pairs$t), ]
    ak <- paste(set_pairs$a, set_pairs$key)
    lo <- set_pairs[!duplicated(ak), ]
    hi <- set_pairs[!duplicated(ak, fromLast = TRUE), ]
    hk2 <- paste(hk[lo$a], lo$key)
    cnt <- data.frame(hid = hk[lo$a], key = lo$key, n = 1L, s0 = lo$t, s1 = hi$t)
    cnt <- data.frame(hid = cnt$hid[!duplicated(hk2)], key = cnt$key[!duplicated(hk2)], n = as.vector(tapply(cnt$n, factor(hk2, unique(hk2)), sum)),
                      s0 = as.vector(tapply(cnt$s0, factor(hk2, unique(hk2)), min)), s1 = as.vector(tapply(cnt$s1, factor(hk2, unique(hk2)), max)))
    cnt <- cnt[order(cnt$hid, -cnt$n, -cnt$s0), ]
    cnt <- cnt[!duplicated(cnt$hid), ]
    k <- match(under$hid, cnt$hid)
    under$set_id <- cnt$key[k]; under$set_start <- cnt$s0[k]; under$set_end <- cnt$s1[k]
    under$set_share <- ifelse(is.na(k), NA_real_, cnt$n[k] / under$pings)
    at <- function(tt) d$gr[pmax(findInterval(tt, d$t), 1L)]
    under$set_m <- ifelse(is.na(k), NA_real_, at(under$set_end) - at(under$set_start))
    under$set_same_group <- sets$g[match(under$set_id, sets$set_id)] == d$g[ih][match(under$hid, hk)]
  }
  ratio <- under$set_m / under$haul_m
  list(wit = wit, setting = setting, set_id = set_id, basis = basis, range = range, sets = sets, under = under, run_in = is_run_in,
       fit = mean(!is.na(ratio) & is.finite(ratio) & ratio >= 0.8 & ratio < 1.25))
}

# The line through each key's positions (in time order, within a group): the positions, and points every `step` metres
# ---- between two consecutive ones that are further apart
.os_line_points <- function(key, g, t, x, y, step) {
  n <- length(t)
  out <- data.frame(key = key, t = t, x = x, y = y)
  if (n < 2) return(out)
  a <- which(key[-n] == key[-1] & g[-n] == g[-1])
  if (!length(a)) return(out)
  m <- ceiling(sqrt((x[a + 1] - x[a])^2 + (y[a + 1] - y[a])^2) / step) - 1L
  a <- a[m > 0]; m <- m[m > 0]
  if (!length(a)) return(out)
  j <- rep(a, m)
  fr <- sequence(m) / (rep(m, m) + 1)
  rbind(out, data.frame(key = key[j], t = t[j] + fr * (t[j + 1] - t[j]), x = x[j] + fr * (x[j + 1] - x[j]), y = y[j] + fr * (y[j + 1] - y[j])))
}

# The pairs of an `a` and a `b` point within `r` metres that pass `keep` --------------------------------------------------
# The plane is cut in cells of side `r`, so the nine cells around a point hold every point within `r` of it. The `a`
# points (in time order) are taken a block at a time, and of `b` only those whose reference time `bref` lies within
# `back_s` of the block, so that a vessel on the same ground all year is not held against the whole year at once.
.os_near <- function(at, ax, ay, bx, by, bref, r, back_s, keep, block = 20000L) {
  none <- list(a = integer(0), b = integer(0))
  if (!length(at) || !length(bx)) return(none)
  x0 <- min(ax, bx); y0 <- min(ay, by)
  acx <- floor((ax - x0) / r) + 1; acy <- floor((ay - y0) / r) + 1
  bcx <- floor((bx - x0) / r) + 1; bcy <- floor((by - y0) / r) + 1
  big <- max(acy, bcy) + 3
  bkey <- bcx * big + bcy
  oa <- order(at)
  res_a <- res_b <- list()
  for (s in seq(1L, length(oa), by = block)) {
    ia <- oa[s:min(s + block - 1L, length(oa))]
    ib <- which(bref >= min(at[ia]) - back_s & bref <= max(at[ia]) + back_s)
    if (!length(ib)) next
    ob <- ib[order(bkey[ib])]
    bk <- bkey[ob]
    for (dx in -1:1) for (dy in -1:1) {
      akey <- (acx[ia] + dx) * big + acy[ia] + dy
      hi <- findInterval(akey, bk)
      lo <- findInterval(akey - 1, bk) + 1L
      m <- hi - lo + 1L
      w <- which(m > 0)
      if (!length(w)) next
      a <- rep(ia[w], m[w])
      b <- ob[sequence(m[w], from = lo[w])]
      ok <- (ax[a] - bx[b])^2 + (ay[a] - by[b])^2 < r^2
      a <- a[ok]; b <- b[ok]
      ok <- keep(a, b, at)
      res_a[[length(res_a) + 1]] <- a[ok]; res_b[[length(res_b) + 1]] <- b[ok]
    }
  }
  if (!length(res_a)) return(none)
  list(a = unlist(res_a), b = unlist(res_b))
}
