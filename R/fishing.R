# Fishing from a vessel's track: one rule, in one file -----------------------------------------------------------------
#
# The rule: a ping is fishing when the vessel's smoothed speed is within its working speed range.
#  1. the speed at a ping, from the track itself: the distance from the ping before to the ping after, over the time
#     between them;
#  2. smoothed: the median of the speeds within a time window either side of the ping;
#  3. the vessel's working speed range, from the best source there is: given by the user, measured where a witness
#     marks the vessel's work, or estimated from the vessel's own speeds;
#  4. the call, and which source the range came from (its basis).
# Nothing else. Where it comes from, what was tried and left out, and how it scored: README.md.
#
# Drafted with AI assistance (Claude), 2026-11-25; not yet checked by a person.

#' Fishing from a vessel's track
#'
#' A ping is fishing when the vessel's smoothed speed is within the vessel's working speed range. On a static
#' gear (pots, nets, longlines) that is hauling; on a towed gear, towing.
#'
#' The speed at a ping is derived from the track: the distance from the ping before to the ping after, divided
#' by the time between them (at the first and last ping of a group, the one step there is). It is smoothed by
#' the median of the speeds within `window_s` seconds either side of the ping.
#'
#' The working speed range is taken, vessel by vessel, from the best source there is:
#' \describe{
#'   \item{given}{`range` holds the vessel: its `speed_min_kn` and `speed_max_kn` are used as they are.}
#'   \item{measured}{`witness` names a column that marks pings an independent source says are fishing (a
#'     hauler sensor active, a logbook's recorded haul or tow). The range is the stretch of speeds, around the
#'     speed the marked pings are most often at, at which a ping is more often marked than not.}
#'   \item{estimated}{from the vessel's own smoothed speeds: 0 to the low point (valley) of their distribution
#'     that best separates the slower from the faster speeds.}
#' }
#' Each ping carries the source that made its call in `fishing_basis`, and the ranges are returned as the
#' attribute `"ranges"`. No ping is dropped: one that cannot be judged has `fishing` `NA` and the reason as its
#' basis.
#'
#' @param pings A data frame of pings with the columns `vid` (the vessel), `time` (`POSIXct`), `lon` and `lat`
#'   (decimal degrees). Rows may be in any order.
#' @param by The name of a column that groups a vessel's pings (a trip), or `NULL`. Speeds and windows never
#'   reach across groups. A ping with no group (`NA`) is not judged.
#' @param window_s Half the smoothing window in seconds. `NULL` (the default) takes, for each vessel, its
#'   median time between pings, so the window holds the ping and its neighbour either side. 0 does not smooth.
#' @param range A data frame with `vid`, `speed_min_kn` and `speed_max_kn`: ranges given for some or all
#'   vessels, or `NULL`.
#' @param witness The name of a logical column of `pings`, `TRUE` where an independent source says the vessel
#'   is fishing and `FALSE` where it says not (`NA`: no word), or `NULL`.
#'
#' @return `pings`, rows in the order given, with four columns added: `speed_kn` (the speed at the ping),
#'   `speed_smooth_kn` (smoothed), `fishing` (logical) and `fishing_basis` (`"given"`, `"measured"`,
#'   `"estimated"`, or `"none: "` and why). The attribute `"ranges"` is a data frame, a row a vessel: its pings,
#'   median time between pings, window, range and the range's basis.
#' @export
#'
#' @examples
#' # a vessel steams out, hauls for two hours, and steams home, a ping a minute
#' set.seed(1)
#' kn <- c(rep(8, 60), rep(1.2, 120), rep(8, 60)) * exp(rnorm(240, 0, 0.1))
#' pings <- data.frame(
#'   vid = 1,
#'   time = as.POSIXct("2024-01-01", tz = "UTC") + 60 * seq_along(kn),
#'   lon = 13 + cumsum(kn * 1852 / 60) / 80000,
#'   lat = 43.6
#' )
#' out <- os_add_fishing(pings)
#' table(out$fishing)
#' attr(out, "ranges")
os_add_fishing <- function(pings, by = NULL, window_s = NULL, range = NULL, witness = NULL) {
  need <- c("vid", "time", "lon", "lat", by, witness)
  miss <- setdiff(need, names(pings))
  if (length(miss)) stop("`pings` lacks: ", paste(miss, collapse = ", "), call. = FALSE)
  if (!inherits(pings$time, "POSIXct")) stop("`pings$time` must be POSIXct.", call. = FALSE)
  if (!is.null(window_s) && !(is.numeric(window_s) && length(window_s) == 1 && !is.na(window_s) && window_s >= 0)) {
    stop("`window_s` must be NULL or one number of seconds, 0 or more.", call. = FALSE)
  }
  if (!is.null(range) && length(setdiff(c("vid", "speed_min_kn", "speed_max_kn"), names(range)))) {
    stop("`range` must have the columns vid, speed_min_kn and speed_max_kn.", call. = FALSE)
  }
  if (!is.null(witness) && !is.logical(pings[[witness]])) stop("`pings$", witness, "` must be logical.", call. = FALSE)
  made <- intersect(c("speed_kn", "speed_smooth_kn", "fishing", "fishing_basis"), names(pings))
  if (length(made)) stop("`pings` already has: ", paste(made, collapse = ", "), call. = FALSE)

  n <- nrow(pings)
  t <- as.numeric(pings$time)
  grp <- if (is.null(by)) rep(1L, n) else pings[[by]]
  judged <- !is.na(pings$vid) & !is.na(t) & !is.na(pings$lon) & !is.na(pings$lat) & !is.na(grp)
  speed <- smooth <- rep(NA_real_, n)
  fishing <- rep(NA, n)
  basis <- rep("none: no vessel, time, position or group", n)
  ranges <- list()

  for (v in unique(pings$vid[judged])) {
    iv <- which(judged & pings$vid == v)
    groups <- lapply(split(iv, grp[iv]), function(i) i[order(t[i])])
    for (i in groups) speed[i] <- .os_speed(t[i], pings$lon[i], pings$lat[i])
    spacing <- stats::median(unlist(lapply(groups, function(i) diff(t[i]))))
    w <- if (is.null(window_s)) spacing else window_s
    if (is.na(w)) w <- 0
    for (i in groups) smooth[i] <- .os_smooth(speed[i], t[i], w)

    s <- smooth[iv]
    r <- NULL
    if (!is.null(range) && v %in% range$vid) {
      k <- match(v, range$vid)
      r <- list(min = range$speed_min_kn[k], max = range$speed_max_kn[k], basis = "given")
    }
    if (is.null(r) && !is.null(witness)) {
      m <- .os_range_measured(s[pings[[witness]][iv] %in% TRUE], s[pings[[witness]][iv] %in% FALSE])
      if (!is.na(m$max)) r <- c(m, basis = "measured")
    }
    if (is.null(r)) {
      e <- .os_range_estimated(s)
      r <- if (is.na(e$max)) list(min = NA_real_, max = NA_real_, basis = paste("none:", e$why)) else c(e[c("min", "max")], basis = "estimated")
    }
    fishing[iv] <- s >= r$min & s <= r$max
    basis[iv] <- ifelse(is.na(s) & !is.na(r$max), "none: no speed", r$basis)
    ranges[[length(ranges) + 1]] <- data.frame(vid = v, pings = length(iv), spacing_s = spacing, window_s = w,
                                               speed_min_kn = r$min, speed_max_kn = r$max, basis = r$basis)
  }
  pings$speed_kn <- speed
  pings$speed_smooth_kn <- smooth
  pings$fishing <- fishing
  pings$fishing_basis <- basis
  attr(pings, "ranges") <- if (length(ranges)) do.call(rbind, ranges) else NULL
  pings
}

# The speed at each ping of one group, in time order (knots): from the ping before to the ping after ---------------------
.os_speed <- function(t, lon, lat) {
  n <- length(t)
  if (n < 2) return(rep(NA_real_, n))
  a <- pmax(seq_len(n) - 1L, 1L)
  b <- pmin(seq_len(n) + 1L, n)
  span <- t[b] - t[a]
  ifelse(span > 0, .os_dist_m(lon[a], lat[a], lon[b], lat[b]) / span * 3600 / 1852, NA_real_)
}

# Metres between two positions close to each other, on the WGS84 ellipsoid: the north and east steps, each at the
# ellipsoid's own radius of curvature at the latitude between them (within a millionth for a step of a few km)
.os_dist_m <- function(lon1, lat1, lon2, lat2) {
  a <- 6378137
  e2 <- 0.00669437999014
  phi <- (lat1 + lat2) / 2 * pi / 180
  k <- 1 - e2 * sin(phi)^2
  north <- a * (1 - e2) / k^1.5 * (lat2 - lat1) * pi / 180
  east <- a / sqrt(k) * cos(phi) * (((lon2 - lon1 + 180) %% 360) - 180) * pi / 180
  sqrt(north^2 + east^2)
}

# The median of the speeds within `w` seconds either side of each ping of one group, in time order -----------------------
.os_smooth <- function(speed, t, w) {
  out <- rep(NA_real_, length(speed))
  ok <- which(!is.na(speed))
  if (w == 0 || !length(ok)) {
    out[ok] <- speed[ok]
    return(out)
  }
  s <- speed[ok]
  tt <- t[ok]
  lo <- findInterval(tt - w, tt, left.open = TRUE) + 1L
  hi <- findInterval(tt + w, tt)
  out[ok] <- vapply(seq_along(tt), function(i) stats::median(s[lo[i]:hi[i]]), numeric(1))
  out
}

# The fewest speeds a range is read from; fewer, and the vessel has no range of that kind
.os_min_n <- 100L

# The range estimated from a vessel's own speeds: 0 to the valley that best separates slower from faster ---------------
# The distribution of the log of the speeds (a kernel density, R's bandwidth rule) may have several low points. Each is
# scored by how well it parts the speeds in two: the share of speeds either side times the squared distance between the
# two sides' means (Otsu's measure). The best one is the upper limit.
.os_range_estimated <- function(speed) {
  z <- log(speed[is.finite(speed) & speed > 0])
  none <- function(why) list(min = NA_real_, max = NA_real_, why = why)
  if (length(z) < .os_min_n) return(none("too few speeds for a range"))
  if (length(unique(z)) < 2) return(none("one speed only"))
  d <- stats::density(z, bw = "nrd0", n = 1024)
  y <- d$y
  y[y < max(y) * sqrt(.Machine$double.eps)] <- 0
  # the highs and lows of the density; a flat stretch counts once, at its middle
  rl <- rle(y)
  end <- cumsum(rl$lengths)
  mid <- end - (rl$lengths - 1) %/% 2 - (rl$lengths - 1) %% 2
  step <- diff(rl$values)
  high <- mid[which(c(FALSE, step > 0) & c(step < 0, FALSE))]
  low <- mid[which(c(FALSE, step < 0) & c(step > 0, FALSE))]
  low <- low[low > min(high) & low < max(high)]
  if (length(high) < 2 || !length(low)) return(none("the speeds show one kind only"))
  zs <- sort(z)
  nz <- length(zs)
  cs <- cumsum(zs)
  k <- findInterval(d$x[low], zs)
  below <- k / nz
  score <- ifelse(k > 0 & k < nz, below * (1 - below) * (cs[pmax(k, 1)] / pmax(k, 1) - (cs[nz] - cs[pmax(k, 1)]) / pmax(nz - k, 1))^2, 0)
  list(min = 0, max = exp(d$x[low[which.max(score)]]), why = NA_character_)
}

# The range measured where a witness marks the vessel's work -------------------------------------------------------------
# Two distributions of the log speed, the marked pings' and the others', each weighted by its count of pings. The range
# runs from the speed the marked pings are most often at, down and up, to where the others become the more common.
# Marked pings the more common all the way down: the lower limit is 0. All the way up: no range.
.os_range_measured <- function(inside, outside) {
  a <- log(inside[is.finite(inside) & inside > 0])
  b <- log(outside[is.finite(outside) & outside > 0])
  none <- list(min = NA_real_, max = NA_real_)
  if (length(a) < .os_min_n || length(b) < .os_min_n || length(unique(a)) < 2 || length(unique(b)) < 2) return(none)
  ha <- stats::bw.nrd0(a)
  hb <- stats::bw.nrd0(b)
  from <- min(a, b) - 3 * max(ha, hb)
  to <- max(a, b) + 3 * max(ha, hb)
  da <- stats::density(a, bw = ha, from = from, to = to, n = 1024)
  db <- stats::density(b, bw = hb, from = from, to = to, n = 1024)
  more <- da$y * length(a) > db$y * length(b)
  m <- which.max(da$y)
  if (!more[m]) return(none)
  lo <- m
  while (lo > 1 && more[lo - 1]) lo <- lo - 1
  hi <- m
  while (hi < 1024 && more[hi + 1]) hi <- hi + 1
  if (hi == 1024) return(none)
  list(min = if (lo == 1) 0 else exp(da$x[lo]), max = exp(da$x[hi]))
}
