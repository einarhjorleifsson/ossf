# Copied from ramb 2026.11.24 (b777ac4), R/track.R, by data-raw/copy_from_ramb.R: do not edit here.
# The text is ramb's, with the names changed (rb_ to os_); a change is made in ramb and the copy made again.

#' Great-circle distance between two points
#'
#' Two methods:
#' * `"haversine"` (default): on a sphere of radius 6,371,000 m — the radius the
#'   impossible-position filters ([os_flag_ping_impossible()]) use, so a distance computed
#'   here agrees with theirs.
#' * `"geodesic"`: Karney's algorithm on the WGS84 ellipsoid (vendored GeographicLib C,
#'   see `inst/COPYRIGHTS`). Exact everywhere, including nearly antipodal points.
#'
#' Which method: use the default for anything measured against the impossible-position
#' filters (a speed computed on the ellipsoid can disagree with a filter threshold purely
#' from the sphere mismatch) and for large data — Haversine is about 40-50x faster and is
#' the only method expressible in SQL. Use `"geodesic"` when the absolute distance itself
#' matters, e.g. distance-steamed totals: the Haversine error is systematic, up to ~0.6 %
#' depending on latitude and bearing, so it does not cancel in sums. A speed set beside
#' recorded speed over ground is another such case, which is why `ramb::rb_calc_speed()`
#' defaults to `"geodesic"`.
#'
#' Two forms. The vector form takes the four coordinate vectors; they recycle to a common
#' length, so it works inside `mutate()` with `dplyr::lag()` and `dplyr::lead()`. The
#' data-frame form takes a data frame as the first argument and the coordinate columns as
#' tidy-eval arguments, for point-to-point columns such as ping to port. A lazy DuckDB
#' table is not supported — collect first.
#'
#' @param x The vector form: longitude of the first point, decimal degrees. The data-frame
#'   form: a data frame with the coordinate columns.
#' @param ... The vector form: `lat1`, `lon2` and `lat2`, in that order.
#' @param method `"haversine"` (default) or `"geodesic"`.
#' @param lon1,lat1,lon2,lat2 The data-frame form: the coordinate columns (tidy-eval).
#'   `lon1` and `lat1` default to the grammar names `lon` and `lat`; `lon2` and `lat2`
#'   are required. The named vector form needs all four.
#'
#' @return Numeric vector of distances in metres; `NA` where any coordinate is `NA`.
#'
#' @references For `"geodesic"`: Karney, C.F.F. (2013). "Algorithms for
#'   geodesics". Journal of Geodesy, 87(1), 43–55.
#'   \doi{10.1007/s00190-012-0578-z}; the engine is GeographicLib's
#'   `geodesic.c` (\url{https://geographiclib.sourceforge.io/}), vendored
#'   unchanged.
#'
#' @export
os_calc_distance <- function(
  x,
  ...,
  method = c("haversine", "geodesic"),
  lon1 = lon,
  lat1 = lat,
  lon2,
  lat2
) {
  method <- match.arg(method)

  # Named vector form: os_calc_distance(lon1 = ..., lat1 = ..., lon2 = ..., lat2 = ...).
  # All four are required: the defaults lon1 = lon, lat1 = lat name columns of the
  # data-frame form, and evaluated here they would silently pick up any `lon`/`lat`
  # on the search path (a global `lon <- 5` gave 1,258,560 m instead of an error).
  if (missing(x)) {
    if (missing(lon1) || missing(lat1) || missing(lon2) || missing(lat2)) {
      stop(
        "The named vector form needs all of `lon1`, `lat1`, `lon2` and `lat2`.",
        call. = FALSE
      )
    }
    return(.os_dist(lon1, lat1, lon2, lat2, method))
  }

  if (is.data.frame(x) || inherits(x, "tbl_lazy")) {
    if (inherits(x, "tbl_lazy")) {
      stop(
        "`os_calc_distance()` does not support lazy DuckDB tables; collect first.",
        call. = FALSE
      )
    }
    if (...length()) {
      stop(
        "In the data-frame form the columns are named arguments, e.g. ",
        "`os_calc_distance(d, lon2 = x2, lat2 = y2)`.",
        call. = FALSE
      )
    }
    if (
      rlang::quo_is_missing(rlang::enquo(lon2)) ||
        rlang::quo_is_missing(rlang::enquo(lat2))
    ) {
      stop("The data-frame form needs `lon2` and `lat2`.", call. = FALSE)
    }
    map <- .os_cols(
      lon1 = {{ lon1 }},
      lat1 = {{ lat1 }},
      lon2 = {{ lon2 }},
      lat2 = {{ lat2 }}
    )
    cols <- lapply(stats::setNames(nm = names(map)), function(nm) {
      cl <- map[[nm]]
      if (!cl %in% names(x)) {
        stop(
          sprintf("Column `%s` not found in the data frame.", cl),
          call. = FALSE
        )
      }
      v <- x[[cl]]
      if (!is.numeric(v)) {
        stop(
          sprintf("Column `%s` must be numeric, not <%s>.", cl, class(v)[1]),
          call. = FALSE
        )
      }
      v
    })
    return(.os_dist(cols$lon1, cols$lat1, cols$lon2, cols$lat2, method))
  }

  # Positional vector form: os_calc_distance(lon1, lat1, lon2, lat2)
  if (!missing(lon1) || !missing(lat1) || !missing(lon2) || !missing(lat2)) {
    stop(
      "Column arguments (`lon1`, ...) only apply to the data-frame form.",
      call. = FALSE
    )
  }
  dots <- list(...)
  if (length(dots) != 3L) {
    stop(
      "The vector form is `os_calc_distance(lon1, lat1, lon2, lat2)`.",
      call. = FALSE
    )
  }
  .os_dist(x, dots[[1]], dots[[2]], dots[[3]], method)
}

# The computation behind both forms of os_calc_distance().
.os_dist <- function(lon1, lat1, lon2, lat2, method) {
  if (method == "haversine") {
    return(.os_haversine(lon1, lat1, lon2, lat2))
  }
  # .os_geodesic() does not recycle; bring the vectors to a common length first
  l <- lengths(list(lon1, lat1, lon2, lat2))
  n <- max(l)
  if (any(!l %in% c(1L, n))) {
    stop(
      "Coordinate vectors must have length 1 or a common length.",
      call. = FALSE
    )
  }
  .os_geodesic(
    rep_len(lon1, n),
    rep_len(lat1, n),
    rep_len(lon2, n),
    rep_len(lat2, n)
  )
}

# Haversine on the filters' sphere (R = 6,371,000 m). Vectorised, recycles, NA passes through.
.os_haversine <- function(lon1, lat1, lon2, lat2) {
  k <- pi / 180
  a <- sin((lat2 - lat1) * k / 2)^2 +
    cos(lat1 * k) * cos(lat2 * k) * sin((lon2 - lon1) * k / 2)^2
  2 * 6371000 * asin(sqrt(pmin(1, a)))
}

.os_geodesic <- function(lon1, lat1, lon2, lat2) {
  geod_karney(lon1, lat1, lon2, lat2)
}

#' Median speed over a time window around each position
#'
#' The median of the speeds within `window_s` seconds either side of each position, its own included. A
#' ping's own speed jitters through a tow; the median over ±10 minutes holds steady. On the Icelandic AIS of
#' 2007-2026 it called 92.7 % of the fixes inside recorded tows fishing, against 89.2 % on each fix's own
#' speed, and 86.8 % of the fixes 30 minutes or more from any tow not fishing, against 87.2 % (fishycode
#' decision 090). The window is a time span: positions without a speed are not counted, and get `NA`. Give one
#' vessel at a time (`dplyr::mutate(.by = vid)`).
#'
#' The same median is the `window_s` argument of `ramb::rb_add_fishing()` and `ramb::rb_create_record_stretches()`, where
#' it runs in DuckDB.
#'
#' @param speed A numeric vector of speeds.
#' @param time Their times (`POSIXct`), in any order.
#' @param window_s Half the window, in seconds. 0 returns `speed` unchanged.
#'
#' @return A numeric vector, `NA` where `speed` or `time` is `NA`.
#' @export
#'
os_calc_speed_smooth <- function(speed, time, window_s = 600) {
  stopifnot(
    length(speed) == length(time),
    is.numeric(window_s),
    length(window_s) == 1,
    window_s >= 0
  )
  out <- rep(NA_real_, length(speed))
  ok <- which(!is.na(speed) & !is.na(time))
  if (window_s == 0 || !length(ok)) {
    out[ok] <- speed[ok]
    return(out)
  }
  t <- as.numeric(time[ok])
  s <- speed[ok]
  o <- order(t)
  t <- t[o]
  s <- s[o]
  lo <- findInterval(t - window_s, t, left.open = TRUE) + 1L
  hi <- findInterval(t + window_s, t)
  out[ok[o]] <- vapply(
    seq_along(t),
    function(i) stats::median(s[lo[i]:hi[i]]),
    numeric(1)
  )
  out
}

#' Number runs of consecutive equal values
#'
#' Each run of consecutive equal values gets a sequential number, starting
#' at 1.
#'
#' @param x A vector, normally character or integer.
#'
#' @return An integer vector numbering the run each element belongs to.
#' @export
#'
os_number_runs <- function(x) {
  x <- dplyr::if_else(x != dplyr::lag(x), 1L, 0L, 1L)
  x <- cumsum(x)
  return(x)
}

