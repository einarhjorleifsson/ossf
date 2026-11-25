# Copied from ramb 2026.11.24 (b777ac4), R/ping_speed.R, by data-raw/copy_from_ramb.R: do not edit here.
# The text is ramb's, with the names changed (rb_ to os_); a change is made in ramb and the copy made again.

# The distance of a leg on the WGS84 ellipsoid's local radii at the leg's mean latitude: the meridional radius for
# the north-south part, the prime vertical's parallel for the east-west part. Within 0.1 % of the geodesic for the
# legs a speed spans (tests/testthat/test-ping_speed.R ties it to os_calc_distance(method = "geodesic")), and
# expressible in SQL, which Karney's algorithm is not. fishycode's union used it from its plan 059 step 8.
# `lon0`, `lat0`, `lon1`, `lat1` are SQL expressions.
.os_ellipsoid_m_sql <- function(
  lon0 = "lon0",
  lat0 = "lat0",
  lon1 = "lon1",
  lat1 = "lat1"
) {
  .os_fill(
    "sqrt(pow(6335439.327 / pow(1 - 0.00669437999 * pow(sin(radians(({lat0} + {lat1}) / 2)), 2), 1.5) * radians({lat1} - {lat0}), 2) + pow(6378137.0 / sqrt(1 - 0.00669437999 * pow(sin(radians(({lat0} + {lat1}) / 2)), 2)) * cos(radians(({lat0} + {lat1}) / 2)) * radians((({lon1} - {lon0} + 540) % 360) - 180), 2))"
  )
}

#' Add a speed derived from the positions, within each stream
#'
#' Step 0 of the fishing-activity flow, after the flags. Each usable ping gets the speed from the ping before it to
#' the ping after it in its stream, divided by the time between them (a central difference), with that time beside
#' it. The first and last ping of a stream take the one leg they have. A recorded speed is not touched: this is the
#' speed of a feed that carries none, or one to hold against it.
#'
#' A speed is derived within one stream, never across two whose clocks may differ (`ramb::rb_add_ping_stream()`): a lag of
#' a few seconds between two feeds reads as distance. It is derived after the flags, from the pings that are left:
#'
#' * a ping flagged as a bad position (any `ping_flag` but `"duplicate"`) gets no speed, and is no ping's
#'   neighbour;
#' * a copy of a report (`"duplicate"`) is a report of its own stream: it gets no speed, but stays a neighbour in
#'   its stream. Dropped, a copy would double the time its neighbours' speeds span (fishycode plan 062, step 1);
#' * a ping with no vessel, time or position gets no speed.
#'
#' The distance is the WGS84 ellipsoid's, on its local radii at the leg's mean latitude, within 0.1 % of the
#' geodesic (Karney, [os_calc_distance()]) for the legs a speed spans, and the same in a data frame and a lazy table.
#' Two pings of one stream at the same time and place count as one.
#'
#' @param x A data frame or a lazy DuckDB table with `vid`, `time`, `lon` and `lat`, and `ping_flag` if the pings
#'   have been flagged.
#' @param stream The column naming each ping's stream, or `NULL` (default): one stream per vessel.
#' @param vid,time,lon,lat The columns, if named otherwise.
#'
#' @return `x` with `speed_derived` (knots), `speed_derived_span_s` (the seconds from the ping before to the ping
#'   after) and `speed_derived_basis` (`"derived"`, or `"none"` where the ping gets no speed): the same type, the same
#'   rows.
#' @family flag pings
#' @export
#'
os_add_ping_speed <- function(
  x,
  stream = NULL,
  vid = vid,
  time = time,
  lon = lon,
  lat = lat
) {
  stream_quo <- rlang::enquo(stream)
  has_stream <- !rlang::quo_is_null(stream_quo)
  .os_check_input(
    x,
    "os_add_ping_speed",
    cols = c(
      vid = rlang::as_name(rlang::enquo(vid)),
      time = rlang::as_name(rlang::enquo(time)),
      lon = rlang::as_name(rlang::enquo(lon)),
      lat = rlang::as_name(rlang::enquo(lat))
    ),
    also = if (has_stream) {
      c(stream = rlang::as_name(stream_quo))
    } else {
      character(0)
    },
    types = c(time = "time", lon = "numeric", lat = "numeric")
  )
  map <- .os_cols(
    vid = {{ vid }},
    time = {{ time }},
    lon = {{ lon }},
    lat = {{ lat }}
  )
  if (has_stream) {
    map <- c(map, stream = rlang::as_name(stream_quo))
  }
  have <- colnames(x)
  out_cols <- c("speed_derived", "speed_derived_span_s", "speed_derived_basis")
  if (any(out_cols %in% have)) {
    stop(
      "`x` already has ",
      paste(intersect(out_cols, have), collapse = ", "),
      "; drop them to derive the speed again.",
      call. = FALSE
    )
  }
  lazy_in <- inherits(x, "tbl_lazy")
  con <- .os_con_for(x)
  .os_scope()
  given <- x
  if (!lazy_in) {
    x <- .os_with_rid(x)
  }
  p <- .os_as_lazy(.os_std_in(x, map), con)
  flagged <- "ping_flag" %in% have
  # the pings a speed is derived from: unflagged, and the copies, which stay in their stream
  usable <- if (flagged) {
    "(ping_flag IS NULL OR ping_flag = 'duplicate')"
  } else {
    "TRUE"
  }
  own <- if (flagged) "p.ping_flag IS NULL" else "TRUE"
  st <- if (has_stream) "stream" else "NULL::VARCHAR"
  dist <- .os_ellipsoid_m_sql()
  sql <- .os_fill(
    "
WITH p AS NOT MATERIALIZED ({.os_sql(p)}),
u AS (SELECT DISTINCT vid, {st} AS rb_stream, time, lon, lat FROM p
      WHERE {usable} AND vid IS NOT NULL AND time IS NOT NULL AND lon IS NOT NULL AND lat IS NOT NULL),
w AS (SELECT *,
        coalesce(lag(time) OVER s, time) AS t0, coalesce(lag(lon) OVER s, lon) AS lon0, coalesce(lag(lat) OVER s, lat) AS lat0,
        coalesce(lead(time) OVER s, time) AS t1, coalesce(lead(lon) OVER s, lon) AS lon1, coalesce(lead(lat) OVER s, lat) AS lat1
      FROM u WINDOW s AS (PARTITION BY vid, rb_stream ORDER BY time, lon, lat)),
v AS (SELECT vid AS rb_vid, rb_stream, time AS rb_time, lon AS rb_lon, lat AS rb_lat,
        CAST(epoch(t1) - epoch(t0) AS DOUBLE) AS rb_span,
        CASE WHEN epoch(t1) - epoch(t0) > 0 THEN {dist} / (epoch(t1) - epoch(t0)) * 1.9438444924406 END AS rb_speed
      FROM w)
SELECT p.*,
  CASE WHEN {own} THEN v.rb_speed END AS speed_derived,
  CASE WHEN {own} AND v.rb_speed IS NOT NULL THEN v.rb_span END AS speed_derived_span_s,
  CASE WHEN {own} AND v.rb_speed IS NOT NULL THEN 'derived' ELSE 'none' END AS speed_derived_basis
FROM p LEFT JOIN v ON p.vid = v.rb_vid AND {if (has_stream) 'p.stream' else 'NULL::VARCHAR'} IS NOT DISTINCT FROM v.rb_stream
  AND p.time = v.rb_time AND p.lon = v.rb_lon AND p.lat = v.rb_lat"
  )
  out <- .os_std_out(dplyr::tbl(con, dplyr::sql(sql)), map)
  if (lazy_in) {
    return(.os_hold(out))
  }
  out <- dplyr::collect(out)
  .os_unregister(con)
  .os_as_given(out, given, ".rb_rid")
}

