# Copied from ramb 2026.11.24 (b777ac4), R/trip.R, by data-raw/copy_from_ramb.R: do not edit here.
# The text is ramb's, with the names changed (rb_ to os_); a change is made in ramb and the copy made again.

.os_con_for <- function(x, ...) {
  # the connection of the lazy inputs, which must share one; a data frame is registered in it
  lazy <- Filter(function(t) inherits(t, "tbl_lazy"), list(x, ...))
  if (length(lazy) == 0) {
    return(.os_connection())
  }
  cons <- lapply(lazy, dbplyr::remote_con)
  if (!all(vapply(cons, identical, logical(1), cons[[1]]))) {
    stop(
      "The lazy tables are on different DuckDB connections; open them on one (ramb::rb_read_parquet() and ramb::rb_read_example() take `con`).",
      call. = FALSE
    )
  }
  cons[[1]]
}

# A name for a table on `con`, numbered, not drawn from R's random numbers: a draw moves the caller's random
# stream (the maintainer's rule, 2026-10-27), and names drawn after set.seed() repeat. A name the connection
# already has (after the package is loaded again) is skipped.
.os_names <- new.env(parent = emptyenv())

.os_names$n <- 0L

.os_name <- function(con, prefix) {
  repeat {
    .os_names$n <- .os_names$n + 1L
    nm <- paste0(prefix, .os_names$n)
    if (!DBI::dbExistsTable(con, nm)) {
      return(nm)
    }
  }
}

# Data frames registered for one call, and the temporary tables it makes (ramb:::.rb_temp_table()), belong to that call
# (the maintainer's rule, 2026-10-27: a function leaves the caller's session as it found it, on error too). A
# function that registers calls .os_scope() first: when it exits, by return or by error, what it made is released,
# unless it handed it to its lazy result with .os_hold(), which then reads it for the rest of the session. A
# release never touches what an earlier call made, so a lazy result made before stays readable. Registered frames
# left behind in a loop accumulate (fishycode R/write_parquet.R).
.os_reg <- new.env(parent = emptyenv())

.os_tmp <- new.env(parent = emptyenv())

.os_scopes <- new.env(parent = emptyenv())

.os_scopes$stack <- list()

.os_register <- function(con, x, prefix = "rb_in_") {
  nm <- .os_name(con, prefix)
  x <- as.data.frame(x)
  # a date-time or date stored as integer (seq() makes them) registers as INTEGER, not as a time
  for (k in names(x)) {
    if (inherits(x[[k]], c("POSIXct", "Date")) && is.integer(x[[k]])) {
      storage.mode(x[[k]]) <- "double"
    }
  }
  duckdb::duckdb_register(con, nm, x)
  assign(nm, con, envir = .os_reg)
  nm
}

.os_made <- function() c(ls(.os_reg), ls(.os_tmp))

# Open a scope in the calling function's frame; closed by an on.exit() placed there.
.os_scope <- function(env = parent.frame()) {
  before <- .os_made()
  .os_scopes$stack <- c(.os_scopes$stack, list(before))
  do.call(
    base::on.exit,
    list(substitute(.os_close_scope(b), list(b = before)), add = TRUE),
    envir = env
  )
  invisible()
}

.os_close_scope <- function(before) {
  .os_release_since(before)
  n <- length(.os_scopes$stack)
  if (n > 0) {
    .os_scopes$stack <- .os_scopes$stack[-n]
  }
}

# What the innermost open scope has made so far (everything, outside a scope).
.os_own <- function() {
  n <- length(.os_scopes$stack)
  setdiff(.os_made(), if (n > 0) .os_scopes$stack[[n]])
}

# Release the frames and drop the temporary tables made since `before`.
.os_release_since <- function(before) {
  for (nm in setdiff(ls(.os_reg), before)) {
    try(duckdb::duckdb_unregister(get(nm, envir = .os_reg), nm), silent = TRUE)
    rm(list = nm, envir = .os_reg)
  }
  for (nm in setdiff(ls(.os_tmp), before)) {
    try(
      DBI::dbExecute(
        get(nm, envir = .os_tmp),
        sprintf("DROP TABLE IF EXISTS %s", nm)
      ),
      silent = TRUE
    )
    rm(list = nm, envir = .os_tmp)
  }
}

# Release what the current call has made, once it has collected its result.
.os_unregister <- function(con) {
  .os_release_since(setdiff(.os_made(), .os_own()))
}

# Hand what the current call has made to its lazy result `out`: kept for the session, never released.
.os_hold <- function(out) {
  # forced first: `out` may register the very table it reads
  force(out)
  own <- .os_own()
  rm(list = intersect(own, ls(.os_reg)), envir = .os_reg)
  rm(list = intersect(own, ls(.os_tmp)), envir = .os_tmp)
  out
}

.os_as_lazy <- function(x, con) {
  if (inherits(x, "tbl_lazy")) {
    return(x)
  }
  dplyr::tbl(con, .os_register(con, x))
}

.os_sql <- function(x) as.character(dbplyr::sql_render(dplyr::ungroup(x)))

#' Create port calls from vessel tracks
#'
#' A step-1 builder of the fishing-activity flow: the port calls, the times a vessel spends in port. Port calls
#' and voyages ([os_create_voyages()]) are the two halves of a track, and the two functions share their methods.
#'
#' * `"dwell"`: a port call is elapsed time at a port, in port for at least a minimum time (`min_dwell_h`), from
#'   any of three rules on the pings and, optionally, from declared port events (below). [os_create_voyages()]
#'   with `method = "dwell"` cuts the track at these port calls.
#' * `"runs"`, `"jepol"`, `"cnr"`, `"iapesca"`: a port call is the time in port between two voyages of the
#'   method ([os_create_voyages()], whose arguments for these methods go in `...`), from the end of one voyage
#'   to the start of the next, at the port the first ended in, else the port the second began from. Where
#'   neither voyage reaches a port (a long voyage split, or a silence at sea), there is no port call between
#'   them. A vessel whose record begins or ends in port has a port call there too: from its first ping to its first
#'   voyage, where that voyage comes from a port, and from its last voyage to its last ping, where that voyage ends in
#'   one (`edge` says which). A port call spans its own pings, from the first after the voyage before it to the last
#'   before the voyage after it, so no ping lies in both a voyage and a port call; a silence between a voyage and its
#'   port call lies in neither, and a port call with no ping of its own between two voyages is none. Until ramb
#'   2026.11.24 the port call ran from the voyage before's last ping to the voyage after's first, so those two pings at
#'   sea fell in it too, and a vessel's opening and closing stays in port were in no port call (found on a case study
#'   with a ping a minute, 2026-11-24: every departure and return lay two pings from the harbour instead of one, and
#'   the records in port before a vessel's first port call or after its last, 12,787, in none).
#'
#' The rules of `"dwell"`:
#'
#' * `tag_then_gap`: two consecutive pings at the same port, the first within `radius_in` of it, at least
#'   the minimum dwell apart, one of them inside the polygon;
#' * `gap_near_port`: the same, neither inside the polygon;
#' * `tag_span`: a run of pings inside the polygon spanning at least the minimum dwell;
#' * `event_pair` (with `events`): an arrival then a departure event at the same port, adjacent in the
#'   vessel's events, both judged good, at least the minimum dwell apart, and no ping between them farther than
#'   `pair_away_m` from the port (the track shows the vessel left: a pair whose events between were lost).
#'
#' With `speed_kn` (fishycode plan 031 §4.1), two rules scale the minimum with the ping interval, as a time
#' budget: the time the vessel could have spent in port, given the pings either side and the time to sail from
#' and to them.
#'
#' * `gap_budget`: two consecutive pings, the first within `radius_in` of a port, the second anywhere: a port call
#'   when the time between them, less the sailing time from the first ping to the port and from the port to the
#'   second, is at least the minimum dwell. Where pings are sparse, the second is often outside the departure
#'   radius that `tag_then_gap` asks for.
#' * `span_budget`: a run of pings inside the polygon, too short for `tag_span`: a port call when the time from
#'   the ping before the run to the ping after it, less the sailing time to and from the port, is at least the
#'   minimum dwell. With hourly pings a call of three hours holds only two or three pings.
#'
#' The sailing time uses `speed_kn` knots, or with `speed_kn = "vessel"` each vessel's own speed: the upper
#' quartile of its leg speeds between 2 and 30 knots (legs of 1 minute to 2 hours), or `speed_fallback_kn` where it
#' has fewer than 20 such legs. Distances are in metres, in `crs_m`, to the port's polygon. The budget is the most
#' the vessel could have spent in port, so these rules find more calls, and also more that are not calls.
#'
#' `min_dwell_h = "valley"` sets the minimum from the data (plan 031, ask a): the candidate calls (every run of
#' pings inside a port, and every silence that begins within `radius_in` of one) are measured by the time budget
#' (or by their span and silence, without `speed_kn`), and the minimum is the low point of the density of their
#' log lengths between the mode under an hour (pass-throughs) and the mode over two hours (calls). It is returned
#' as the attribute `min_dwell_h`, and said in a message.
#'
#' A ping takes the port among those whose `radius_out` buffer holds it, preferring one whose
#' `radius_in` buffer also holds it, then the nearest. Seeds are extended over the contiguous inside pings
#' at either end and merged; port calls from pings and from events are merged in time. `rule` names the
#' strongest evidence: event_pair > tag_then_gap > tag_span > gap_near_port > span_budget > gap_budget.
#'
#' @param pings A data frame or lazy DuckDB table of cleaned pings: `vid`, `time`, and `lon`, `lat` for
#'   `"dwell"`; for the other methods, the columns [os_create_voyages()] needs.
#' @param ports An sf object of port polygons with `port_id`, and for `"dwell"` optionally `radius_out`
#'   (metres; the departure radius, at least `radius_in`). Required for `"dwell"` and `"iapesca"`, optional for
#'   `"cnr"`, not used by `"runs"` and `"jepol"` (they read `port_id` on the pings).
#' @param method `"dwell"`, `"runs"`, `"jepol"`, `"cnr"` or `"iapesca"`.
#' @param min_dwell_h For `"dwell"`: the minimum time in port, hours, where `min_dwell` has no row for the port;
#'   or `"valley"`, set from the data (above).
#' @param speed_kn For `"dwell"`: `NULL` (no time budget, the default), a speed in knots, or `"vessel"` for each
#'   vessel's own (above).
#' @param speed_fallback_kn For `speed_kn = "vessel"`: the speed of a vessel with too few legs to measure its own.
#' @param min_dwell For `"dwell"`: optional data frame `port_id`, `min_dwell_h`: a minimum per port.
#' @param radius_in For `"dwell"`: the arrival radius, metres.
#' @param events For `"dwell"`: optional data frame or lazy table of port events: `vid`, `time`, `io` (`"I"`
#'   arrival, `"U"` departure), `port_id`, `good` (logical, as `ramb::rb_flag_port_event()` adds it), and for `presence`
#'   `lon`, `lat`.
#' @param presence For `"dwell"` with `events`: `TRUE` takes each good event with a position into the pings the rules
#'   read, as a report of the vessel at that place at that time (fishycode plan 048 §7.3): where a track's port events
#'   were pings, a port call that stood on them stands on them as judged events, beside the pairs. An event with no
#'   position, or not good, gives none; one equal to a ping (vessel, time and position) is that ping. `FALSE`, the
#'   default, as before.
#' @param pair_away_m For `"dwell"` with `events`: an event pair makes no port call where a ping between its arrival
#'   and departure, not at its port, lies farther than this from the port's polygon (metres, in `crs_m`): the track
#'   shows the vessel left, so events between were lost and the pair spans a trip. The ping may be a good event's
#'   presence. `NULL` for no such test (as before ramb 2026.11.07). The default, 10 km, from fishycode plan 048 step 6:
#'   on STK's pairs of 2007-2016, the pairs a logbook's fishing day contradicts had such a ping in 65 % (changed
#'   calls) to 81 % (pairs spanning several calls), those a landing at the port confirms in 0.1-10 %.
#' @param window Optional `c(from, to)` (POSIXct): keep only the port calls that overlap `[from, to)`
#'   (`T_out >= from` and `T_in < to`; for `"dwell"`, once the port calls from pings and from events are merged).
#'   For processing a long series in chunks: pad each chunk's pings with every vessel's last ping before `from`
#'   and first ping after `to`, wherever they lie (a vessel silent through a whole chunk is then seen in port across
#'   it), so that both chunks see a port call over the edge between them, each as far as its pings reach, then bind
#'   the chunks' port calls with `ramb::rb_merge_port_calls()`, which joins the two pieces and numbers the port calls of the
#'   whole series. Until ramb 2026.11.01.1 this was `t_in_window`, which kept only the
#'   port calls that begin in the window: the chunk after the edge dropped the rest of a port call begun before it,
#'   so every port call over an edge was cut at the next chunk's first ping while the vessel was heard in port.
#' @param crs_m For `"dwell"`: a metric CRS for the buffers (EPSG code). The default, 3575, suits the North
#'   Atlantic.
#' @param ... For `"runs"`, `"jepol"`, `"cnr"` and `"iapesca"`: arguments of [os_create_voyages()] (such as
#'   `min_pings`, `max_dur_h`, `coast`, `cnr`, `iapesca`, or the ping columns `port_id` and `speed`).
#' @param vid,time,lon,lat The ping columns, if named otherwise.
#'
#' @return One row per port call: `vid`, `year` (of `T_in`; a port call is not cut at New Year), `port_call_id`
#'   (numbered per vessel by `T_in`, so a ping in a port call over New Year finds it by `vid` and `port_call_id`
#'   alone), `port_id`, `T_in`, `T_out`, `dur_h`, `rule`, `evidence`, `T_in_ping`, `T_out_ping`, `T_in_ev`, `T_out_ev`,
#'   `basis` (`"reconstructed"`). For the methods other than `"dwell"`, `rule` and `evidence` are the method,
#'   the event times are NA, and `edge` is `"start"` or `"end"` for a port call at the edge of a vessel's record (its
#'   outer end is the vessel's first or last ping), NA for one between two voyages. A data frame for data frame input,
#'   a lazy table for lazy input.
#' @family trip
#' @export
os_create_port_calls <- function(
  pings,
  ports = NULL,
  method = c("dwell", "runs", "jepol", "cnr", "iapesca"),
  min_dwell_h = 4,
  min_dwell = NULL,
  radius_in = 2000,
  events = NULL,
  presence = FALSE,
  pair_away_m = 10000,
  window = NULL,
  crs_m = 3575,
  speed_kn = NULL,
  speed_fallback_kn = 6,
  ...,
  vid = vid,
  time = time,
  lon = lon,
  lat = lat
) {
  method <- match.arg(method)
  if ("t_in_window" %in% ...names()) {
    stop(
      "`t_in_window` is now `window`, and keeps the port calls that overlap it, not only those that begin in it. ",
      "Bind the port calls of chunks with ramb::rb_merge_port_calls().",
      call. = FALSE
    )
  }
  window <- .os_check_window(window)
  if (!isFALSE(presence) && method != "dwell") {
    stop("`presence` is for method \"dwell\".", call. = FALSE)
  }
  .os_scope()
  if (method != "dwell") {
    v <- os_create_voyages(
      pings,
      method = method,
      ports = ports,
      by_year = FALSE,
      ...,
      vid = {{ vid }},
      time = {{ time }},
      lon = {{ lon }},
      lat = {{ lat }}
    )
    out <- .os_calls_between(
      v,
      pings,
      inherits(pings, "tbl_lazy"),
      rlang::as_name(rlang::enquo(vid)),
      rlang::as_name(rlang::enquo(time)),
      method,
      window
    )
    return(.os_as_given(out, pings))
  }
  if (...length() > 0) {
    stop(
      "The arguments in `...` are for the methods other than \"dwell\".",
      call. = FALSE
    )
  }
  .os_check_input(
    pings,
    "os_create_port_calls",
    cols = c(
      vid = rlang::as_name(rlang::enquo(vid)),
      time = rlang::as_name(rlang::enquo(time))
    ),
    also = c(
      lon = rlang::as_name(rlang::enquo(lon)),
      lat = rlang::as_name(rlang::enquo(lat))
    ),
    types = c(time = "time", lon = "numeric", lat = "numeric")
  )
  if (is.null(ports)) {
    stop("Method \"dwell\" needs `ports`: the port polygons.", call. = FALSE)
  }
  map <- .os_cols(
    vid = {{ vid }},
    time = {{ time }},
    lon = {{ lon }},
    lat = {{ lat }}
  )
  lazy_in <- inherits(pings, "tbl_lazy")
  con <- .os_con_for(pings, events)
  .os_extension(con, "spatial")
  p <- .os_as_lazy(.os_std_in(pings, map), con) |>
    dplyr::select(vid, time, lon, lat)

  # ports: polygon and the two buffers, as WKT, registered on the connection
  hb <- sf::st_make_valid(ports)
  r_out <- if ("radius_out" %in% names(hb)) {
    hb$radius_out
  } else {
    rep(radius_in, nrow(hb))
  }
  r_out <- pmax(dplyr::coalesce(r_out, radius_in), radius_in)
  buf <- function(d) {
    sf::st_transform(sf::st_buffer(sf::st_transform(hb, crs_m), d), 4326)
  }
  hw <- data.frame(
    port_id = hb$port_id,
    poly = sf::st_as_text(sf::st_geometry(hb), digits = 15),
    near_in = sf::st_as_text(sf::st_geometry(buf(radius_in)), digits = 15),
    near_out = sf::st_as_text(sf::st_geometry(buf(r_out)), digits = 15),
    poly_m = sf::st_as_text(
      sf::st_geometry(sf::st_transform(hb, crs_m)),
      digits = 15
    )
  )
  hw_nm <- .os_register(con, hw, "rb_hw_")
  hg <- if (is.null(min_dwell)) {
    data.frame(port_id = character(0), g_h = numeric(0))
  } else {
    data.frame(port_id = min_dwell$port_id, g_h = min_dwell$min_dwell_h)
  }
  hg_nm <- .os_register(con, hg, "rb_hg_")

  if (!is.null(events)) {
    if (!(is.data.frame(events) || inherits(events, "tbl_lazy"))) {
      stop("`events` must be a data frame or a lazy DuckDB table.", call. = FALSE)
    }
    miss <- setdiff(c("vid", "time", "io", "port_id", "good"), colnames(events))
    if (length(miss)) {
      rlang::abort(c(
        sprintf("`events` must have %s.", paste0("`", miss, "`", collapse = ", ")),
        i = if ("good" %in% miss) "`good` says which events may be trusted: it is added by `ramb::rb_flag_port_event()`."
      ))
    }
  }
  if (!(is.logical(presence) && length(presence) == 1 && !is.na(presence))) {
    stop("`presence` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.null(pair_away_m) && !(is.numeric(pair_away_m) && length(pair_away_m) == 1 && !is.na(pair_away_m) &&
                                 pair_away_m > 0)) {
    stop("`pair_away_m` must be NULL or a distance in metres above 0.", call. = FALSE)
  }
  if (presence) {
    if (is.null(events)) {
      stop("`presence = TRUE` takes the good events' positions: give `events`.", call. = FALSE)
    }
    miss <- setdiff(c("lon", "lat"), colnames(events))
    if (length(miss)) {
      rlang::abort(c(
        sprintf("With `presence = TRUE`, `events` must have %s.", paste0("`", miss, "`", collapse = ", ")),
        i = "An event's presence is its position at its time; an event with no position gives none."
      ))
    }
    # the good events with a position, as pings; UNION, so one equal to a ping is that ping
    pe <- .os_as_lazy(events, con) |>
      dplyr::filter(good, !is.na(lon), !is.na(lat), !is.na(time)) |>
      dplyr::transmute(vid, time, lon = as.numeric(lon), lat = as.numeric(lat))
    p <- dplyr::union(p, pe)
  }
  ev_sql <- if (is.null(events)) {
    "SELECT NULL::INTEGER AS vid, NULL::TIMESTAMP AS time, NULL::VARCHAR AS io, NULL::VARCHAR AS port_id, NULL::BOOLEAN AS good WHERE false"
  } else {
    .os_sql(
      .os_as_lazy(events, con) |> dplyr::select(vid, time, io, port_id, good)
    )
  }
  budget <- !is.null(speed_kn)
  if (
    budget &&
      !identical(speed_kn, "vessel") &&
      !(is.numeric(speed_kn) && length(speed_kn) == 1 && speed_kn > 0)
  ) {
    stop(
      "`speed_kn` must be NULL, a speed in knots or \"vessel\".",
      call. = FALSE
    )
  }
  valley <- identical(min_dwell_h, "valley")
  if (!valley && !(is.numeric(min_dwell_h) && length(min_dwell_h) == 1)) {
    stop(
      "`min_dwell_h` must be a number of hours or \"valley\".",
      call. = FALSE
    )
  }

  # The common part: the pings numbered per vessel, the ports, each ping's port, the runs inside a port, and for
  # the time budget the next ping of every ping and each vessel's speed.
  # metres from a ping to the port's polygon, in crs_m
  dm <- function(lon, lat) {
    sprintf(
      "ST_Distance(ST_Transform(ST_Point(%s, %s), 'EPSG:4326', 'EPSG:%d', always_xy := true), hm.gm)",
      lon,
      lat,
      as.integer(crs_m)
    )
  }
  spd_sql <- if (identical(speed_kn, "vessel")) {
    sprintf(
      ",
legs AS (SELECT vid, 2 * 6371000 * asin(sqrt(pow(sin(radians(lead(lat) OVER w - lat) / 2), 2) +
                cos(radians(lat)) * cos(radians(lead(lat) OVER w)) * pow(sin(radians(lead(lon) OVER w - lon) / 2), 2))) AS d,
                epoch(lead(time) OVER w - time) AS dt
         FROM seq WINDOW w AS (PARTITION BY vid ORDER BY rn)),
spd0 AS (SELECT vid, count(*) AS n, quantile_cont(d / dt * 3600 / 1852, 0.75) AS v FROM legs
         WHERE dt BETWEEN 60 AND 7200 AND d / dt * 3600 / 1852 BETWEEN 2 AND 30 GROUP BY vid),
spd AS (SELECT v0.vid, CASE WHEN spd0.n >= 20 THEN spd0.v ELSE %f END AS v
        FROM (SELECT DISTINCT vid FROM seq) v0 LEFT JOIN spd0 USING (vid))",
      speed_fallback_kn
    )
  } else if (budget) {
    sprintf(",\nspd AS (SELECT DISTINCT vid, %f AS v FROM seq)", speed_kn)
  } else {
    ""
  }
  pre <- sprintf(
    "
WITH seq AS (SELECT vid, time, lon, lat, row_number() OVER (PARTITION BY vid ORDER BY time, lon, lat) AS rn FROM (%s)),
harb AS (SELECT port_id AS pid, ST_GeomFromText(poly) AS g, ST_GeomFromText(near_in) AS gi, ST_GeomFromText(near_out) AS go,
                ST_GeomFromText(poly_m) AS gm FROM %s),
hg AS (SELECT port_id AS pid, g_h FROM %s),
c AS (SELECT s.vid, s.rn, s.time, h.pid, ST_Distance(ST_Point(s.lon, s.lat), h.g) AS d, ST_Intersects(h.gi, ST_Point(s.lon, s.lat)) AS near_in
      FROM seq s JOIN harb h ON ST_Intersects(h.go, ST_Point(s.lon, s.lat))),
tg AS (SELECT vid, rn, any_value(time) AS time,
              arg_min(pid, (CASE WHEN near_in THEN 0 ELSE 10 END) + d) AS h,
              min(d) = 0 AS inside,
              arg_min(near_in, (CASE WHEN near_in THEN 0 ELSE 10 END) + d) AS near_in
       FROM c GROUP BY vid, rn),
runs AS (SELECT vid, h, min(rn) AS s, max(rn) AS e, min(time) AS t_s, max(time) AS t_e
         FROM (SELECT *, rn - row_number() OVER (PARTITION BY vid, h ORDER BY rn) AS grp FROM tg WHERE inside)
         GROUP BY vid, h, grp),
a AS (SELECT *, lead(rn) OVER w AS nrn, lead(h) OVER w AS nh, lead(inside) OVER w AS nin, lead(time) OVER w AS nt
      FROM tg WINDOW w AS (PARTITION BY vid ORDER BY rn))%s",
    .os_sql(p),
    hw_nm,
    hg_nm,
    spd_sql
  )
  # the time-budget candidates: a silence that begins within radius_in of a port (gb), and a run inside a port with
  # the pings either side of it (sb); `gmin` keeps gb to the silences that can matter
  budget_ctes <- function(gmin) {
    sprintf(
      ",
sa AS (SELECT vid, rn, time, lon, lat, lead(rn) OVER w AS nrn, lead(time) OVER w AS nt, lead(lon) OVER w AS nlon,
              lead(lat) OVER w AS nlat FROM seq WINDOW w AS (PARTITION BY vid ORDER BY rn)),
gb AS (SELECT t.vid, t.h, t.rn AS s, sa.nrn AS e, t.time AS t_s, sa.nt AS t_e, epoch(sa.nt - t.time) / 3600 AS gap_h,
              %s AS d1, %s AS d2
       FROM tg t JOIN sa ON sa.vid = t.vid AND sa.rn = t.rn JOIN harb hm ON hm.pid = t.h
       WHERE t.near_in AND sa.nrn IS NOT NULL AND epoch(sa.nt - t.time) >= %f * 3600),
sb AS (SELECT r.vid, r.h, r.s, r.e, r.t_s, r.t_e, epoch(n.time - pv.time) / 3600 AS gap_h, %s AS d1, %s AS d2
       FROM runs r JOIN seq pv ON pv.vid = r.vid AND pv.rn = r.s - 1 JOIN seq n ON n.vid = r.vid AND n.rn = r.e + 1
       JOIN harb hm ON hm.pid = r.h)",
      dm("sa.lon", "sa.lat"),
      dm("sa.nlon", "sa.nlat"),
      gmin,
      dm("pv.lon", "pv.lat"),
      dm("n.lon", "n.lat")
    )
  }

  ## the minimum from the data -----------------------------------------------------------------------------------
  if (valley) {
    cand <- if (budget) {
      paste0(
        pre,
        budget_ctes(0.25),
        "
SELECT gb.gap_h - (gb.d1 + gb.d2) / (spd.v * 1852) AS h FROM gb JOIN spd USING (vid)
UNION ALL SELECT sb.gap_h - (sb.d1 + sb.d2) / (spd.v * 1852) FROM sb JOIN spd USING (vid)"
      )
    } else {
      paste0(
        pre,
        ",
sa AS (SELECT vid, rn, lead(time) OVER w AS nt FROM seq WINDOW w AS (PARTITION BY vid ORDER BY rn))
SELECT epoch(sa.nt - t.time) / 3600 AS h FROM tg t JOIN sa USING (vid, rn) WHERE t.near_in AND sa.nt IS NOT NULL
  AND epoch(sa.nt - t.time) >= 900
UNION ALL SELECT epoch(t_e - t_s) / 3600 FROM runs"
      )
    }
    min_dwell_h <- .os_dwell_valley(DBI::dbGetQuery(con, cand)$h)
    message(sprintf(
      "os_create_port_calls(): min_dwell_h from the data, %.2f h",
      min_dwell_h
    ))
  }
  G <- min_dwell_h
  gmin <- min(c(G, hg$g_h))

  budget_seeds <- if (budget) {
    sprintf(
      "
          UNION ALL
          SELECT gb.vid, gb.h, gb.s, gb.e, gb.t_s, gb.t_e, 'gap_budget' FROM gb JOIN spd USING (vid) LEFT JOIN hg ON hg.pid = gb.h
          WHERE gb.gap_h >= coalesce(hg.g_h, %1$f) AND gb.gap_h - (gb.d1 + gb.d2) / (spd.v * 1852) >= coalesce(hg.g_h, %1$f)
          UNION ALL
          SELECT sb.vid, sb.h, sb.s, sb.e, sb.t_s, sb.t_e, 'span_budget' FROM sb JOIN spd USING (vid) LEFT JOIN hg ON hg.pid = sb.h
          WHERE sb.gap_h - (sb.d1 + sb.d2) / (spd.v * 1852) >= coalesce(hg.g_h, %1$f)",
      G
    )
  } else {
    ""
  }
  # The pair guard (pair_away_m): a vessel's pairs never overlap (an arrival and the departure next to it), so each ping
  # goes to the pair it falls in by an ASOF join; a ping not at the pair's port and farther than pair_away_m from its
  # polygon drops the pair. Only pings at another port or at sea are measured.
  guard_sql <- if (is.null(events) || is.null(pair_away_m)) {
    ",\necall_kept AS (SELECT * FROM ecall)"
  } else {
    sprintf(
      ",
pa AS (SELECT s.vid, s.time, s.lon, s.lat, t.h FROM seq s LEFT JOIN tg t USING (vid, rn)),
ej AS (SELECT p.time, p.lon, p.lat, p.h, e.vid, e.t_in, e.t_out, e.pid
       FROM pa p ASOF JOIN (SELECT vid, pid, t_in, t_out FROM ecall) e ON e.vid = p.vid AND e.t_in < p.time),
away AS (SELECT DISTINCT ej.vid, ej.t_in FROM ej JOIN harb hm ON hm.pid = ej.pid
         WHERE ej.time < ej.t_out AND ej.h IS DISTINCT FROM ej.pid AND %s > %f),
ecall_kept AS (SELECT e.* FROM ecall e ANTI JOIN away a ON a.vid = e.vid AND a.t_in = e.t_in)",
      dm("ej.lon", "ej.lat"),
      pair_away_m
    )
  }
  sql <- sprintf(
    "%s%s,
seeds AS (SELECT vid, h, rn AS s, nrn AS e, time AS t_s, nt AS t_e,
                 CASE WHEN inside OR nin THEN 'tag_then_gap' ELSE 'gap_near_port' END AS rule
          FROM a LEFT JOIN hg ON hg.pid = a.h
          WHERE nrn = rn + 1 AND nh = h AND near_in AND epoch(nt - time) >= coalesce(hg.g_h, %f) * 3600
          UNION ALL
          SELECT vid, h, s, e, t_s, t_e, 'tag_span' FROM runs LEFT JOIN hg ON hg.pid = runs.h
          WHERE epoch(t_e - t_s) >= coalesce(hg.g_h, %f) * 3600%s),
ext AS (SELECT sd.vid, sd.h, least(sd.s, coalesce(r1.s, sd.s)) AS s, greatest(sd.e, coalesce(r2.e, sd.e)) AS e,
               least(sd.t_s, coalesce(r1.t_s, sd.t_s)) AS t_s, greatest(sd.t_e, coalesce(r2.t_e, sd.t_e)) AS t_e, sd.rule
        FROM seeds sd
        LEFT JOIN runs r1 ON r1.vid = sd.vid AND r1.h = sd.h AND sd.s BETWEEN r1.s AND r1.e
        LEFT JOIN runs r2 ON r2.vid = sd.vid AND r2.h = sd.h AND sd.e BETWEEN r2.s AND r2.e),
m AS (SELECT *, max(e) OVER (PARTITION BY vid, h ORDER BY s, e ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING) AS pe FROM ext),
g AS (SELECT *, sum(CASE WHEN pe IS NULL OR s > pe + 1 THEN 1 ELSE 0 END) OVER (PARTITION BY vid, h ORDER BY s, e) AS grp FROM m),
pcall AS (SELECT vid, h AS pid, min(t_s) AS t_in, max(t_e) AS t_out, list_distinct(list(rule)) AS rules FROM g GROUP BY vid, h, grp),
ev AS (SELECT vid, time, io, port_id AS hid_pid, good,
              lead(io) OVER w AS nio, lead(time) OVER w AS nt, lead(port_id) OVER w AS nh, lead(good) OVER w AS ngood
       FROM (%s) WINDOW w AS (PARTITION BY vid ORDER BY time, io)),
ecall AS (SELECT vid, hid_pid AS pid, time AS t_in, nt AS t_out, ['event_pair'] AS rules
          FROM ev LEFT JOIN hg ON hg.pid = ev.hid_pid
          WHERE io = 'I' AND nio = 'U' AND good AND ngood AND hid_pid IS NOT NULL AND nh = hid_pid
            AND epoch(nt - time) >= coalesce(hg.g_h, %f) * 3600)%s,
u AS (SELECT vid, pid, t_in, t_out, rules, 'ping' AS src FROM pcall
      UNION ALL SELECT vid, pid, t_in, t_out, rules, 'ev' FROM ecall_kept),
m2 AS (SELECT *, max(t_out) OVER (PARTITION BY vid, pid ORDER BY t_in, t_out ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING) AS pe FROM u),
g2 AS (SELECT *, sum(CASE WHEN pe IS NULL OR t_in > pe THEN 1 ELSE 0 END) OVER (PARTITION BY vid, pid ORDER BY t_in, t_out) AS grp FROM m2),
st AS (SELECT vid, pid, min(t_in) AS T_in, max(t_out) AS T_out,
              min(t_in) FILTER (src = 'ping') AS T_in_ping, max(t_out) FILTER (src = 'ping') AS T_out_ping,
              min(t_in) FILTER (src = 'ev') AS T_in_ev, max(t_out) FILTER (src = 'ev') AS T_out_ev,
              array_to_string(list_sort(list_distinct(flatten(list(rules)))), '+') AS evidence
       FROM g2 GROUP BY vid, pid, grp)
SELECT vid, year(T_in) AS year, row_number() OVER (PARTITION BY vid ORDER BY T_in, T_out, pid) AS port_call_id,
       pid AS port_id, T_in, T_out, epoch(T_out - T_in) / 3600 AS dur_h,
       CASE WHEN evidence LIKE '%%event_pair%%' THEN 'event_pair'
            WHEN evidence LIKE '%%tag_then_gap%%' THEN 'tag_then_gap'
            WHEN evidence LIKE '%%tag_span%%' THEN 'tag_span'
            WHEN evidence LIKE '%%gap_near_port%%' THEN 'gap_near_port'
            WHEN evidence LIKE '%%span_budget%%' THEN 'span_budget'
            ELSE 'gap_budget' END AS rule,
       evidence, T_in_ping, T_out_ping, T_in_ev, T_out_ev, 'reconstructed' AS basis
FROM st WHERE %s",
    pre,
    if (budget) budget_ctes(gmin) else "",
    G,
    G,
    budget_seeds,
    ev_sql,
    G,
    guard_sql,
    .os_window_sql(window)
  )
  out <- dplyr::tbl(con, dplyr::sql(sql))
  out <- dplyr::rename(
    out,
    !!!stats::setNames(rlang::syms("vid"), map[["vid"]])
  )
  if (lazy_in) {
    attr(out, "min_dwell_h") <- G
    return(.os_hold(out))
  }
  out <- dplyr::collect(out)
  .os_unregister(con)
  out <- .os_as_given(out[order(out[[map[["vid"]]]], out$T_in), ], pings)
  attr(out, "min_dwell_h") <- G
  out
}

# The minimum dwell from the data (os_create_port_calls(min_dwell_h = "valley"), plan 031 ask a): the low point of
# the density of the candidate calls' log lengths, between the highest mode under an hour (pass-throughs, the
# vessel's runs through a port) and the highest mode over two hours (calls). Lengths under a minute or over two
# days are left out.
.os_dwell_valley <- function(h) {
  h <- h[is.finite(h) & h > 1 / 60 & h < 48]
  if (length(h) < 50) {
    stop(
      "Too few candidate port calls to set `min_dwell_h` from the data; give it in hours.",
      call. = FALSE
    )
  }
  d <- stats::density(log10(h), n = 512)
  lo <- which(d$x < 0)
  hi <- which(d$x > log10(2))
  if (!length(lo) || !length(hi)) {
    stop(
      "The candidate port calls show no pass-throughs and calls to set `min_dwell_h` between; give it in hours.",
      call. = FALSE
    )
  }
  p1 <- lo[which.max(d$y[lo])]
  p2 <- hi[which.max(d$y[hi])]
  b <- seq(p1, p2)
  10^d$x[b[which.min(d$y[b])]]
}

# The port calls of the methods other than "dwell": the time in port between two voyages of one vessel, at the port
# the first ended in, else the port the second began from. The voyages come uncut at New Year (by_year = FALSE), so a
# New Year cut at sea makes no port call; a split at sea or a silence with no port at either side makes none either,
# since neither voyage end has a port. A vessel's opening and closing stays in port are port calls too (`edge`), and a
# port call spans its own pings, strictly between the voyages (an edge call's outer end is itself a ping): until
# 2026.11.24 it ran from the voyage before's last ping to the voyage after's first, and there were no edge calls.
.os_calls_between <- function(voyages, pings, lazy_in, vid_name, time_name, method, window) {
  con <- .os_con_for(voyages)
  v <- .os_as_lazy(voyages, con) |>
    dplyr::rename(!!!stats::setNames(rlang::syms(vid_name), "vid"))
  p <- .os_as_lazy(pings, con) |>
    dplyr::rename(!!!stats::setNames(
      rlang::syms(c(vid_name, time_name)),
      c("vid", "time")
    ))
  win <- .os_window_sql(window)
  sql <- sprintf(
    "
WITH v AS (SELECT vid, T1, T2, port_from, port_to FROM (%s)),
w AS (SELECT vid, T2 AS T_in, lead(T1) OVER o AS T_out, coalesce(port_to, lead(port_from) OVER o) AS port_id,
             NULL::VARCHAR AS edge
      FROM v WINDOW o AS (PARTITION BY vid ORDER BY T1, T2)),
pe AS (SELECT vid, min(time) AS t_first, max(time) AS t_last FROM (%s) GROUP BY vid),
e0 AS (SELECT v.vid, pe.t_first AS T_in, v.T1 AS T_out, v.port_from AS port_id, 'start' AS edge
       FROM v JOIN pe USING (vid) QUALIFY row_number() OVER (PARTITION BY v.vid ORDER BY v.T1, v.T2) = 1),
e1 AS (SELECT v.vid, v.T2 AS T_in, pe.t_last AS T_out, v.port_to AS port_id, 'end' AS edge
       FROM v JOIN pe USING (vid) QUALIFY row_number() OVER (PARTITION BY v.vid ORDER BY v.T2 DESC, v.T1 DESC) = 1),
u0 AS (SELECT * FROM w UNION ALL SELECT * FROM e0 WHERE T_in < T_out UNION ALL SELECT * FROM e1 WHERE T_in < T_out),
-- each port call's own pings, strictly between the voyages (an edge call's outer end is itself a ping)
u AS (SELECT c.vid, c.port_id, c.edge, min(p.time) AS T_in, max(p.time) AS T_out
      FROM u0 c JOIN (%s) p ON p.vid = c.vid
       AND (p.time > c.T_in OR (c.edge = 'start' AND p.time = c.T_in))
       AND (p.time < c.T_out OR (c.edge = 'end' AND p.time = c.T_out))
      WHERE c.T_out IS NOT NULL GROUP BY c.vid, c.port_id, c.edge, c.T_in, c.T_out),
c AS (SELECT * FROM u WHERE T_out IS NOT NULL AND port_id IS NOT NULL AND %s)
SELECT vid, year(T_in) AS year, row_number() OVER (PARTITION BY vid ORDER BY T_in, T_out, port_id) AS port_call_id,
       port_id, T_in, T_out, epoch(T_out - T_in) / 3600 AS dur_h, '%s' AS rule, '%s' AS evidence,
       T_in AS T_in_ping, T_out AS T_out_ping, NULL::TIMESTAMP AS T_in_ev, NULL::TIMESTAMP AS T_out_ev,
       'reconstructed' AS basis, edge
FROM c",
    .os_sql(v),
    .os_sql(p),
    .os_sql(p),
    win,
    method,
    method
  )
  out <- dplyr::tbl(con, dplyr::sql(sql)) |>
    dplyr::rename(!!!stats::setNames(rlang::syms("vid"), vid_name))
  if (lazy_in) {
    return(.os_hold(out))
  }
  out <- dplyr::collect(out)
  .os_unregister(con)
  out[order(out[[vid_name]], out$T_in), ]
}

# The window of os_create_port_calls(): NULL, or two date-times in order; a Date is taken as midnight UTC.
.os_check_window <- function(window) {
  if (is.null(window)) {
    return(NULL)
  }
  if (inherits(window, "Date")) {
    window <- as.POSIXct(format(window), tz = "UTC")
  }
  if (
    !inherits(window, "POSIXct") ||
      length(window) != 2 ||
      anyNA(window) ||
      window[1] >= window[2]
  ) {
    stop(
      "`window` must be NULL or two date-times, `c(from, to)`, with `from` before `to`.",
      call. = FALSE
    )
  }
  window
}

# The port calls that overlap the window [from, to): a call that ends at `from` touches the chunk before and is kept,
# so that ramb::rb_merge_port_calls() can join it to the piece that chunk saw.
.os_window_sql <- function(window) {
  if (is.null(window)) {
    return("TRUE")
  }
  sprintf(
    "T_out >= TIMESTAMP '%s' AND T_in < TIMESTAMP '%s'",
    format(window[1], "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    format(window[2], "%Y-%m-%d %H:%M:%S", tz = "UTC")
  )
}

# A Suggests package, checked where a function needs it.
.os_need <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(
      sprintf(
        "This function needs the package '%s'. Install it with install.packages(\"%s\").",
        pkg,
        pkg
      ),
      call. = FALSE
    )
  }
}

# glue::glue() for SQL templates, in base R: each {expr} is evaluated in the caller's frame.
.os_fill <- function(template, env = parent.frame()) {
  m <- gregexpr("\\{[^{}]+\\}", template)
  regmatches(template, m) <- list(vapply(
    regmatches(template, m)[[1]],
    function(e) {
      paste(
        as.character(eval(parse(text = substr(e, 2, nchar(e) - 1)), env)),
        collapse = ""
      )
    },
    character(1)
  ))
  template
}

# A ping takes the port call it falls in; inside two overlapping port calls, the earlier one. Rows are numbered
# within the vessel (rb_rid), not over the whole table: an unpartitioned row_number() runs on one core, which made
# the step 5.5 times slower than the code it replaced (fishycode TODO 48).
# With `port_call_id` in the port calls, the ping also takes the port call's key, so a ping can carry the key and
# not the port call's values (fishycode plan 014).
.os_ping_call_sql <- function(p_sql, s_sql, by_year, call_id = FALSE) {
  sprintf(
    "
WITH p AS (SELECT *, row_number() OVER (PARTITION BY vid ORDER BY time) AS rb_rid FROM (%s)),
s AS (SELECT vid, port_id, T_in, T_out, %s AS port_call_id FROM (%s)),
ps AS (SELECT p.*, s.port_id AS port_call_port_id, s.port_call_id FROM p LEFT JOIN s ON s.vid = p.vid AND p.time BETWEEN s.T_in AND s.T_out
       QUALIFY row_number() OVER (PARTITION BY p.vid, p.rb_rid ORDER BY s.T_in NULLS LAST, s.port_id) = 1)
SELECT *, %s AS rb_year FROM ps",
    p_sql,
    if (call_id) "port_call_id" else "NULL::BIGINT",
    s_sql,
    if (by_year) "year(time)" else "0"
  )
}

# The voyage key: one column that names a voyage across vessels and years (asked for by ogis's track viewer,
# 2026-11-18, where trip 1 of `dansk` was twelve vessels' first trip). "<vid>:<year>:<voyage_id>", or
# "<vid>:<voyage_id>" for voyages numbered over the vessel's whole series (no `year` column); NA where a part is. A
# whole number is written without decimals whatever its type, so a vessel id read as a double (an MMSI from a csv)
# gives the key that an integer or a string of its digits gives. A data frame's key is made in R, a lazy table's in
# SQL; test-trip.R pins the two to each other.
.os_key_text <- function(x) {
  if (is.factor(x)) {
    x <- as.character(x)
  }
  out <- as.character(x)
  if (is.double(x) && !inherits(x, "integer64")) {
    w <- is.finite(x) & x == trunc(x) & abs(x) < 2^53
    out[w] <- sprintf("%.0f", x[w] + 0) # + 0: no "-0"
  }
  out
}

.os_key_sql <- function(col, proto) {
  q <- sprintf('"%s"', col)
  if (is.character(proto) || is.factor(proto)) {
    return(sprintf("CAST(%s AS VARCHAR)", q))
  }
  d <- sprintf("CAST(%s AS DOUBLE)", q)
  sprintf(
    "CASE WHEN isfinite(%1$s) AND %1$s = trunc(%1$s) AND abs(%1$s) < 9007199254740992
     THEN CAST(CAST(%2$s AS BIGINT) AS VARCHAR) ELSE CAST(%2$s AS VARCHAR) END",
    d,
    q
  )
}

# A table that rides beside the pings (port calls, voyages) holds the vessel under the caller's name, as the function
# that made it gave it back (`os_create_port_calls(vid = boat)` gives `boat`); here it takes the grammar's `vid`, as
# the pings do. One made under `vid` is used as it is. Until 2026.11.18 the joins read `vid` whatever the caller
# named it, and a call with `vid = boat` failed in the SQL binder.
.os_side_in <- function(x, map) {
  have <- if (inherits(x, "tbl_lazy")) colnames(x) else names(x)
  if (!map[["vid"]] %in% have) {
    return(x)
  }
  .os_std_in(x, map["vid"])
}

# `x` (voyages: `vid` named `vid`, `voyage_id`, and `year` where they are numbered per year) with `voyage_key` after
# `voyage_id`; a key already there is kept.
.os_with_voyage_key <- function(x, vid = "vid") {
  have <- if (inherits(x, "tbl_lazy")) colnames(x) else names(x)
  if ("voyage_key" %in% have) {
    return(x)
  }
  parts <- c(vid, intersect("year", have), "voyage_id")
  if (inherits(x, "tbl_lazy")) {
    proto <- dplyr::collect(utils::head(
      dplyr::select(x, dplyr::all_of(parts)),
      0
    ))
    sql <- paste(
      vapply(parts, function(p) .os_key_sql(p, proto[[p]]), ""),
      collapse = " || ':' || "
    )
    x <- dplyr::mutate(x, voyage_key = dplyr::sql(sql))
  } else {
    txt <- lapply(parts, function(p) .os_key_text(x[[p]]))
    key <- do.call(paste, c(txt, sep = ":"))
    key[Reduce(`|`, lapply(txt, is.na))] <- NA_character_
    x$voyage_key <- key
  }
  dplyr::relocate(x, "voyage_key", .after = "voyage_id")
}

#' Create voyages from vessel tracks
#'
#' A step-1 builder of the fishing-activity flow. Five methods, one result: a table of voyages. Voyages and port
#' calls ([os_create_port_calls()]) are the two halves of a track, and the two functions share their methods.
#'
#' * `"dwell"`: a voyage is the sea between two port calls (see [os_create_port_calls()]). A ping takes the port
#'   call it falls in (the earlier of two that overlap), the track is cut into runs of port call and sea, and each
#'   sea run becomes a voyage, from the port of the port call before it to the port of the port call after it.
#'   A port call cuts the track whether or not a ping falls in it (until 2026.11.06.3 one that no ping fell in cut
#'   nothing, and the sea on either side of it made one voyage).
#' * `"runs"`: as `"dwell"`, but every run of pings tagged in a port (`os_flag_ping_polygon()`) is a port call,
#'   however short.
#' * `"jepol"`: WKSSFGEO's rule (`define_trips_pol()`; see References): a voyage runs from the
#'   first ping at sea to the first ping back in port; voyages of `min_dur_h` hours or less are dropped, and
#'   with `split` one over `max_dur_h` hours is cut at its longest gap between pings, repeatedly. A long
#'   voyage with no inside gap of three minutes or more is left whole.
#' * `"cnr"`: CNR-IRBIM's trip finder (`create_fishing_trip()`, Galdelli et al. 2019), recoded in C++. A
#'   vessel is in port after `n_in` (4) consecutive pings in a port at a mean recorded speed under `speed_kn`
#'   (6 knots). A voyage runs from the last such ping (or the next, if it is over `shift_h`, 1 hour, later) to
#'   the ping before the `n_in`-th port ping of the next such run; a silence of over `gap_h` (4) hours at sea
#'   also ends it, and the next starts at the ping after the silence. Two voyages split by a silence with no
#'   port at either side of it are joined, unless both ends lie in the `coast` zone and together they last
#'   `join_h` (24) hours or more. A voyage end with no port then gets one from `ports`: after a silence that
#'   was not joined, the closest port; otherwise the port at the voyage's other end if it is among the
#'   `n_closest` (5) closest within `reach_m` (50 km), else the closest of those; with none in reach, NA. The
#'   port of an end is the port whose polygon, grown by `buffer_deg` (0.001 degrees, planar, as the R code),
#'   holds its ping, read `n_in - 1` pings before a voyage's first ping and at its last. A vessel (with
#'   `by_year`, a vessel-year) of fewer than `min_n` (10) pings has no voyages, and a missing speed never
#'   counts as slow. The `cnr` argument sets these values. Distances to the ports are on the WGS84 ellipsoid
#'   (`distance = "geodesic"`, the default, as the R code measured them; needs lwgeom) or on ramb's sphere
#'   (`"sphere"`, needs s2), whatever [sf::sf_use_s2()] says; the port buffer and the coastal zone are
#'   tested on the plane, as in the R code.
#' * `"iapesca"`: Ifremer's `Detect_FishingTrips()` with harbours (iapesca 1.3, J. Rodriguez; see References),
#'   recoded as one DuckDB query, so a lazy table (parquet) never comes into memory. A ping at the same time as an
#'   earlier one is dropped, wherever it is; then, up to `max_pass` (5) times, the first of each run of pings
#'   reached at `max_speed_kn` (25) or more. A ping is in port within a buffer of the `ports` polygons: 1 % of the
#'   track's bounding-box diagonal, at least `buf_start_m` (2,000 m) and at most `buf_max_m` (2 nautical miles);
#'   with no ping in port the buffer grows by a fifth of its way to `buf_max_m`, rounded to the metre, until a ping
#'   is in port or it reaches `buf_max_m`. A voyage runs from a ping outside the buffer to the last before a stop:
#'   a run in port of over `min_stop_s` (1 hour), timed from the last ping at sea before it to the first after it.
#'   The pings in port during a shorter stop belong to the voyage and count in `n_pings`; those in port between
#'   voyages belong to none. The geometry is planar in `crs_m` (default: the UTM zone of the pings' mean
#'   position), as the R code is with projected input (with longitude and latitude it works on s2's sphere); the
#'   buffer is GEOS's (30 segments a quarter circle), as sf's. Added here: `port_from` and `port_to`, the nearest
#'   port of the in-port ping before and after the voyage. The `iapesca` argument sets these values.
#'
#' @param pings A data frame or lazy DuckDB table with `vid` and `time`, and `port_id` for `"runs"`,
#'   `"jepol"` and `"cnr"` (NA at sea). For `"cnr"` also `speed` (knots, recorded) and `lon`, `lat`; for
#'   `"iapesca"`, `lon` and `lat`.
#' @param port_calls For `"dwell"`: port calls as returned by [os_create_port_calls()]: `vid`, `port_id`, `T_in`,
#'   `T_out`.
#' @param method `"dwell"`, `"runs"`, `"jepol"`, `"cnr"` or `"iapesca"`.
#' @param min_pings Voyages with fewer pings are dropped. They are numbered before they are dropped, so the
#'   numbers have gaps where a short voyage was. Default 10 for `"dwell"`, 1 (none dropped) otherwise.
#' @param by_year Cut voyages at New Year and number them per vessel and year.
#' @param short_pings For `"dwell"` and `"runs"`: a voyage of fewer pings is labelled `short` (with
#'   `min_pings = 1` it is kept, labelled, rather than dropped; fishycode plan 031 §4.7).
#' @param min_dur_h,max_dur_h,split For `"jepol"`: the minimum and maximum voyage length in hours, and whether
#'   longer voyages are split.
#' @param ports For `"cnr"`: an sf object of port polygons with `port_id`, for the ports of the voyage
#'   ends (see above). Without it an end takes its ping's `port_id`, and an end with none stays NA. For
#'   `"iapesca"`, required: the harbours whose buffer marks the pings in port.
#' @param coast For `"cnr"`: an sf object of the coastal zone (CNR's: the 3 nautical-mile band closed to
#'   towed gears), for the joining of voyages split by a silence. Without it they are always joined.
#' @param cnr For `"cnr"`: a list of settings, any of `n_in`, `speed_kn`, `gap_h`, `shift_h`, `join_h`,
#'   `min_n`, `buffer_deg`, `reach_m`, `n_closest`, `distance`; those left out keep CNR's values (above).
#' @param iapesca For `"iapesca"`: a list of settings, any of `max_speed_kn`, `max_pass`, `buf_start_m`,
#'   `buf_max_m`, `min_stop_s`, `crs_m` (an EPSG code of a projected CRS in metres); those left out keep
#'   iapesca's values (above).
#' @param batch_pings For `"jepol"` and `"cnr"` on a lazy table: about this many pings are collected at a
#'   time, whole vessels.
#' @param vid,time,port_id,speed,lon,lat The ping columns, if named otherwise. `port_calls` hold the vessel under
#'   the pings' name for it, as [os_create_port_calls()] gives them back, or under `vid`.
#'
#' @return One row per voyage: `vid`, `year` (with `by_year`), `voyage_id`, `voyage_key`, `T1`, `T2`, `port_from`,
#'   `port_to`, `n_pings`; for `"dwell"` and `"runs"` also `open_start` and `open_end` (no port call before or
#'   after the voyage: the start or end of the vessel's series, or a New Year cut with `by_year`) and `short`
#'   (fewer than `short_pings` pings). A data frame for data frame input, a lazy table for lazy input.
#'
#'   `voyage_id` numbers a vessel's voyages from 1 (with `by_year`, each year from 1), so it names a voyage only
#'   together with `vid` (and `year`). `voyage_key` names it alone, across vessels and years: the text
#'   `"<vid>:<year>:<voyage_id>"`, or `"<vid>:<voyage_id>"` without `by_year` (`"EX_1:2013:4"`). A whole number is
#'   written without decimals whatever its type, so a vessel id read as a double gives the key its integer gives.
#'   The key is as stable as `voyage_id`: the same pings and settings give the same keys.
#'
#' @references For `"jepol"`: `define_trips_pol()` in `R/define_trips_pol.R` of the ICES Workshop WKSSFGEO,
#'   <https://github.com/ices-eg/WKSSFGEO> (MIT), which reads the harbour flag `SI_HARB` of `vmstools`' tacsat
#'   format (Hintzen, N.T., Bastardie, F., Beare, D., et al. (2012). "VMStools: Open-source software for the
#'   processing, analysis and visualisation of fisheries logbook and VMS data". Fisheries Research, 115–116,
#'   31–43. \doi{10.1016/j.fishres.2011.11.007}). Until 2026.10.28.1 this page called it the ICES VMS data call's
#'   rule; the data call's repository has never held it.
#'
#'   For `"cnr"`: Galdelli, A., Mancini, A., Tassetti, A.N., et al. (2019). "A cloud computing architecture to
#'   map trawling activities using positioning data". Proceedings of the ASME 2019 IDETC/CIE, Volume 9.
#'   \doi{10.1115/DETC2019-97779}; the R code (R4AIS v1.0.1, \doi{10.5281/zenodo.4757505}) by A. Galdelli,
#'   J. Pulcinella, A.N. Tassetti and E. Armelloni (CNR-IRBIM), GNU GPL 3, in the WKSSFGEO repository's
#'   `dev_branch`.
#'
#'   For `"iapesca"`: Rodriguez, J. iapesca: interpretation of fishing paths from vessels' geo-spatial data,
#'   R package (Ifremer), <https://gitlab.ifremer.fr/iapesca/r-packages_iapesca> (version 1.3, CC BY 4.0), whose
#'   trips the method gives on CNR-IRBIM's Italian tracks. Until 2026.10.28 it followed version 0.1 (2022, GPL), the
#'   GitHub copy its author has since deprecated (<https://github.com/JulRodrigu/r-packages_iapesca>), where a stop
#'   was timed from the previous voyage end, a ping was a duplicate when its time and its place had each occurred
#'   before, and the buffer grew by 1 % of the diagonal, at most five times (fishycode decision 232). iapesca names
#'   a trip by its last fix's date; here voyages are numbered.
#' @family trip
#' @export
os_create_voyages <- function(
  pings,
  port_calls = NULL,
  method = c("dwell", "runs", "jepol", "cnr", "iapesca"),
  min_pings = NULL,
  by_year = TRUE,
  short_pings = 10,
  min_dur_h = 0.5,
  max_dur_h = 72,
  split = TRUE,
  ports = NULL,
  coast = NULL,
  cnr = list(),
  iapesca = list(),
  batch_pings = 5e7,
  vid = vid,
  time = time,
  port_id = port_id,
  speed = speed,
  lon = lon,
  lat = lat
) {
  method <- match.arg(method)
  .os_check_input(
    pings,
    "os_create_voyages",
    cols = c(
      vid = rlang::as_name(rlang::enquo(vid)),
      time = rlang::as_name(rlang::enquo(time))
    ),
    also = switch(
      method,
      cnr = c(
        port_id = rlang::as_name(rlang::enquo(port_id)),
        speed = rlang::as_name(rlang::enquo(speed)),
        lon = rlang::as_name(rlang::enquo(lon)),
        lat = rlang::as_name(rlang::enquo(lat))
      ),
      iapesca = c(
        lon = rlang::as_name(rlang::enquo(lon)),
        lat = rlang::as_name(rlang::enquo(lat))
      ),
      character(0)
    ),
    types = c(time = "time")
  )
  if (is.null(min_pings)) {
    min_pings <- if (method == "dwell") 10 else 1
  }
  if (method == "dwell" && is.null(port_calls)) {
    stop("Method \"dwell\" needs `port_calls`.", call. = FALSE)
  }
  if (method == "iapesca" && is.null(ports)) {
    stop(
      "Method \"iapesca\" needs `ports`: the harbour polygons.",
      call. = FALSE
    )
  }
  map <- .os_cols(
    vid = {{ vid }},
    time = {{ time }},
    port_id = {{ port_id }},
    speed = {{ speed }},
    lon = {{ lon }},
    lat = {{ lat }}
  )
  use <- switch(
    method,
    dwell = c("vid", "time"),
    cnr = names(map),
    iapesca = c("vid", "time", "lon", "lat"),
    c("vid", "time", "port_id")
  )
  lazy_in <- inherits(pings, "tbl_lazy")
  con <- .os_con_for(pings, port_calls)
  .os_scope()
  # every method's voyages take their key here; a lazy result keeps what the call registered, a data frame's comes
  # back as the pings came in
  done <- function(out) {
    out <- .os_with_voyage_key(out, map[["vid"]])
    if (lazy_in) .os_hold(out) else .os_as_given(out, pings)
  }
  p <- .os_as_lazy(.os_std_in(pings, map[use]), con)
  if (method == "cnr") {
    return(done(.os_cut_cnr(
      p,
      con,
      lazy_in,
      map,
      min_pings,
      by_year,
      ports,
      coast,
      cnr,
      batch_pings
    )))
  }
  if (method == "iapesca") {
    return(done(.os_cut_iapesca(
      p,
      con,
      lazy_in,
      map,
      min_pings,
      by_year,
      ports,
      iapesca
    )))
  }
  if (method == "jepol") {
    return(done(.os_cut_jepol(
      p,
      con,
      lazy_in,
      map,
      min_pings,
      by_year,
      min_dur_h,
      max_dur_h,
      split,
      batch_pings
    )))
  }
  ps <- if (method == "dwell") {
    # Every port call cuts the track, whether or not a ping falls in it: its start and end join the pings as the
    # call's own rows (rb_rid 0), so a call no ping falls in still parts the sea before it from the sea after it,
    # and a call over New Year gives the next year's first voyage its port. They count in no voyage's n_pings.
    p_sql <- .os_sql(dplyr::select(p, vid, time))
    c_sql <- .os_sql(.os_as_lazy(.os_side_in(port_calls, map), con))
    yr <- function(t) if (by_year) sprintf("year(%s)", t) else "0"
    sprintf(
      "SELECT * FROM (%s)
UNION ALL BY NAME
SELECT vid, T_in AS time, 0::BIGINT AS rb_rid, port_id AS port_call_port_id, NULL::BIGINT AS port_call_id,
       %s AS rb_year
FROM (%s) WHERE T_in IS NOT NULL AND vid IN (SELECT vid FROM (%s))
UNION ALL BY NAME
SELECT vid, T_out AS time, 0::BIGINT AS rb_rid, port_id AS port_call_port_id, NULL::BIGINT AS port_call_id,
       %s AS rb_year
FROM (%s) WHERE T_out IS NOT NULL AND vid IN (SELECT vid FROM (%s))",
      .os_ping_call_sql(p_sql, c_sql, by_year),
      yr("T_in"),
      c_sql,
      p_sql,
      yr("T_out"),
      c_sql,
      p_sql
    )
  } else {
    sprintf(
      "SELECT *, port_id AS port_call_port_id, row_number() OVER (PARTITION BY vid ORDER BY time) AS rb_rid, %s AS rb_year FROM (%s)",
      if (by_year) "year(time)" else "0",
      .os_sql(dplyr::select(p, vid, time, port_id))
    )
  }
  sql <- sprintf(
    "
WITH ps AS (%s),
f AS (SELECT *, port_call_port_id IS NOT NULL AS in_h,
             lag(port_call_port_id IS NOT NULL) OVER w AS p_in, lag(port_call_port_id) OVER w AS p_h
      FROM ps WINDOW w AS (PARTITION BY vid, rb_year ORDER BY time, rb_rid, port_call_port_id)),
r AS (SELECT *, sum(CASE WHEN p_in IS NULL OR in_h <> p_in OR (in_h AND port_call_port_id <> p_h) THEN 1 ELSE 0 END)
                  OVER (PARTITION BY vid, rb_year ORDER BY time, rb_rid, port_call_port_id) AS run_id FROM f),
runs AS (SELECT vid, rb_year, run_id, bool_or(in_h) AS in_h, min(port_call_port_id) AS h,
                min(time) AS T1, max(time) AS T2, count(*) AS n_pings
         FROM r GROUP BY vid, rb_year, run_id),
t AS (SELECT *, CASE WHEN NOT in_h THEN lag(h) OVER w END AS port_from,
                CASE WHEN NOT in_h THEN lead(h) OVER w END AS port_to
      FROM runs WINDOW w AS (PARTITION BY vid, rb_year ORDER BY T1, run_id)),
v AS (SELECT *, row_number() OVER (PARTITION BY vid, rb_year ORDER BY T1, run_id) AS voyage_id FROM t WHERE NOT in_h)
SELECT vid, %s voyage_id, T1, T2, port_from, port_to, n_pings,
       port_from IS NULL AS open_start, port_to IS NULL AS open_end, n_pings < %d AS short
FROM v WHERE n_pings >= %d",
    ps,
    if (by_year) "rb_year AS year," else "",
    as.integer(short_pings),
    as.integer(min_pings)
  )
  out <- dplyr::tbl(con, dplyr::sql(sql))
  out <- dplyr::rename(
    out,
    !!!stats::setNames(rlang::syms("vid"), map[["vid"]])
  )
  if (lazy_in) {
    return(done(out))
  }
  out <- dplyr::collect(out)
  .os_unregister(con)
  done(out[order(out[[map[["vid"]]]], out$T1), ])
}

#' Add the voyage to each ping
#'
#' Step 1 of the fishing-activity flow. Each ping gets the voyage it lies in (`voyage_id` and `voyage_key`, from
#' [os_create_voyages()]) and, with `port_calls`, the port of the port call it lies in (`port_call_port_id`) and
#' the port call's key (`port_call_id`, where the port calls carry one). `trip_id` is
#' the voyage and `trip_basis` says where it came from: `"reconstructed"` (the track) or `"none"`. Declared
#' trips (logbook) and landings as sources, and their reconciliation with the voyages, are planned for a
#' later release. No ping is dropped.
#'
#' `voyage_id` and `trip_id` number a vessel's voyages (with `by_year`, per year), so the pings of twelve vessels'
#' first voyages share `voyage_id` 1; `voyage_key` is one value per voyage across vessels and years, for grouping
#' or choosing the pings of a voyage by one column. Voyages without a `voyage_key` (made before ramb 2026.11.18) get
#' it here, as [os_create_voyages()] makes it.
#'
#' @param pings A data frame or lazy DuckDB table with `vid` and `time`.
#' @param voyages Voyages from [os_create_voyages()].
#' @param port_calls Optional port calls from [os_create_port_calls()].
#' @param by_year As in [os_create_voyages()]: voyages are numbered per vessel and year.
#' @param vid,time The ping columns, if named otherwise. `voyages` and `port_calls` hold the vessel under the pings'
#'   name for it, as [os_create_voyages()] and [os_create_port_calls()] give them back, or under `vid`.
#'
#' @return `pings` with `port_call_port_id` and `port_call_id` (with `port_calls`; the key where the port calls
#'   carry one), `voyage_id`, `voyage_key`, `trip_id`, `trip_basis`: the same type, the same rows.
#' @family trip
#' @export
os_add_voyages <- function(
  pings,
  voyages,
  port_calls = NULL,
  by_year = TRUE,
  vid = vid,
  time = time
) {
  .os_check_input(
    pings,
    "os_add_voyages",
    cols = c(
      vid = rlang::as_name(rlang::enquo(vid)),
      time = rlang::as_name(rlang::enquo(time))
    ),
    types = c(time = "time")
  )
  map <- .os_cols(vid = {{ vid }}, time = {{ time }})
  lazy_in <- inherits(pings, "tbl_lazy")
  con <- .os_con_for(pings, voyages, port_calls)
  .os_scope()
  given <- pings
  if (!lazy_in) {
    pings <- .os_with_rid(pings)
  }
  p <- .os_as_lazy(.os_std_in(pings, map), con)
  # voyages made before 2026.11.18 have no key: it is made here as os_create_voyages() makes it
  v <- .os_with_voyage_key(.os_as_lazy(.os_side_in(voyages, map), con))
  base <- if (is.null(port_calls)) {
    sprintf(
      "SELECT *, NULL::VARCHAR AS port_call_port_id, NULL::BIGINT AS port_call_id, row_number() OVER (PARTITION BY vid ORDER BY time) AS rb_rid FROM (%s)",
      .os_sql(p)
    )
  } else {
    sprintf(
      "SELECT * EXCLUDE (rb_year) FROM (%s)",
      .os_ping_call_sql(
        .os_sql(p),
        .os_sql(.os_as_lazy(.os_side_in(port_calls, map), con)),
        by_year,
        call_id = "port_call_id" %in% colnames(port_calls)
      )
    )
  }
  yr <- if (by_year) "AND v.year = year(b.time)" else ""
  drop <- if (is.null(port_calls)) {
    "rb_rid, port_call_port_id, port_call_id"
  } else if ("port_call_id" %in% colnames(port_calls)) {
    "rb_rid"
  } else {
    "rb_rid, port_call_id"
  }
  sql <- sprintf(
    "
WITH b AS (%s), v AS (%s),
j AS (SELECT b.*, v.voyage_id, v.voyage_key FROM b LEFT JOIN v ON v.vid = b.vid %s AND b.time BETWEEN v.T1 AND v.T2
      QUALIFY row_number() OVER (PARTITION BY b.vid, b.rb_rid ORDER BY v.T1) = 1)
SELECT * EXCLUDE (%s), voyage_id AS trip_id,
       CASE WHEN voyage_id IS NULL THEN 'none' ELSE 'reconstructed' END AS trip_basis
FROM j",
    base,
    .os_sql(v),
    yr,
    drop
  )
  out <- dplyr::tbl(con, dplyr::sql(sql))
  out <- .os_std_out(out, map)
  if (lazy_in) {
    return(.os_hold(out))
  }
  out <- dplyr::collect(out)
  .os_unregister(con)
  .os_as_given(out, given, ".rb_rid")
}

# The "jepol" method: whole vessels collected in batches, the rule applied in R (R/trip_jepol.R), one voyage table.
.os_cut_jepol <- function(
  p,
  con,
  lazy_in,
  map,
  min_pings,
  by_year,
  min_dur_h,
  max_dur_h,
  split,
  batch_pings
) {
  vn <- p |> dplyr::count(vid) |> dplyr::collect()
  vn <- vn[order(-vn$n, vn$vid), ]
  batches <- if (nrow(vn)) {
    split(vn$vid, cumsum(as.numeric(vn$n)) %/% batch_pings)
  } else {
    list()
  }
  out <- lapply(batches, function(g) {
    d <- p |>
      dplyr::filter(vid %in% g) |>
      dplyr::select(vid, time, port_id) |>
      dplyr::collect()
    d <- d[order(d$vid, d$time, d$port_id, na.last = TRUE), ]
    yr <- as.numeric(format(d$time, "%Y", tz = "UTC"))
    k <- if (by_year) paste(d$vid, yr) else as.character(d$vid)
    tr <- .os_jepol(
      k,
      d$time,
      as.integer(!is.na(d$port_id)),
      min_dur_h,
      max_dur_h,
      split
    )$trip
    f <- which(!is.na(tr))
    if (!length(f)) {
      return(NULL)
    }
    id <- unique(tr[f])
    first <- f[match(id, tr[f])]
    last <- f[length(f) + 1 - match(id, rev(tr[f]))]
    prev <- pmax(first - 1, 1)
    v <- data.frame(
      vid = d$vid[first],
      year = yr[first],
      k = k[first],
      T1 = d$time[first],
      T2 = d$time[last],
      port_from = ifelse(first > 1 & k[prev] == k[first], d$port_id[prev], NA),
      port_to = d$port_id[last],
      n_pings = as.numeric(tabulate(match(tr[f], id), length(id)))
    )
    v <- v[order(v$k, v$T1), ]
    v$voyage_id <- as.numeric(stats::ave(
      seq_len(nrow(v)),
      v$k,
      FUN = seq_along
    ))
    v
  })
  out <- do.call(rbind, out)
  if (is.null(out)) {
    out <- data.frame(
      vid = vector(class(vn$vid)[1], 0),
      year = numeric(0),
      k = character(0),
      T1 = as.POSIXct(character(0), tz = "UTC"),
      T2 = as.POSIXct(character(0), tz = "UTC"),
      port_from = character(0),
      port_to = character(0),
      n_pings = numeric(0),
      voyage_id = numeric(0)
    )
  }
  out <- out[
    out$n_pings >= min_pings,
    c(
      "vid",
      if (by_year) "year",
      "voyage_id",
      "T1",
      "T2",
      "port_from",
      "port_to",
      "n_pings"
    )
  ]
  out <- out[order(out$vid, out$T1), ]
  rownames(out) <- NULL
  names(out)[1] <- map[["vid"]]
  if (!lazy_in) {
    .os_unregister(con)
    return(out)
  }
  dplyr::tbl(con, .os_register(con, out, "rb_voy_"))
}

.os_jepol_one <- function(time, inh, vessel, min_dur, max_dur, split) {
  o <- order(time)
  time <- time[o]
  inh <- inh[o]
  n <- length(time)
  none <- data.frame(
    vessel = vessel[0],
    trip = character(0),
    depart = time[0],
    arrival = time[0]
  )
  intv <- c(NA, diff(as.numeric(time)) / 60)
  ev <- c(0, -diff(inh))
  ev[ev == -1] <- 2
  if (all(ev == 0)) {
    return(none)
  }
  if (inh[1] == 0) {
    ev[1] <- 1
  }
  # At sea at the end: the last ping closes the open voyage. When the last ping is
  # itself a departure it opens and closes a voyage of one ping, which has no
  # duration and is dropped below. define_trips() overwrote that departure with an
  # arrival, one departure short: the departures were then recycled into a voyage
  # spanning the port call, or the vessel stopped the run ("do not pair") - 791 of
  # 34,286 Icelandic vessel-years with port pings end this way.
  last_dep <- inh[n] == 0 && ev[n] == 1
  if (inh[n] == 0 && !last_dep) {
    ev[n] <- 2
  }
  dep <- time[ev == 1]
  arr <- time[ev == 2]
  if (last_dep) {
    arr <- c(arr, time[n])
  }
  if (length(dep) != length(arr) && length(dep) != 1 && length(arr) != 1) {
    stop(
      "Vessel ",
      vessel,
      ": departures and arrivals do not pair.",
      call. = FALSE
    )
  }
  k <- max(length(dep), length(arr))
  tr <- data.frame(
    trip = paste0(vessel, "_", seq_len(k)),
    depart = rep(dep, length.out = k),
    arrival = rep(arr, length.out = k)
  )
  hrs <- function(d) as.numeric(difftime(d$arrival, d$depart, units = "hours"))
  tr$dur <- hrs(tr)
  tr <- tr[tr$dur > min_dur, ]
  if (split) {
    while (any(tr$dur > max_dur)) {
      i <- which(tr$dur > max_dur)[1]
      inside <- which(time > tr$depart[i] & time < tr$arrival[i])
      j <- if (length(inside) >= 2) which.max(intv[inside]) else integer(0)
      g <- inside[j]
      if (!length(g) || intv[g] < 3) {
        tr$dur[i] <- max_dur
        next
      }
      nt <- data.frame(
        trip = paste(tr$trip[i], 1:2, sep = "_"),
        depart = c(tr$depart[i], time[g]),
        arrival = c(time[g - 1], tr$arrival[i])
      )
      nt$dur <- hrs(nt)
      tr <- rbind(tr[-i, ], nt)
      tr <- tr[order(tr$depart), ]
    }
  }
  tr <- tr[tr$dur != 0, ]
  if (any(tr$dur < min_dur)) {
    warning(
      "At least one trip for ",
      vessel,
      " is shorter than min_dur after splitting at max_dur.",
      call. = FALSE
    )
    tr <- tr[tr$dur > min_dur, ]
  }
  if (!nrow(tr)) {
    return(none)
  }
  data.frame(
    vessel = vessel,
    trip = tr$trip,
    depart = tr$depart,
    arrival = tr$arrival
  )
}

# All vessels: the trip table, and each ping's trip (NA outside every trip).
.os_jepol <- function(
  vessel,
  time,
  inh,
  min_dur = 0.5,
  max_dur = 72,
  split = TRUE
) {
  if (anyNA(inh)) {
    stop("The in-harbour indicator has missing values.", call. = FALSE)
  }
  idx <- split(seq_along(vessel), vessel)
  trips <- do.call(
    rbind,
    lapply(names(idx), function(v) {
      ii <- idx[[v]]
      .os_jepol_one(time[ii], inh[ii], vessel[ii][1], min_dur, max_dur, split)
    })
  )
  trip <- rep(NA_character_, length(vessel))
  if (!is.null(trips) && nrow(trips)) {
    for (v in unique(trips$vessel)) {
      ii <- idx[[as.character(v)]]
      t <- trips[trips$vessel == v, ]
      k <- findInterval(as.numeric(time[ii]), as.numeric(t$depart))
      hit <- k > 0 & as.numeric(time[ii]) <= as.numeric(t$arrival[pmax(k, 1)])
      trip[ii[hit]] <- t$trip[k[hit]]
    }
  }
  list(trips = trips, trip = trip)
}

# The "cnr" method: CNR-IRBIM's trip finder, create_fishing_trip() of the WKSSFGEO dev_branch
# (R-dev/galdelli_pulcinella_tassetti/R/global_functions.R, GNU GPL 3), recoded. The state machine and the
# joining of voyages split by a silence run in C++ (src/trip_cnr.cpp, which says what is kept and what is not);
# the ports of the voyage ends are found here, for the few pings that need one, with sf. Whole vessels are
# collected in batches, as for "jepol", so a lazy table (parquet) never comes into memory whole.
.os_cnr_par <- function(cnr) {
  par <- list(
    n_in = 4L,
    speed_kn = 6,
    gap_h = 4,
    shift_h = 1,
    join_h = 24,
    min_n = 10L,
    buffer_deg = 0.001,
    reach_m = 50000,
    n_closest = 5L,
    distance = "geodesic"
  )
  bad <- setdiff(names(cnr), names(par))
  if (length(bad)) {
    stop(
      "Unknown `cnr` setting(s): ",
      paste(bad, collapse = ", "),
      ". Known: ",
      paste(names(par), collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  par <- utils::modifyList(par, cnr)
  if (!par$distance %in% c("geodesic", "sphere")) {
    stop("`cnr$distance` must be \"geodesic\" or \"sphere\".", call. = FALSE)
  }
  par
}

# The ports, once per call: the polygons on WGS84 for distances, and grown by buffer_deg on the plane (the R
# code's st_buffer(ports, 0.001) with s2 off) for the port of an end ping.
.os_cnr_ports <- function(ports, buffer_deg) {
  if (is.null(ports)) {
    return(NULL)
  }
  if (
    !"port_id" %in% names(ports) ||
      anyDuplicated(ports$port_id) ||
      anyNA(ports$port_id)
  ) {
    stop(
      "`ports` must have a column `port_id`, unique and never NA.",
      call. = FALSE
    )
  }
  hb <- sf::st_transform(ports, 4326)
  buf <- sf::st_buffer(sf::st_set_crs(sf::st_geometry(hb), NA), buffer_deg)
  list(
    id = hb$port_id,
    geom = sf::st_geometry(hb),
    buf = buf,
    area = as.numeric(sf::st_area(buf))
  )
}

.os_cnr_points <- function(lon, lat, crs = NA) {
  sf::st_as_sf(
    data.frame(lon = lon, lat = lat),
    coords = c("lon", "lat"),
    crs = crs
  )
}

# The port (index into P) whose grown polygon holds each point; of two, the smaller, as ramb::rb_flag_ping_port().
.os_cnr_in_port <- function(lon, lat, P) {
  out <- rep(NA_integer_, length(lon))
  if (!length(lon)) {
    return(out)
  }
  hit <- sf::st_intersects(.os_cnr_points(lon, lat), P$buf)
  n <- lengths(hit)
  one <- n == 1
  out[one] <- unlist(hit[one])
  for (i in which(n > 1)) {
    h <- hit[[i]]
    out[i] <- h[order(P$area[h], h)][1]
  }
  out
}

# 1 where the point lies in the coastal zone (any of its polygons), else 0; planar, as the R code.
.os_cnr_coast <- function(lon, lat, coast) {
  if (is.null(coast) || !length(lon)) {
    return(rep(0L, length(lon)))
  }
  pmin(lengths(sf::st_intersects(.os_cnr_points(lon, lat), coast)), 1L)
}

# Distances in metres from the points to every port, a matrix per call: "geodesic" on the WGS84 ellipsoid
# (lwgeom, which sf::st_distance() calls with s2 off, as the R code ran), "sphere" on ramb's sphere (s2,
# R = 6,371,000 m). Neither reads sf_use_s2(), so the result never depends on that global option, and neither
# is planar: st_nearest_feature() is, with s2 off.
.os_cnr_dist_fun <- function(P, distance) {
  if (distance == "geodesic") {
    .os_need("lwgeom")
    return(function(pts) unclass(lwgeom::st_geod_distance(pts, P$geom)))
  }
  .os_need("s2")
  g <- s2::as_s2_geography(P$geom)
  function(pts) {
    s2::s2_distance_matrix(
      s2::as_s2_geography(sf::st_geometry(pts)),
      g,
      radius = 6371000
    )
  }
}

# The port (index into P) for open voyage ends. recover = FALSE: closest_port(), the closest at any distance.
# recover = TRUE: closest_port_recovery(), of the n_closest ports within reach_m the reference port (the voyage's
# other end) if it is among them, else the closest; NA with none in reach. Ties go to the earlier port.
.os_cnr_closest <- function(lon, lat, ref, P, par, recover) {
  out <- rep(NA_integer_, length(lon))
  if (!length(lon)) {
    return(out)
  }
  dist <- .os_cnr_dist_fun(P, par$distance)
  pts <- .os_cnr_points(lon, lat, 4326)
  for (i in split(seq_along(lon), (seq_along(lon) - 1) %/% 500)) {
    m <- dist(pts[i, ])
    out[i] <- vapply(
      seq_along(i),
      function(r) {
        d <- m[r, ]
        if (!recover) {
          return(which.min(d))
        }
        cand <- which(d <= par$reach_m)
        if (!length(cand)) {
          return(NA_integer_)
        }
        cand <- cand[order(d[cand], cand)][seq_len(min(
          par$n_closest,
          length(cand)
        ))]
        rf <- ref[i[r]]
        if (!is.na(rf) && rf %in% cand) rf else cand[1]
      },
      integer(1)
    )
  }
  out
}

# One batch of whole vessels, sorted by vid, time, lon, lat; k the vessel (or vessel-year) of each ping.
.os_cnr_batch <- function(d, k, P, coast, par) {
  grp <- match(k, unique(k))
  tm <- as.numeric(d$time)
  cut <- cnr_cut_cpp(
    grp,
    tm,
    as.integer(!is.na(d$port_id)),
    as.numeric(d$speed),
    as.integer(par$n_in),
    par$speed_kn,
    par$gap_h,
    par$shift_h,
    as.integer(par$min_n)
  )
  if (!nrow(cut)) {
    return(NULL)
  }
  # ports are coded as indices into lv, the ports' ids (or, without ports, the pings' own)
  lv <- if (is.null(P)) unique(d$port_id[!is.na(d$port_id)]) else P$id
  end_port <- function(i) {
    out <- rep(NA_integer_, length(i))
    ok <- !is.na(i)
    out[ok] <- if (is.null(P)) {
      match(d$port_id[i[ok]], lv)
    } else {
      .os_cnr_in_port(d$lon[i[ok]], d$lat[i[ok]], P)
    }
    out
  }
  j <- cnr_join_cpp(
    cut$grp,
    cut$s,
    cut$e,
    end_port(cut$dep),
    end_port(cut$e),
    .os_cnr_coast(d$lon[cut$s], d$lat[cut$s], coast),
    .os_cnr_coast(d$lon[cut$e], d$lat[cut$e], coast),
    tm,
    par$join_h
  )
  if (!is.null(P)) {
    # after a silence that was not joined, each side the closest port
    f <- which(j$from_closest)
    t <- which(j$to_closest)
    j$port_from[f] <- .os_cnr_closest(
      d$lon[j$s[f]],
      d$lat[j$s[f]],
      NULL,
      P,
      par,
      FALSE
    )
    j$port_to[t] <- .os_cnr_closest(
      d$lon[j$e[t]],
      d$lat[j$e[t]],
      NULL,
      P,
      par,
      FALSE
    )
    # then any end still open; each end's reference is the other end's port before this step
    f0 <- j$port_from
    t0 <- j$port_to
    f <- which(is.na(f0))
    t <- which(is.na(t0))
    j$port_from[f] <- .os_cnr_closest(
      d$lon[j$s[f]],
      d$lat[j$s[f]],
      t0[f],
      P,
      par,
      TRUE
    )
    j$port_to[t] <- .os_cnr_closest(
      d$lon[j$e[t]],
      d$lat[j$e[t]],
      f0[t],
      P,
      par,
      TRUE
    )
  }
  data.frame(
    vid = d$vid[j$s],
    year = as.numeric(format(d$time[j$s], "%Y", tz = "UTC")),
    k = k[j$s],
    T1 = d$time[j$s],
    T2 = d$time[j$e],
    port_from = lv[j$port_from],
    port_to = lv[j$port_to],
    n_pings = as.numeric(j$e - j$s + 1)
  )
}

.os_cut_cnr <- function(
  p,
  con,
  lazy_in,
  map,
  min_pings,
  by_year,
  ports,
  coast,
  cnr,
  batch_pings
) {
  par <- .os_cnr_par(cnr)
  P <- .os_cnr_ports(ports, par$buffer_deg)
  if (!is.null(coast)) {
    coast <- sf::st_set_crs(sf::st_geometry(sf::st_transform(coast, 4326)), NA)
  }
  vn <- p |> dplyr::count(vid) |> dplyr::collect()
  vn <- vn[order(-vn$n, vn$vid), ]
  batches <- if (nrow(vn)) {
    split(vn$vid, cumsum(as.numeric(vn$n)) %/% batch_pings)
  } else {
    list()
  }
  out <- lapply(batches, function(g) {
    d <- p |>
      dplyr::filter(vid %in% g) |>
      dplyr::select(vid, time, lon, lat, speed, port_id) |>
      dplyr::collect()
    d <- d[order(d$vid, d$time, d$lon, d$lat), ]
    k <- if (by_year) {
      paste(d$vid, format(d$time, "%Y", tz = "UTC"))
    } else {
      as.character(d$vid)
    }
    v <- .os_cnr_batch(d, k, P, coast, par)
    if (is.null(v)) {
      return(NULL)
    }
    v <- v[order(v$k, v$T1), ]
    v$voyage_id <- as.numeric(stats::ave(
      seq_len(nrow(v)),
      v$k,
      FUN = seq_along
    ))
    v
  })
  out <- do.call(rbind, out)
  if (is.null(out)) {
    pid <- dplyr::collect(utils::head(dplyr::select(p, port_id), 0))$port_id
    out <- data.frame(
      vid = vector(class(vn$vid)[1], 0),
      year = numeric(0),
      T1 = as.POSIXct(character(0), tz = "UTC"),
      T2 = as.POSIXct(character(0), tz = "UTC"),
      port_from = if (is.null(P)) pid else P$id[0],
      port_to = if (is.null(P)) pid else P$id[0],
      n_pings = numeric(0),
      voyage_id = numeric(0)
    )
  }
  out <- out[
    out$n_pings >= min_pings,
    c(
      "vid",
      if (by_year) "year",
      "voyage_id",
      "T1",
      "T2",
      "port_from",
      "port_to",
      "n_pings"
    )
  ]
  out <- out[order(out$vid, out$T1), ]
  rownames(out) <- NULL
  names(out)[1] <- map[["vid"]]
  if (!lazy_in) {
    .os_unregister(con)
    return(out)
  }
  dplyr::tbl(con, .os_register(con, out, "rb_voy_"))
}

# The "iapesca" method: Ifremer's Detect_FishingTrips() with harbours (iapesca 1.3, J. Rodriguez, CC BY 4.0), recoded
# as one DuckDB query; checked against the R function on the Italian tracks (data-raw/iapesca_trips.R). Planar in a
# metric CRS, as the R function is with projected input. Kept from the R function so the two agree:
# - a ping at the same time as an earlier one is dropped, wherever it is (CalcSpeed's duplicated(time));
# - the buffer's step is rounded half to even, as R's round().
# Not kept: the R function loops for ever when no ping is in port and the step rounds to 0 m (the start buffer within
# 2.5 m of the cap); here the buffer then stays at its start.
# "In port" is the exact distance to the nearest harbour, and only pings within 2 m inside a buffer's edge are
# tested against GEOS's 30-segment buffer polygon, which lies inside the exact offset, at most 1.3 m short at 3.7 km.
.os_iapesca_par <- function(iapesca) {
  par <- list(
    max_speed_kn = 25,
    max_pass = 5L,
    buf_start_m = 2000,
    buf_max_m = 2 / 0.539957 * 1000,
    min_stop_s = 3600,
    crs_m = NULL
  )
  bad <- setdiff(names(iapesca), names(par))
  if (length(bad)) {
    stop(
      "Unknown `iapesca` setting(s): ",
      paste(bad, collapse = ", "),
      ". Known: ",
      paste(names(par), collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  utils::modifyList(par, iapesca)
}

.os_cut_iapesca <- function(
  p,
  con,
  lazy_in,
  map,
  min_pings,
  by_year,
  ports,
  iapesca
) {
  par <- .os_iapesca_par(iapesca)
  .os_extension(con, "spatial")
  if (is.null(par$crs_m)) {
    m <- dplyr::collect(dplyr::summarise(
      p,
      lon = mean(lon, na.rm = TRUE),
      lat = mean(lat, na.rm = TRUE)
    ))
    par$crs_m <- (if (isTRUE(m$lat >= 0)) 32600 else 32700) +
      floor((m$lon + 180) / 6) +
      1
  }
  crs <- sf::st_crs(par$crs_m)
  if (
    is.na(crs) ||
      isTRUE(crs$IsGeographic) ||
      !identical(crs$units_gdal, "metre")
  ) {
    stop("`iapesca$crs_m` must be a projected CRS in metres.", call. = FALSE)
  }
  if (!"port_id" %in% names(ports) || anyNA(ports$port_id)) {
    stop("`ports` must have a column `port_id`, never NA.", call. = FALSE)
  }
  hb <- sf::st_transform(ports, crs)
  hw <- data.frame(port_id = hb$port_id)
  hw$rb_wkb <- lapply(sf::st_as_binary(sf::st_geometry(hb)), as.raw)
  hn <- .os_register(con, hw, "rb_harb_")
  reach <- par$buf_max_m + 2
  # pings meet harbours in square cells twice the reach wide, an equality join: on the boxes alone, four ranges,
  # DuckDB compared every ping with every harbour (2026.10.28: 26 s at 60 vessels, 85 GiB spilled at 1,800)
  gs <- sprintf("%.17g", 2 * reach)
  # a voyage's times and ports come from its own pings' rows (window functions, ordered first/last), not from joins
  # back to them: DuckDB guessed those tables empty and joined two of them on the vessel alone (336 million rows at 60
  # vessels)
  pass <- function(k) {
    sprintf(
      "
f%1$d AS (SELECT *, CASE WHEN pt IS NULL THEN FALSE WHEN t = pt THEN nm > 0 ELSE nm / ((t - pt) / 3600) >= %3$.17g END AS fl
          FROM (SELECT *, lag(t) OVER w AS pt, sqrt((x - lag(x) OVER w)^2 + (y - lag(y) OVER w)^2) * 0.539957 / 1000 AS nm
                FROM c%2$d WINDOW w AS (PARTITION BY vid, rb_year ORDER BY rid))),
c%1$d AS (SELECT vid, rb_year, time, rid, t, x, y FROM (
          SELECT *, fl AND NOT coalesce(lag(fl) OVER (PARTITION BY vid, rb_year ORDER BY rid), FALSE) AS dr FROM f%1$d)
          WHERE NOT dr)",
      k,
      k - 1,
      par$max_speed_kn
    )
  }
  np <- as.integer(par$max_pass)
  bs <- sprintf("%.17g", par$buf_start_m)
  bm <- sprintf("%.17g", par$buf_max_m)
  ms <- sprintf("%.17g", par$min_stop_s)
  sql <- paste0(
    "
WITH
s0 AS MATERIALIZED (
  SELECT vid, rb_year, time, epoch(time) AS t, ST_X(g) AS x, ST_Y(g) AS y,
         row_number() OVER (PARTITION BY vid, rb_year ORDER BY time, lon, lat) AS rid
  FROM (SELECT vid, time, lon, lat, ",
    if (by_year) "year(time)" else "0",
    " AS rb_year,
               ST_Transform(ST_Point(lon, lat), 'EPSG:4326', '",
    crs$input,
    "', always_xy := true) AS g
        FROM (",
    .os_sql(dplyr::select(p, vid, time, lon, lat)),
    ")
        WHERE time IS NOT NULL AND lon IS NOT NULL AND lat IS NOT NULL)),
c0 AS (SELECT vid, rb_year, time, rid, t, x, y FROM (
       SELECT *, row_number() OVER (PARTITION BY vid, rb_year, t ORDER BY rid) > 1 AS dup FROM s0)
       WHERE NOT dup),",
    if (np > 0) {
      paste0(paste(sapply(seq_len(np), pass), collapse = ","), ",")
    } else {
      ""
    },
    "
cl AS MATERIALIZED (SELECT *, row_number() OVER (PARTITION BY vid, rb_year ORDER BY rid) AS r,
                           floor(x / ",
    gs,
    ")::BIGINT AS cx, floor(y / ",
    gs,
    ")::BIGINT AS cy FROM c",
    np,
    "),
v AS MATERIALIZED (SELECT vid, rb_year, b0, round_even((",
    bm,
    " - b0) / 5, 0) AS step
                   FROM (SELECT vid, rb_year, least(greatest(",
    bs,
    ", sqrt((max(x) - min(x))^2 + (max(y) - min(y))^2) / 100), ",
    bm,
    ") AS b0
                         FROM cl GROUP BY ALL)),
h AS MATERIALIZED (SELECT port_id, g AS geom, ST_XMin(g) - ",
    reach,
    " AS x0, ST_XMax(g) + ",
    reach,
    " AS x1,
                          ST_YMin(g) - ",
    reach,
    " AS y0, ST_YMax(g) + ",
    reach,
    " AS y1
                   FROM (SELECT port_id, ST_GeomFromWKB(rb_wkb) AS g FROM ",
    hn,
    ")),
hc0 AS (SELECT *, unnest(range(floor(x0 / ",
    gs,
    ")::BIGINT, floor(x1 / ",
    gs,
    ")::BIGINT + 1)) AS cx FROM h),
hc AS MATERIALIZED (SELECT *, unnest(range(floor(y0 / ",
    gs,
    ")::BIGINT, floor(y1 / ",
    gs,
    ")::BIGINT + 1)) AS cy FROM hc0),
d AS MATERIALIZED (SELECT c.vid, c.rb_year, c.r, c.x, c.y, c.cx, c.cy, min(ST_Distance(h.geom, ST_Point(c.x, c.y))) AS d,
                          arg_min(h.port_id, ST_Distance(h.geom, ST_Point(c.x, c.y))) AS pid
                   FROM cl c JOIN hc h ON h.cx = c.cx AND h.cy = c.cy
                   WHERE c.x BETWEEN h.x0 AND h.x1 AND c.y BETWEEN h.y0 AND h.y1 GROUP BY ALL),
b AS MATERIALIZED (SELECT vid, rb_year, j, step, least(b0 + j * step, ",
    bm,
    ") AS bj
                   FROM v, range(0, 9) r(j)),
x AS MATERIALIZED (SELECT d.vid, d.rb_year, d.r, b.j, bool_or(ST_Intersects(ST_Buffer(h.geom, b.bj, 30), ST_Point(d.x, d.y))) AS inp
                   FROM d JOIN b ON b.vid = d.vid AND b.rb_year = d.rb_year AND d.d > b.bj - 2 AND d.d <= b.bj
                   JOIN hc h ON h.cx = d.cx AND h.cy = d.cy
                   WHERE d.x BETWEEN h.x0 AND h.x1 AND d.y BETWEEN h.y0 AND h.y1 GROUP BY ALL),
jj AS (SELECT b.vid, b.rb_year, b.j, b.step, b.bj, coalesce(bool_or(d.d <= b.bj - 2 OR coalesce(x.inp, FALSE)), FALSE) AS any_in
       FROM b LEFT JOIN d ON d.vid = b.vid AND d.rb_year = b.rb_year
       LEFT JOIN x ON x.vid = d.vid AND x.rb_year = d.rb_year AND x.r = d.r AND x.j = b.j GROUP BY ALL),
js AS (SELECT vid, rb_year, min(j) AS js FROM jj WHERE any_in OR step = 0 OR bj >= ",
    bm,
    " GROUP BY ALL),
fb AS (SELECT jj.vid, jj.rb_year, jj.j, jj.bj AS buf FROM jj JOIN js ON js.vid = jj.vid AND js.rb_year = jj.rb_year AND js.js = jj.j),
ip AS MATERIALIZED (SELECT c.vid, c.rb_year, c.r, c.t, c.time, d.pid,
                           coalesce(d.d <= fb.buf - 2, FALSE) OR coalesce(x.inp, FALSE) AS inp
                    FROM cl c JOIN fb ON fb.vid = c.vid AND fb.rb_year = c.rb_year
                    LEFT JOIN d ON d.vid = c.vid AND d.rb_year = c.rb_year AND d.r = c.r
                    LEFT JOIN x ON x.vid = c.vid AND x.rb_year = c.rb_year AND x.r = c.r AND x.j = fb.j),
rn AS (SELECT vid, rb_year, min(r) AS r1, max(r) AS r2, min(t) AS t1, max(t) AS t2, min(time) AS time1, max(time) AS time2,
              first(CASE WHEN pin THEN ppid END ORDER BY r) AS port_from, last(CASE WHEN nin THEN npid END ORDER BY r) AS port_to
       FROM (SELECT *, r - row_number() OVER (PARTITION BY vid, rb_year ORDER BY r) AS g
             FROM (SELECT *, lag(inp) OVER w AS pin, lag(pid) OVER w AS ppid, lead(inp) OVER w AS nin, lead(pid) OVER w AS npid
                   FROM ip WINDOW w AS (PARTITION BY vid, rb_year ORDER BY r))
             WHERE NOT inp)
       GROUP BY vid, rb_year, g),
lab AS (SELECT *, sum(CASE WHEN pt IS NULL OR t1 - pt > ",
    ms,
    " THEN 1 ELSE 0 END)
                    OVER (PARTITION BY vid, rb_year ORDER BY r1 ROWS UNBOUNDED PRECEDING) AS lab
        FROM (SELECT *, lag(t2) OVER (PARTITION BY vid, rb_year ORDER BY r1) AS pt FROM rn)),
tr AS (SELECT vid, rb_year, lab, min(r1) AS r1, max(r2) AS r2, min(time1) AS time1, max(time2) AS time2,
              first(port_from ORDER BY r1) AS port_from, last(port_to ORDER BY r1) AS port_to
       FROM lab GROUP BY vid, rb_year, lab)
SELECT vid, ",
    if (by_year) "rb_year AS year, " else "",
    "CAST(lab AS DOUBLE) AS voyage_id, time1 AS T1, time2 AS T2, port_from, port_to, CAST(r2 - r1 + 1 AS DOUBLE) AS n_pings
FROM tr
WHERE r2 - r1 + 1 >= ",
    as.integer(min_pings)
  )
  out <- dplyr::tbl(con, dplyr::sql(sql))
  out <- dplyr::rename(
    out,
    !!!stats::setNames(rlang::syms("vid"), map[["vid"]])
  )
  if (lazy_in) {
    return(out)
  }
  out <- as.data.frame(dplyr::collect(out))
  .os_unregister(con)
  out <- out[order(out[[map[["vid"]]]], out$T1), ]
  rownames(out) <- NULL
  out
}

