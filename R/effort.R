# Effort on the pings, and in cells of any grid ---------------------------------------------------------------------------
#
# The user (2026-11-26): effort "can be expressed in various ways" (the soak, the time spent in a cell while hauling,
# per hook or pot, a swept area), and the grid general: "the C-square is just one dimension". Two functions, one
# principle each:
#   * os_add_effort(): a quantity stays with the stretch of track it stands for. Each ping of a unit (a haul) takes the
#     stretch to the unit's next ping, its time and its ground; a unit's own quantities (its soak, its catch, its hooks)
#     are shared over its pings by ground (where the gear lay) or by time (how long it was worked).
#   * os_create_cells(): a cell takes the share of each stretch that lies in it. The stretch is cut into pieces no
#     longer than half the finest cell asked for, so no value is given: the pieces follow from the cells.
# Every quantity in is a quantity out, or the functions stop (kg in = kg out, fishycode's rule).
#
# Drafted with AI assistance (Claude), 2026-11-26; not yet checked by a person.

#' The effort each ping stands for, and a unit's quantities shared over its pings
#'
#' Each ping of a unit (a haul, a tow, a set) takes the stretch of track from it to the unit's next ping: its time
#' (`effort_s`) and its ground (`effort_m`). A unit's effort is the sum over its pings, from its first ping to its
#' last; the last ping's stretch is empty. Nothing is reached across units, so steaming between two hauls is in
#' neither.
#'
#' A unit's own quantities, given in `units` (the soak of a passive gear, its catch, its hooks or pots, a gear's
#' width times its ground), are shared over its pings: by ground (`share = "ground"`, a ping's share is its stretch's
#' ground over the unit's; where the gear lay, the right share for a gear's soak or amount) or by time (`"time"`; how
#' long it was worked there). A unit that covers no ground (or takes no time) shares equally over its pings, and says
#' so. Summed over a unit's pings, each quantity is the unit's, or the function stops.
#'
#' Which unit a ping is in is the caller's: hauls from [os_add_hauls()] (whose pauses are in their haul), tows, or a
#' witness's events. Effort on a passive gear's hauls is hauling time; its soak comes from [os_add_setting()].
#'
#' @param pings A data frame of pings with `vid`, `time` (`POSIXct`), `lon` and `lat` (decimal degrees) and the `unit`
#'   column. Rows may be in any order; pings at the same time keep the order given.
#' @param unit The name of the column that names each ping's unit within its vessel, `NA` in none: by default
#'   `"haul_id"`, from [os_add_hauls()].
#' @param units A data frame, a row a unit: the `unit` column (and `vid` where units are numbered by vessel), and
#'   the quantities to share, each a numeric column (`NA` where the unit's is not known); or `NULL`, none.
#' @param share How the quantities are shared over a unit's pings: `"ground"` (the default) or `"time"`.
#'
#' @return `pings`, rows in the order given, with columns added: `effort_s` (seconds), `effort_m` (metres),
#'   `effort_basis` (`"the stretch to the unit's next ping"`, `"the unit's last ping"`, `"no position"`, `"not in a
#'   unit"`), and with `units`, each quantity's share and `share_basis` (`"ground"`, `"time"`, `"equal: the unit
#'   covers no ground"`, `"equal: the unit takes no time"`, `"no position"`, `"no row in units"`, or `NA` in no
#'   unit). The attribute `"units"` is a data frame, a row a unit met in `pings`: `vid`, the `unit` column, `pings`,
#'   `effort_s`, `effort_m`, `share_basis` and each quantity as given; `"unplaced"`, the rows of `units` no ping is
#'   in.
#' @seealso [os_create_cells()], which lays the stretches on cells.
#' @export
#'
#' @examples
#' # two hauls, a ping a minute; the second laid twice as long
#' pings <- data.frame(vid = 1, time = as.POSIXct("2026-01-01", tz = "UTC") + 60 * 0:9,
#'                     lon = 13.5 + 0.001 * c(0:4, 10, 10, 10:12), lat = 43.6,
#'                     haul_id = c(1, 1, 1, 1, 1, NA, 2, 2, 2, 2))
#' e <- os_add_effort(pings, units = data.frame(vid = 1, haul_id = 1:2, soak_h = c(24, 48)))
#' e[c("haul_id", "effort_s", "effort_m", "soak_h", "share_basis")]
#' attr(e, "units")
os_add_effort <- function(pings, unit = "haul_id", units = NULL, share = c("ground", "time")) {
  share <- match.arg(share)
  if (!is.data.frame(pings)) stop("`pings` must be a data frame.", call. = FALSE)
  if (!(is.character(unit) && length(unit) == 1 && !is.na(unit))) stop("`unit` must be one column name.", call. = FALSE)
  miss <- setdiff(c("vid", "time", "lon", "lat", unit), names(pings))
  if (length(miss)) stop("`pings` lacks: ", paste(miss, collapse = ", "), call. = FALSE)
  if (!inherits(pings$time, "POSIXct")) stop("`pings$time` must be POSIXct.", call. = FALSE)
  if (anyNA(pings$vid)) stop("`pings$vid` has NA: every ping needs its vessel.", call. = FALSE)
  qs <- character(0)
  if (!is.null(units)) {
    if (!is.data.frame(units) || !unit %in% names(units)) {
      stop("`units` must be a data frame with the `", unit, "` column.", call. = FALSE)
    }
    key <- intersect(c("vid", unit), names(units))
    qs <- setdiff(names(units), key)
    if (!length(qs)) stop("`units` has no quantity to share: numeric columns besides ", paste(key, collapse = " and "), ".", call. = FALSE)
    bad <- qs[!vapply(units[qs], is.numeric, NA)]
    if (length(bad)) stop("`units` columns to share must be numeric: ", paste(bad, collapse = ", "), call. = FALSE)
  }
  made <- c("effort_s", "effort_m", "effort_basis", qs, if (length(qs)) "share_basis")
  if (length(clash <- intersect(made, names(pings)))) {
    stop("`pings` already has: ", paste(clash, collapse = ", "), call. = FALSE)
  }

  n <- nrow(pings)
  u <- pings[[unit]]
  t <- as.numeric(pings$time)
  pos <- !is.na(pings$lon) & !is.na(pings$lat)
  inu <- !is.na(u)
  ord <- order(pings$vid, u, t, seq_len(n), na.last = TRUE)
  k <- ord[inu[ord] & pos[ord]]                  # the positioned pings of the units, in time order by unit
  m <- length(k)
  same <- if (m > 1) c(pings$vid[k][-1] == pings$vid[k][-m] & u[k][-1] == u[k][-m], FALSE) else rep(FALSE, m)
  nxt <- c(k[-1], NA_integer_)
  es <- rep(NA_real_, n)
  em <- rep(NA_real_, n)
  basis <- rep("not in a unit", n)
  basis[inu & !pos] <- "no position"
  es[inu & !pos] <- 0
  em[inu & !pos] <- 0
  es[k] <- ifelse(same, t[nxt] - t[k], 0)
  em[k] <- ifelse(same, .os_dist_m(pings$lon[k], pings$lat[k], pings$lon[nxt], pings$lat[nxt]), 0)
  basis[k] <- ifelse(same, "the stretch to the unit's next ping", "the unit's last ping")

  out <- pings
  out$effort_s <- es
  out$effort_m <- em
  out$effort_basis <- basis

  # each unit met: its pings, time and ground
  pk <- if (!is.null(units) && !"vid" %in% names(units)) as.character(u) else paste(pings$vid, u, sep = "\r")
  w <- which(inu)
  ut <- data.frame(key = unique(pk[w]))
  ut$vid <- pings$vid[w][match(ut$key, pk[w])]
  ut[[unit]] <- u[w][match(ut$key, pk[w])]
  ut$pings <- as.integer(tabulate(match(pk[w], ut$key), nrow(ut)))
  ut$effort_s <- as.numeric(rowsum(es[w], factor(pk[w], ut$key), reorder = TRUE)[, 1])
  ut$effort_m <- as.numeric(rowsum(em[w], factor(pk[w], ut$key), reorder = TRUE)[, 1])
  npos <- as.numeric(rowsum(as.numeric(pos[w]), factor(pk[w], ut$key), reorder = TRUE)[, 1])

  unplaced <- NULL
  if (length(qs)) {
    if (!"vid" %in% names(units)) {
      two <- tapply(pings$vid[w], pk[w], function(v) length(unique(v)))
      if (any(two > 1)) stop("A `", unit, "` value is on two vessels: give `units` a `vid` column.", call. = FALSE)
      uk <- as.character(units[[unit]])
    } else {
      uk <- paste(units$vid, units[[unit]], sep = "\r")
    }
    if (anyDuplicated(uk)) stop("`units` has a unit twice.", call. = FALSE)
    mu <- match(ut$key, uk)
    tot <- if (share == "ground") ut$effort_m else ut$effort_s
    i <- match(pk, ut$key)                         # each ping's unit (NA in none)
    wt <- if (share == "ground") em else es
    frac <- ifelse(tot[i] > 0, wt / tot[i], ifelse(pos, 1 / npos[i], 0))
    sb <- ifelse(is.na(mu[i]), "no row in units",
                 ifelse(!pos, "no position",
                        ifelse(tot[i] > 0, share,
                               if (share == "ground") "equal: the unit covers no ground" else "equal: the unit takes no time")))
    sb[!inu] <- NA_character_
    ut$share_basis <- ifelse(is.na(mu), "no row in units",
                             ifelse(npos == 0, "no position",
                                    ifelse(tot > 0, share, if (share == "ground") "equal: the unit covers no ground" else "equal: the unit takes no time")))
    for (q in qs) {
      v <- units[[q]][mu[i]] * frac
      v[!inu] <- NA_real_
      out[[q]] <- v
      ut[[q]] <- units[[q]][mu]
      placed <- sum(v, na.rm = TRUE)
      given <- sum(ut[[q]][npos > 0], na.rm = TRUE)   # a unit with no positioned ping places nothing
      if (abs(placed - given) > 1e-9 * max(1, abs(given))) {
        stop(sprintf("os_add_effort(): `%s` placed on the pings (%g) is not the units' (%g).", q, placed, given), call. = FALSE)
      }
    }
    out$share_basis <- sb
    unplaced <- units[!uk %in% ut$key, , drop = FALSE]
  }
  ut$key <- NULL
  attr(out, "units") <- ut
  attr(out, "unplaced") <- unplaced
  out
}

#' Quantities laid on cells of a grid: squares of any size, H3 hexagons, or both
#'
#' Sums quantities by cell, each ping's laid along the stretch of track it stands for: the straight line from the ping
#' to its unit's next ping (as [os_add_effort()] gives it), cut into pieces no longer than half the finest cell asked
#' for, each piece carrying its share of the stretch's quantities to the cell it lies in. So a cell finer than the
#' distance between pings takes its share and is not skipped, and no value is given: the pieces follow from the cells.
#' Without `unit`, each ping's quantities stay at the ping.
#'
#' The cells are the finest asked for, and any coarser grid is a sum of them: squares at `square` degrees sum to any
#' whole number of them (a c-square of 0.05 degrees, 0.5 degrees, an ICES rectangle of 1 by 0.5 degrees), H3 cells at
#' resolution `h3` to any coarser resolution (`h3_cell_to_parent()`). A grid is one choice among the columns the
#' cells are kept apart by, as a gear or a month is: ogis's `og_cells()` draws the table and lets the reader choose.
#'
#' @param pings A data frame of pings with `lon` and `lat` (decimal degrees), the `values` columns, the `by` columns,
#'   and, with `unit`, `vid`, `time` (`POSIXct`) and the `unit` column.
#' @param values The names of the numeric columns to sum, each the quantity of the stretch from the ping to its unit's
#'   next ping (`effort_s`, `effort_m` and the shares from [os_add_effort()]). An `NA` is not counted.
#' @param by The names of columns the cells are kept apart by (a gear, a month, a vessel), or `NULL`.
#' @param unit The name of the column that names each ping's unit within its vessel, as in [os_add_effort()]; `NULL`,
#'   each ping's quantities at the ping.
#' @param square The side of the finest squares in degrees (`0.001` by default; `NULL`, none): each cell's south-west
#'   corner as whole steps of it, `ix` and `iy`.
#' @param h3 The resolution of the finest H3 cells (0-15; `NULL`, none, the default), by DuckDB's community extension
#'   h3, installed on first use.
#'
#' @return A data frame, a row a cell and combination of `by`: `ix` and `iy` (with `square`), `h3` (with `h3`, the
#'   cell's index as text), the `by` columns, each of `values` summed, and `n`, the pings counted in the cell they lie
#'   in. The attribute `"pieces"` is the number of pieces the stretches were cut into, `"piece_m"` the longest piece
#'   allowed, and `"unplaced"` the sums of the pings with no position.
#' @seealso [os_add_effort()]; ogis's `og_cells()`, which draws the cells.
#' @export
#'
#' @examples
#' pings <- data.frame(vid = 1, time = as.POSIXct("2026-01-01", tz = "UTC") + 60 * 0:9,
#'                     lon = 13.5 + 0.001 * c(0:4, 10, 10, 10:12), lat = 43.6,
#'                     haul_id = c(1, 1, 1, 1, 1, NA, 2, 2, 2, 2), gear = "GNS")
#' e <- os_add_effort(pings, units = data.frame(vid = 1, haul_id = 1:2, soak_h = c(24, 48)))
#' e$hauling_h <- e$effort_s / 3600
#' os_create_cells(e, c("hauling_h", "soak_h"), by = "gear", unit = "haul_id", square = 0.001)
os_create_cells <- function(pings, values, by = NULL, unit = NULL, square = 0.001, h3 = NULL) {
  if (!is.data.frame(pings)) stop("`pings` must be a data frame.", call. = FALSE)
  if (!is.character(values) || !length(values)) stop("`values` must name columns of `pings`.", call. = FALSE)
  need <- c("lon", "lat", values, by, if (!is.null(unit)) c("vid", "time", unit))
  miss <- setdiff(need, names(pings))
  if (length(miss)) stop("`pings` lacks: ", paste(miss, collapse = ", "), call. = FALSE)
  bad <- values[!vapply(pings[values], is.numeric, NA)]
  if (length(bad)) stop("`values` must be numeric columns: ", paste(bad, collapse = ", "), call. = FALSE)
  if (length(clash <- intersect(c(values, by), c("ix", "iy", "h3", "n")))) {
    stop("`values` and `by` cannot be named ix, iy, h3 or n: rename ", paste(clash, collapse = ", "), ".", call. = FALSE)
  }
  if (is.null(square) && is.null(h3)) stop("Ask for squares (`square`), H3 cells (`h3`) or both.", call. = FALSE)
  if (!is.null(square) && !(is.numeric(square) && length(square) == 1 && is.finite(square) && square > 0)) {
    stop("`square` is the side of the squares in degrees, above 0.", call. = FALSE)
  }
  if (!is.null(h3) && !(is.numeric(h3) && length(h3) == 1 && h3 %in% 0:15)) stop("`h3` is a resolution, 0 to 15.", call. = FALSE)
  if (!is.null(unit) && !inherits(pings$time, "POSIXct")) stop("`pings$time` must be POSIXct.", call. = FALSE)

  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  if (!is.null(h3)) .os_load_h3(con)

  n <- nrow(pings)
  pos <- !is.na(pings$lon) & !is.na(pings$lat)
  unplaced <- vapply(pings[!pos, values, drop = FALSE], sum, 0, na.rm = TRUE)
  p <- which(pos)

  # the longest piece: half the finest cell's narrowest side, at the pings' highest latitude
  lat_max <- if (length(p)) max(abs(pings$lat[p])) else 0
  sides <- c(
    if (!is.null(square)) min(square * 111320 * cos(min(lat_max, 89) * pi / 180), square * 110574),
    if (!is.null(h3)) DBI::dbGetQuery(con, sprintf("SELECT h3_get_hexagon_edge_length_avg(%d, 'm') AS e", as.integer(h3)))$e
  )
  piece_m <- min(sides) / 2

  # each positioned ping's stretch: to its unit's next positioned ping, if any
  nxt <- rep(NA_integer_, n)
  if (!is.null(unit)) {
    u <- pings[[unit]]
    t <- as.numeric(pings$time)
    ord <- order(pings$vid, u, t, seq_len(n), na.last = TRUE)
    k <- ord[!is.na(u[ord]) & pos[ord]]
    m <- length(k)
    if (m > 1) {
      same <- pings$vid[k][-1] == pings$vid[k][-m] & u[k][-1] == u[k][-m]
      nxt[k[-m][same]] <- k[-1][same]
    }
  }
  lon0 <- pings$lon[p]
  lat0 <- pings$lat[p]
  lon1 <- ifelse(is.na(nxt[p]), lon0, pings$lon[nxt[p]])
  lat1 <- ifelse(is.na(nxt[p]), lat0, pings$lat[nxt[p]])
  len <- ifelse(is.na(nxt[p]), 0, .os_dist_m(lon0, lat0, lon1, lat1))
  k <- pmax(1L, as.integer(ceiling(len / piece_m)))
  at <- rep(seq_along(p), k)
  i <- sequence(k) - 1L
  f <- (i + 0.5) / k[at]
  pieces <- data.frame(x = lon0[at] + (lon1 - lon0)[at] * f, y = lat0[at] + (lat1 - lat0)[at] * f)
  for (b in by) pieces[[b]] <- pings[[b]][p][at]
  for (v in values) pieces[[v]] <- pings[[v]][p][at] / k[at]
  pieces$n <- as.integer(i == 0L)

  duckdb::duckdb_register(con, "os_pieces", pieces)
  qi <- function(x) as.character(DBI::dbQuoteIdentifier(con, x))
  cells <- c(
    if (!is.null(square)) sprintf("CAST(floor(round(x / %1$.12g, 9)) AS INTEGER) AS ix, CAST(floor(round(y / %1$.12g, 9)) AS INTEGER) AS iy", square),
    if (!is.null(h3)) sprintf("h3_latlng_to_cell_string(y, x, %d) AS h3", as.integer(h3))
  )
  keys <- c(if (!is.null(square)) c("ix", "iy"), if (!is.null(h3)) "h3", by)
  sql <- sprintf("SELECT %s, %s, %s, CAST(sum(n) AS INTEGER) AS n FROM (SELECT *, %s FROM os_pieces) GROUP BY ALL ORDER BY %s",
                 paste(vapply(c(if (!is.null(square)) c("ix", "iy"), if (!is.null(h3)) "h3"), qi, ""), collapse = ", "),
                 if (length(by)) paste(vapply(by, qi, ""), collapse = ", ") else "NULL AS os_none",
                 paste(sprintf("sum(%1$s) AS %1$s", vapply(values, qi, "")), collapse = ", "),
                 paste(cells, collapse = ", "),
                 paste(vapply(keys, qi, ""), collapse = ", "))
  out <- DBI::dbGetQuery(con, sql)
  out$os_none <- NULL

  # every quantity in is a quantity out, by `by`
  for (v in values) {
    given <- sum(pings[[v]][p], na.rm = TRUE)
    laid <- sum(out[[v]], na.rm = TRUE)
    if (abs(given - laid) > 1e-9 * max(1, abs(given))) {
      stop(sprintf("os_create_cells(): `%s` laid on the cells (%g) is not the pings' (%g).", v, laid, given), call. = FALSE)
    }
  }
  if (sum(out$n) != length(p)) stop("os_create_cells(): the pings counted in the cells are not the pings placed.", call. = FALSE)
  attr(out, "pieces") <- nrow(pieces)
  attr(out, "piece_m") <- piece_m
  attr(out, "unplaced") <- unplaced
  out
}

# DuckDB's h3 extension on a connection: loaded, or installed from the community repository first ----------------------
.os_load_h3 <- function(con) {
  ok <- function(sql) !inherits(try(DBI::dbExecute(con, sql), silent = TRUE), "try-error")
  if (ok("LOAD h3") || (ok("INSTALL h3 FROM community") && ok("LOAD h3"))) return(invisible(TRUE))
  stop("H3 cells need DuckDB's community extension h3, which could not be loaded or installed here (it is fetched ",
       "once from the internet). Use `square` alone, or install it: DBI::dbExecute(con, \"INSTALL h3 FROM community\").",
       call. = FALSE)
}
