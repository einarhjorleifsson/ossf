# A trip's value from ranked witnesses: one function for a time, a gear, a port. ossf's own, in base R.
#
# The maintainer (2026-11-25), of ramb's rb_add_trip_time() and rb_add_trip_gear(), after "If there are other function
# 'pairs' that can be generalized, consider those also": "yes, merge them". They ask one question, which witness's
# value a trip takes; they differ only in how one witness's rows give one value (the earliest time, the most usual
# gear). Here that is `how`, and one value is settled a call: ramb's place beside the time is a call of its own.
# Drafted with AI assistance (Claude), 2026-11-25; not yet checked by a person.

#' Add a value to each trip from ranked witnesses
#'
#' Several sources witness the same fact of a trip: when it left (the track, the ignition, a logbook), which gear it
#' used (the fishers, the logbook, the landings), from which port. Each trip takes the value of the best-ranked
#' witness that has one, and every trip falls in one outcome: no witness has a value, one has, all that have agree
#' with the value taken, or at least one does not.
#'
#' How a witness's rows for a trip give its one value (`how`):
#' * `"earliest"`: its earliest value, for a time. Another witness agrees when its time lies within `tolerance`
#'   seconds of the one taken, or on the same date where either knows only the date. A witness whose value is a date
#'   only (`day` is `TRUE`) gives way to the best lower-ranked witness with a time on that date.
#' * `"most"`: the value most of its rows give, or most of their `weight`; of two, the value that sorts first. For a
#'   gear, a port. Another witness agrees when its value is the same.
#'
#' A witness's row with no value is no vote. A trip with no row is kept, with outcome `"none"`.
#'
#' In ramb these are two functions: `ramb::rb_add_trip_time()` (as `how = "earliest"`, its place a second call
#' with `how = "most"`) and `ramb::rb_add_trip_gear()` (as `how = "most"`).
#'
#' @param trips A data frame of trips with their key columns.
#' @param witnesses A data frame, a row per trip and witness (or more): the `keys`, `witness` (its name) and the
#'   `value` column; optionally `day` (`TRUE` where a time is a date only) and `weight` (for `"most"`).
#' @param keys The columns that identify a trip, in both tables.
#' @param rank The witnesses' names, best first. A witness that is not ranked is an error.
#' @param value The witnesses' column holding the value: `"time"`, `"gear"`, `"place"`.
#' @param how `"earliest"` or `"most"`, as above.
#' @param basis The basis of each witness's values, named by witness (`c(track = "reconstructed", logbook =
#'   "recorded")`). A witness not named gives `NA`; a trip with no value gets `"none"`.
#' @param name The name of the column added, and the stem of the others.
#' @param tolerance Seconds within which another witness's time agrees with the one taken (`"earliest"`).
#'
#' @return `trips`, the same rows in the same order, with `name` (the value taken), `<name>_from` (the witness it came
#'   from), `<name>_basis`, then for `"earliest"` `<name>_day` (`TRUE` where the value is a date only), for `"most"`
#'   `<name>_share` (the share of the witness's rows, or weight, that give it), then `<name>_n` (the witnesses with a
#'   value), `<name>_outcome` (`"none"`, `"one"`, `"agree"`, `"differ"`) and for `"earliest"` `<name>_spread_s` (the
#'   furthest another witness's time lies from the one taken, by date where either is a date only).
#' @export
#' @examples
#' trips <- data.frame(trip = 1:3)
#' w <- data.frame(trip = c(1, 1, 2, 2), witness = c("track", "logbook", "track", "logbook"),
#'                 gear = c("LLS", "LLS", "GNS", "LLS"))
#' os_add_trip_value(trips, w, keys = "trip", rank = c("logbook", "track"), value = "gear", how = "most")
os_add_trip_value <- function(trips, witnesses, keys, rank, value, how = c("earliest", "most"), basis = NULL,
                              name = value, tolerance = 3600) {
  how <- match.arg(how)
  if (!is.data.frame(trips) || !is.data.frame(witnesses)) stop("`trips` and `witnesses` must be data frames.", call. = FALSE)
  if (length(setdiff(keys, names(trips)))) stop("`trips` lacks: ", paste(setdiff(keys, names(trips)), collapse = ", "), call. = FALSE)
  need <- c(keys, "witness", value)
  if (length(setdiff(need, names(witnesses)))) stop("`witnesses` lacks: ", paste(setdiff(need, names(witnesses)), collapse = ", "), call. = FALSE)
  if (!length(rank) || anyDuplicated(rank)) stop("`rank` must name each witness once, best first.", call. = FALSE)
  unranked <- setdiff(unique(witnesses$witness), rank)
  if (length(unranked)) stop("Every witness must be ranked; not in `rank`: ", paste(unranked, collapse = ", "), call. = FALSE)
  if (how == "earliest" && !inherits(witnesses[[value]], c("POSIXt", "Date"))) stop("`how = \"earliest\"` takes a time.", call. = FALSE)
  added <- paste0(name, c("", "_from", "_basis", if (how == "earliest") "_day" else "_share", "_n", "_outcome",
                          if (how == "earliest") "_spread_s"))
  if (length(intersect(added, names(trips)))) {
    stop("`trips` already has: ", paste(intersect(added, names(trips)), collapse = ", "), call. = FALSE)
  }
  key_of <- function(d) do.call(paste, c(lapply(d[keys], function(v) ifelse(is.na(v), "\u0001NA", as.character(v))), sep = "\r"))
  val <- witnesses[[value]]
  ok <- !is.na(val)
  w <- data.frame(key = key_of(witnesses)[ok], witness = witnesses$witness[ok], stringsAsFactors = FALSE)
  w$rank <- match(w$witness, rank)
  w$i <- which(ok)                                              # the row of `witnesses`, for the value as given
  basis_of <- function(wit) if (is.null(basis)) rep(NA_character_, length(wit)) else unname(basis[wit])

  # each witness's one value for a trip
  if (how == "earliest") {
    w$num <- as.numeric(as.POSIXct(val[ok]))
    w$day <- if ("day" %in% names(witnesses)) witnesses$day[ok] %in% TRUE else FALSE
    o <- order(w$key, w$witness, w$num)
    w <- w[o, ]
    ws <- w[!duplicated(paste(w$key, w$witness, sep = "\r")), ]
  } else {
    w$wt <- if ("weight" %in% names(witnesses)) {
      x <- suppressWarnings(as.numeric(witnesses$weight[ok]))
      ifelse(is.na(x), 0, x)
    } else 1
    w$v <- as.character(val[ok])
    g <- paste(w$key, w$witness, w$v, sep = "\r")
    first <- !duplicated(g)
    sums <- rowsum(w$wt, g, reorder = FALSE)
    a <- w[first, ]
    a$wt <- sums[match(g[first], rownames(sums)), 1]
    kw <- paste(a$key, a$witness, sep = "\r")
    a$tot <- stats::ave(a$wt, kw, FUN = sum)
    o <- order(a$key, a$witness, -a$wt, a$v, method = "radix")  # radix: text in byte order, as DuckDB sorts it
    a <- a[o, ]
    ws <- a[!duplicated(kw[o]), ]
  }

  # the trip's: the best-ranked witness, how many there are, and whether they agree
  o <- order(ws$key, ws$rank)
  best <- ws[o, ][!duplicated(ws$key[o]), ]
  n <- as.numeric(table(ws$key)[best$key])
  if (how == "earliest") {
    # a first witness that knows only the date gives way to the best one with a time on that date
    date <- function(x) floor(x / 86400)
    cand <- ws[!ws$day, ]
    m <- match(cand$key, best$key)
    cand <- cand[best$day[m] & date(cand$num) == date(best$num[m]), ]
    cand <- cand[order(cand$key, cand$rank), ]
    cand <- cand[!duplicated(cand$key), ]
    swap <- match(best$key, cand$key)
    take <- !is.na(swap)
    best[take, ] <- cand[swap[take], ]
    m <- match(ws$key, best$key)
    far <- ifelse(ws$day | best$day[m], abs(date(ws$num) - date(best$num[m])) * 86400, abs(ws$num - best$num[m]))
    spread <- as.numeric(tapply(far, ws$key, max)[best$key])
    outcome <- ifelse(n == 1, "one", ifelse(spread <= tolerance, "agree", "differ"))
  } else {
    k <- tapply(ws$v, ws$key, function(v) length(unique(v)))[best$key]
    outcome <- ifelse(n == 1, "one", ifelse(k == 1, "agree", "differ"))
    share <- ifelse(best$tot == 0, NA_real_, best$wt / best$tot)
  }

  # onto the trips, a trip with no value in outcome "none"
  t <- match(key_of(trips), best$key)
  has <- !is.na(t)
  out <- list()
  out[[added[1]]] <- witnesses[[value]][best$i[t]]
  out[[added[2]]] <- best$witness[t]
  out[[added[3]]] <- ifelse(has, basis_of(best$witness[t]), "none")
  if (how == "earliest") {
    out[[added[4]]] <- best$day[t]
  } else {
    out[[added[4]]] <- share[t]
  }
  out[[added[5]]] <- ifelse(has, n[t], 0)
  out[[added[6]]] <- ifelse(has, outcome[t], "none")
  if (how == "earliest") out[[added[7]]] <- spread[t]
  .os_put_columns(trips, out)
}

# Columns added to a data frame, a tibble or a data.table, which comes back as it was given
.os_put_columns <- function(x, cols) {
  cls <- class(x)
  d <- as.data.frame(x, stringsAsFactors = FALSE)
  for (k in names(cols)) d[[k]] <- cols[[k]]
  if (inherits(x, "data.table")) return(data.table::as.data.table(d))
  if (inherits(x, "tbl_df")) return(tibble::as_tibble(d))
  d
}
