# The pings on a map, at whatever stage of the flow they have reached: one call. ossf's own; drawn by ogis.
#
# The maintainer (2026-11-25): "Add also 'wrapper' functions for generating some ogis apps that may be of use at each of
# the code flow stage. Thinking here wrappers by which one can make a single call for layers." One function, not one a
# stage: it reads which columns the flow has added so far and draws those, so the same call serves after each step.
# Drafted with AI assistance (Claude), 2026-11-25; not yet checked by a person.

#' The pings on a map, with what the flow has made of them so far
#'
#' One call after any step of the flow. The map shows each vessel's track and pings with a time panel under it
#' (ogis's `og_tracks()`), and reads from the pings' columns how far the flow has come:
#'
#' * **pings**: the speed over time (`speed_smooth_kn`, `speed_kn`, `speed`; with none, the speed is derived from the
#'   track, each ping from the ping before to the ping after);
#' * **flags** (`ping_outcome`, or `ping_flag`): each ping's outcome as a lane under the chart, and a mark on every
#'   ping that is not kept;
#' * **ports** (`port_id`, from [os_flag_ping_polygon()]): in which port or at sea, as a lane;
#' * **trips** (`voyage_key`, `trip_key` or `trip_id`): a trip at a time, chosen in the panel;
#' * **fishing** (`fishing`, from [os_add_fishing()]): fishing or not, as a lane.
#'
#' The pings are coloured by speed, or by one of these, chosen in the page: by default the last the flow made
#' (fishing, else the port, else the outcome), or the one named in `colour`. Layers of polygons (ports, land) are
#' drawn under the pings. Two columns can be held against each other (`compare`): a call against a witness, two
#' methods' trips.
#'
#' The map is served on this computer only when printed (`127.0.0.1`); nothing is sent anywhere.
#'
#' @param pings A data frame of pings: `vid`, `time`, `lon`, `lat`, and whatever the flow has added.
#' @param colour The column the pings are coloured by besides speed: `"activity"`, `"place"`, `"outcome"` (what the
#'   map makes of `fishing`, `port_id` and the flags), or any other column of `pings`.
#' @param polygons An sf object of polygons, or a named list of them (`list(Ports = ports, Land = land)`).
#' @param compare Two columns to hold against each other, named for their sources (`c(ossf = "fishing", sensor =
#'   "hauler")`), or a list of such pairs. For a logical column, its `TRUE` pings are compared.
#' @param trip The trip's column, where it is not one of the usual names.
#' @param lanes,marks More lanes under the chart and more marks, columns as strings (as `ogis::og_tracks()`).
#' @param title The page's title.
#' @param ... More for `ogis::og_tracks()`: `time_label`, `effort`, `cell`, ...
#'
#' @return An ogis map. Printed at the console, it opens in the browser.
#' @export
#' @examples
#' \dontrun{
#' pings |> os_add_fishing(by = "trip_key") |> os_map(polygons = list(Ports = ports))
#' }
os_map <- function(pings, colour = NULL, polygons = NULL, compare = NULL, trip = NULL, lanes = NULL, marks = NULL,
                   title = "Tracks", ...) {
  if (!requireNamespace("ogis", quietly = TRUE)) stop("os_map() draws with ogis, which is not installed.", call. = FALSE)
  if (!is.data.frame(pings)) stop("`pings` must be a data frame.", call. = FALSE)
  miss <- setdiff(c("vid", "time", "lon", "lat"), names(pings))
  if (length(miss)) stop("`pings` lacks: ", paste(miss, collapse = ", "), call. = FALSE)
  x <- as.data.frame(pings)
  made <- c("activity", "place", "outcome", "flagged")
  if (length(intersect(made, names(x)))) {
    stop("`pings` already has columns the map makes: ", paste(intersect(made, names(x)), collapse = ", "), call. = FALSE)
  }

  # the speed: the flow's, the record's, or the track's own
  speed <- intersect(c("speed_smooth_kn", "speed_kn", "speed"), names(x))
  if (!length(speed)) {
    x$speed_kn <- NA_real_
    t <- as.numeric(x$time)
    ok <- !is.na(x$vid) & !is.na(t) & !is.na(x$lon) & !is.na(x$lat)
    for (i in split(which(ok), x$vid[ok])) {
      i <- i[order(t[i])]
      x$speed_kn[i] <- .os_speed(t[i], x$lon[i], x$lat[i])
    }
    speed <- "speed_kn"
  }

  # what the flow has added, as words the page can show
  stages <- character()
  if ("ping_outcome" %in% names(x)) {
    x$outcome <- as.character(x$ping_outcome)
    stages <- c(stages, "outcome")
  } else if ("ping_flag" %in% names(x)) {
    x$outcome <- ifelse(is.na(x$ping_flag), "kept", as.character(x$ping_flag))
    stages <- c(stages, "outcome")
  }
  if ("outcome" %in% stages) x$flagged <- ifelse(is.na(x$outcome) | x$outcome == "kept", NA_character_, x$outcome)
  if ("port_id" %in% names(x)) {
    x$place <- ifelse(is.na(x$port_id), "at sea", paste("port", x$port_id))
    stages <- c(stages, "place")
  }
  if ("fishing" %in% names(x) && is.logical(x$fishing)) {
    x$activity <- ifelse(is.na(x$fishing), NA_character_, ifelse(x$fishing, "fishing", "not fishing"))
    stages <- c(stages, "activity")
  }
  if (is.null(colour) && length(stages)) colour <- stages[length(stages)]
  if (!is.null(colour) && (length(colour) != 1 || !colour %in% names(x))) {
    stop("`colour` names one column of `pings`, or \"activity\", \"place\" or \"outcome\" where the flow has made them.", call. = FALSE)
  }

  # two columns held against each other: a logical column's TRUE pings, under its source's name
  pairs <- if (is.null(compare)) list() else if (is.list(compare)) compare else list(compare)
  for (k in seq_along(pairs)) {
    pr <- pairs[[k]]
    if (!is.character(pr) || length(pr) != 2 || !all(pr %in% names(x))) {
      stop("`compare` is two columns of `pings`, as strings, named for their sources, or a list of such pairs.", call. = FALSE)
    }
    lab <- if (!is.null(names(pr)) && all(nzchar(names(pr)))) names(pr) else unname(pr)
    for (j in 1:2) {
      if (is.logical(x[[pr[j]]])) {
        new <- make.unique(c(names(x), lab[j]))[length(names(x)) + 1]
        x[[new]] <- ifelse(x[[pr[j]]] %in% TRUE, "yes", NA_character_)
        pr[j] <- new
      }
    }
    pairs[[k]] <- stats::setNames(pr, lab)
  }

  more <- list(...)
  if (is.null(more$effort)) more$effort <- FALSE
  m <- do.call(ogis::og_tracks, c(list(
    pings = x, speed = speed, activity = colour, trip = trip,
    lanes = c(setdiff(stages, colour), lanes),
    marks = c(if ("flagged" %in% names(x) && any(!is.na(x$flagged))) c("not kept" = "flagged"), marks),
    compare = if (length(pairs)) pairs, title = title
  ), more))
  if (!is.null(polygons)) {
    if (inherits(polygons, c("sf", "sfc"))) polygons <- list(Polygons = polygons)
    if (!is.list(polygons) || is.null(names(polygons)) || !all(vapply(polygons, inherits, NA, c("sf", "sfc")))) {
      stop("`polygons` is an sf object, or a named list of them.", call. = FALSE)
    }
    paint <- rep_len(c("#ffd400", "#8c8c8c", "#4daf4a", "#984ea3"), length(polygons))
    for (k in seq_along(polygons)) {
      p <- polygons[[k]]
      if (inherits(p, "sfc")) p <- sf::st_sf(geometry = p)
      # by do.call(), so that the colours reach ogis as values: given as an expression, it reads them as column names
      m <- do.call(ogis::og_polygon, list(m, data = sf::st_transform(p, 4326), fill = paint[k], colour = paint[k],
                                          opacity = 0.25, linewidth = 2, name = names(polygons)[k],
                                          id = paste0("os_polygons_", k)))
    }
  }
  m
}
