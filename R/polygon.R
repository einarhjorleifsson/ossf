# A ping in a polygon: one function for every layer (land, ports, areas). ossf's own, not a copy.
#
# The maintainer (2026-11-25), of ramb's rb_flag_ping_land() and rb_flag_ping_port(): "these are basically the same, a
# classical ping-in-polygon question. they should actually really be just one function." So ossf has this one in place
# of copies of the two: the port flag is the polygon's id, the land flag is TRUE or FALSE with the distance in from the
# edge. Drafted with AI assistance (Claude), 2026-11-25; not yet checked by a person.

#' Label each ping with the polygon it lies in
#'
#' One question for every layer of polygons (land, ports, closed areas, a harbour's berths): does a polygon of the
#' layer hold the ping, and which. A label, never a removal: every ping comes back.
#'
#' What is added depends on what is asked for:
#' * with no `id`, `name` is `TRUE` where a polygon of the layer holds the ping and `FALSE` where none does (land: on
#'   land or at sea);
#' * with `id`, `name` is that column's value for the polygon that holds the ping, and `NA` outside every polygon
#'   (ports: the port's id, `NA` at sea);
#' * `keep` adds more columns of that polygon;
#' * `edge_m = TRUE` adds `<name>_m`, how far inside the layer the ping lies, in metres to the layer's edge (land: how
#'   far inland).
#'
#' Where two polygons hold a ping, the smaller is taken (the more specific: a berth inside a harbour), then the one
#' whose `id` sorts first, so there is one answer a ping.
#'
#' A polygon's edge is a straight line between its corners in longitude and latitude, and a ping on the edge is inside.
#' Each position is judged once, however many pings share it (a vessel at a berth). The distance to the edge is taken
#' on an azimuthal equidistant projection centred on the pings, with the layer cut to 10 km beyond the pings' extent,
#' so it is measured to the edge within that. s2 is switched off while the function runs and set back after.
#'
#' In ramb these are two functions: `ramb::rb_flag_ping_port()` (as `id = "port_id"`) and
#' `ramb::rb_flag_ping_land()` (as `edge_m = TRUE`, its port column a second call with the ports).
#'
#' @param pings A data frame of pings with longitude and latitude (WGS 84).
#' @param polygons An sf object of polygons, or an sfc when neither `id` nor `keep` is asked for, in any CRS.
#' @param name The name of the column added.
#' @param id A column of `polygons` whose value is written to `name`, or `NULL` for `TRUE` and `FALSE`.
#' @param keep More columns of `polygons` to add, under their own names.
#' @param edge_m `TRUE` to add `<name>_m`, the metres from a ping inside the layer to the layer's edge.
#' @param lon,lat The names of the position columns.
#'
#' @return `pings`, the same rows in the same order, with the columns added. A ping with no position gets `NA`.
#' @export
#' @examples
#' sq <- function(x0, y0, x1, y1) {
#'   sf::st_polygon(list(rbind(c(x0, y0), c(x1, y0), c(x1, y1), c(x0, y1), c(x0, y0))))
#' }
#' # land, and a port across its edge
#' land <- sf::st_sfc(sq(13.50, 43.60, 13.60, 43.70), crs = 4326)
#' ports <- sf::st_sf(port_id = "A", geometry = sf::st_sfc(sq(13.495, 43.645, 13.505, 43.655), crs = 4326))
#' # a ping at sea, one in the port on land, one inland
#' pings <- data.frame(vid = 1, lon = c(13.45, 13.5005, 13.55), lat = 43.65)
#' pings <- os_flag_ping_polygon(pings, ports, name = "port_id", id = "port_id")
#' os_flag_ping_polygon(pings, land, name = "on_land", edge_m = TRUE)
os_flag_ping_polygon <- function(pings, polygons, name = "in_polygon", id = NULL, keep = NULL, edge_m = FALSE,
                                 lon = "lon", lat = "lat") {
  if (!is.data.frame(pings)) stop("`pings` must be a data frame.", call. = FALSE)
  miss <- setdiff(c(lon, lat), names(pings))
  if (length(miss)) stop("`pings` lacks: ", paste(miss, collapse = ", "), call. = FALSE)
  if (!is.numeric(pings[[lon]]) || !is.numeric(pings[[lat]])) stop("The position columns must be numbers.", call. = FALSE)
  if (!inherits(polygons, c("sf", "sfc"))) stop("`polygons` must be an sf or sfc object of polygons.", call. = FALSE)
  if (is.na(sf::st_crs(polygons))) stop("`polygons` has no CRS: set it (sf::st_set_crs()).", call. = FALSE)
  cols <- c(id, keep)
  if (length(cols) && !inherits(polygons, "sf")) stop("`id` and `keep` name columns: `polygons` must be an sf object.", call. = FALSE)
  if (length(setdiff(cols, names(polygons)))) {
    stop("`polygons` lacks: ", paste(setdiff(cols, names(polygons)), collapse = ", "), call. = FALSE)
  }
  added <- c(name, keep, if (edge_m) paste0(name, "_m"))
  if (anyDuplicated(added) || length(intersect(added, names(pings)))) {
    stop("`pings` already has, or the call names twice: ", paste(unique(c(added[duplicated(added)], intersect(added, names(pings)))), collapse = ", "),
         call. = FALSE)
  }
  # s2 off while it runs, the option set back as it was (unset, if it was)
  op <- options("sf_use_s2")
  on.exit(options(op), add = TRUE)
  suppressMessages(sf::sf_use_s2(FALSE))

  # each position once
  x <- pings[[lon]]
  y <- pings[[lat]]
  ok <- is.finite(x) & is.finite(y)
  key <- paste(x, y)
  u <- ok & !duplicated(key)
  back <- match(key, key[u])
  back[!ok] <- NA
  valid <- sf::st_make_valid(sf::st_geometry(polygons))
  geom <- sf::st_transform(valid, 4326)
  pts <- sf::st_as_sf(data.frame(lon = x[u], lat = y[u]), coords = c("lon", "lat"), crs = 4326)
  hits <- suppressMessages(sf::st_intersects(pts, geom))
  inside <- lengths(hits) > 0

  # the polygon that holds the ping: of two, the smaller, then the one whose id sorts first
  if (length(cols)) {
    table <- sf::st_drop_geometry(polygons)
    area <- as.numeric(sf::st_area(sf::st_set_crs(geom, NA)))
    place <- integer(length(geom))
    place[if (is.null(id)) order(area) else order(area, table[[id]])] <- seq_along(geom)
    holder <- vapply(hits, function(h) if (!length(h)) NA_integer_ else h[which.min(place[h])], 1L)
  }
  pings[[name]] <- if (is.null(id)) inside[back] else table[[id]][holder][back]
  for (k in keep) pings[[k]] <- table[[k]][holder][back]

  if (edge_m) {
    depth <- rep(NA_real_, length(inside))
    if (any(inside)) {
      crs <- sprintf("+proj=aeqd +lat_0=%.5f +lon_0=%.5f +units=m", stats::median(y[u]), stats::median(x[u]))
      flat <- sf::st_transform(pts, crs)
      box <- sf::st_buffer(sf::st_as_sfc(sf::st_bbox(flat)), 10000)
      layer <- sf::st_transform(valid, crs)
      layer <- sf::st_union(suppressWarnings(sf::st_intersection(layer[lengths(sf::st_intersects(layer, box)) > 0], box)))
      depth[inside] <- as.numeric(sf::st_distance(flat[inside, ], sf::st_cast(sf::st_boundary(layer), "MULTILINESTRING")))
    }
    pings[[paste0(name, "_m")]] <- depth[back]
  }
  pings
}
