# A simulated track for the tests: no case study's data is in this package. Each vessel makes two trips from one port:
# in port (30 min, still), out (30 min at 8 kn, north), a haul (60 min at 1.5 kn, east on the first trip and back west
# on the second), back (30 min at 8 kn, south), and in port again at the end. A ping a minute.
# Drafted with AI assistance (Claude), 2026-11-25.
sim_track <- function(vessels = 1:2) {
  kn <- c(rep(0, 30), rep(8, 30), rep(1.5, 60), rep(8, 30))
  north <- c(rep(0, 30), rep(1, 30), rep(0, 60), rep(-1, 30))
  east <- c(rep(0, 60), rep(1, 60), rep(0, 30))
  kn <- c(kn, kn, rep(0, 30))
  north <- c(north, north, rep(0, 30))
  east <- c(east, -east, rep(0, 30))
  n <- length(kn)
  step <- kn / 60 / 60                                   # degrees of latitude a minute
  out <- lapply(vessels, function(v) {
    lat <- 43.600 + cumsum(step * north)
    lon <- 13.50 + 0.2 * (v - 1) + cumsum(step * east / cos(43.65 * pi / 180))
    data.frame(vid = v, time = as.POSIXct("2026-01-01", tz = "UTC") + 60 * seq_len(n), lon = lon, lat = lat,
               phase = c(rep(c("port", "out", "haul", "back"), c(30, 30, 60, 30)), rep(c("port", "out", "haul", "back"), c(30, 30, 60, 30)),
                         rep("port", 30)),
               trip = c(rep(1L, 150), rep(2L, 150), rep(NA, 30)))
  })
  do.call(rbind, out)
}
sim_square <- function(x0, y0, x1, y1) sf::st_polygon(list(rbind(c(x0, y0), c(x1, y0), c(x1, y1), c(x0, y1), c(x0, y0))))
# one port a vessel, around where it lies still
sim_ports <- function(vessels = 1:2) {
  sf::st_sf(port_id = paste0("P", vessels),
            geometry = sf::st_sfc(lapply(vessels, function(v) sim_square(13.45 + 0.2 * (v - 1), 43.59, 13.58 + 0.2 * (v - 1), 43.605)), crs = 4326))
}
