# os_map(): the pings on ogis's map at whatever stage they have reached. Drafted with AI assistance (Claude), 2026-11-25.

test_that("the map shows what the flow has added so far", {
  skip_if_not_installed("ogis")
  d <- sim_track()
  m <- os_map(d[, c("vid", "time", "lon", "lat")])
  expect_s3_class(m, "og_map")
  expect_match(paste(format(m), collapse = " "), "speed_kn over time, 2 groups")
  d <- os_flag_ping_polygon(d, sim_ports(), name = "port_id", id = "port_id")
  d$trip_key <- ifelse(is.na(d$trip), NA, paste(d$vid, d$trip, sep = ":"))
  f <- os_add_fishing(d, by = "trip_key")
  f$hauler <- f$phase == "haul"
  m <- os_map(f, polygons = list(Ports = sim_ports()), compare = c(ossf = "fishing", sensor = "hauler"))
  txt <- paste(format(m), collapse = " ")
  expect_match(txt, "os_polygons_1")
  expect_match(txt, "lanes: activity, trip, place, ossf, sensor")
  expect_match(txt, "spans: trip")
  # the colour is the last stage the flow made, or the one asked for
  expect_match(paste(format(os_map(f, colour = "place")), collapse = " "), "lanes: place, trip, activity")
})

test_that("what cannot be drawn is refused", {
  skip_if_not_installed("ogis")
  d <- sim_track()
  expect_error(os_map(d[, c("vid", "time", "lon")]), "lacks")
  expect_error(os_map(transform(d, activity = "x")), "already")
  expect_error(os_map(d, colour = "nothing"), "colour")
  expect_error(os_map(d, polygons = list(sim_ports())), "named list")
})
