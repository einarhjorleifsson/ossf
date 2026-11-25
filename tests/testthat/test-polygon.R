# os_flag_ping_polygon(): ossf's one ping-in-polygon function. Drafted with AI assistance (Claude), 2026-11-25.

test_that("a ping takes the polygon it lies in, TRUE or FALSE without an id", {
  land <- sf::st_sfc(sim_square(13.50, 43.60, 13.60, 43.70), crs = 4326)
  ports <- sf::st_sf(port_id = c("A", "B"), size = c("large", "small"),
                     geometry = sf::st_sfc(sim_square(13.40, 43.60, 13.52, 43.70), sim_square(13.495, 43.645, 13.505, 43.655), crs = 4326))
  # at sea; in both ports, on land; inland; in the large port only, at sea; no position
  p <- data.frame(vid = 1, lon = c(13.30, 13.5005, 13.55, 13.45, NA), lat = c(43.65, 43.65, 43.65, 43.65, 43.65))
  a <- os_flag_ping_polygon(p, ports, name = "port_id", id = "port_id", keep = "size")
  expect_equal(a$port_id, c(NA, "B", NA, "A", NA))           # of two, the smaller
  expect_equal(a$size, c(NA, "small", NA, "large", NA))
  b <- os_flag_ping_polygon(p, land, name = "on_land", edge_m = TRUE)
  expect_equal(b$on_land, c(FALSE, TRUE, TRUE, FALSE, NA))
  expect_true(all(is.na(b$on_land_m[c(1, 4, 5)])))
  expect_lt(b$on_land_m[2], 60)                              # 0.0005 degrees of longitude in from the edge
  expect_gt(b$on_land_m[3], 3000)
  expect_equal(names(b)[1:3], names(p))                      # the rows and columns as given
  expect_equal(nrow(b), nrow(p))
})

test_that("positions shared by many pings are judged alike, in any CRS, and s2 is left as found", {
  land <- sf::st_sfc(sim_square(13.50, 43.60, 13.60, 43.70), crs = 4326)
  p <- data.frame(lon = rep(c(13.55, 13.30), each = 50), lat = 43.65)
  was <- sf::sf_use_s2()
  a <- os_flag_ping_polygon(p, land)
  b <- os_flag_ping_polygon(p, sf::st_transform(land, 3035))
  expect_equal(a$in_polygon, rep(c(TRUE, FALSE), each = 50))
  expect_equal(a$in_polygon, b$in_polygon)
  expect_identical(sf::sf_use_s2(), was)
})

test_that("what cannot be judged is refused", {
  land <- sf::st_sfc(sim_square(0, 0, 1, 1), crs = 4326)
  p <- data.frame(lon = 0.5, lat = 0.5)
  expect_error(os_flag_ping_polygon(p[, "lon", drop = FALSE], land), "lacks")
  expect_error(os_flag_ping_polygon(p, data.frame(a = 1)), "sf")
  expect_error(os_flag_ping_polygon(p, land, id = "port_id"), "sf object")
  expect_error(os_flag_ping_polygon(transform(p, in_polygon = TRUE), land), "already")
  expect_error(os_flag_ping_polygon(p, sf::st_set_crs(land, NA)), "CRS")
})

test_that("it gives ramb's port flag and land flag", {
  skip_if_not_installed("ramb")
  d <- sim_track()
  ports <- sim_ports()
  # land with short edges, as a coastline has (a long edge is the next test's)
  land <- sf::st_sfc(sim_square(13.45, 43.55, 13.58, 43.602), sim_square(13.65, 43.55, 13.78, 43.602), crs = 4326)
  r_port <- ramb::rb_flag_ping_port(d, ports)
  r_land <- ramb::rb_flag_ping_land(d, land, ports)
  o <- os_flag_ping_polygon(d, ports, name = "port_id", id = "port_id")
  expect_identical(o$port_id, r_port$port_id)
  o <- os_flag_ping_polygon(o, land, name = "ping_land", edge_m = TRUE)
  expect_identical(o$ping_land, r_land$ping_land)
  expect_equal(o$ping_land_m, r_land$ping_land_m)
  expect_identical(!is.na(o$port_id), r_land$ping_land_port)
})

test_that("an edge is straight in longitude and latitude, as ramb's port flag has it", {
  # a 40 km edge along a parallel: on a projection it bows some 30 m to the north at its middle, and ramb's land flag,
  # which judges on one, puts a ping 20 m north of the parallel on land; here, as in ramb's port flag, it is outside
  land <- sf::st_sfc(sim_square(13.40, 43.55, 13.90, 43.602), crs = 4326)
  p <- data.frame(lon = 13.65, lat = 43.602 + c(-20, 20) / 111320)
  expect_equal(os_flag_ping_polygon(p, land)$in_polygon, c(TRUE, FALSE))
  skip_if_not_installed("ramb")
  expect_equal(ramb::rb_flag_ping_port(p, sf::st_sf(port_id = 1L, geometry = land))$port_id, c(1L, NA))
  expect_equal(ramb::rb_flag_ping_land(p, land)$ping_land, c(TRUE, TRUE))
})
