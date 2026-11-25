# os_add_trip_value(): a trip's value from ranked witnesses. Drafted with AI assistance (Claude), 2026-11-25.

t0 <- as.POSIXct("2026-01-05 06:00:00", tz = "UTC")
trips <- data.frame(vid = c(1, 1, 1, 1, 2), trip_id = c(1, 2, 3, 4, 1))
times <- data.frame(
  vid = c(1, 1, 1, 1, 1, 1, 1, 1, 2),
  trip_id = c(1, 1, 1, 2, 2, 3, 3, 3, 1),
  witness = c("track", "logbook", "logbook", "logbook", "track", "logbook", "ignition", "track", "track"),
  # trip 1: the logbook's two rows, its earliest counts; trip 2: the logbook a date only, the track on that date;
  # trip 3: the track 2 h off the logbook; trip 4: no witness; vessel 2: one witness
  time = t0 + c(0, 600, 7200, 0, 3 * 3600, 0, 900, 7200, 0),
  day = c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE),
  place = c("A", "A", "B", "A", NA, "A", "A", "B", "C")
)
rank <- c("logbook", "track", "ignition")
basis <- c(logbook = "recorded", track = "reconstructed")

test_that("a time from ranked witnesses, the date-only witness giving way", {
  x <- os_add_trip_value(trips, times, keys = c("vid", "trip_id"), rank = rank, value = "time", basis = basis,
                         name = "T1", tolerance = 3600)
  expect_equal(x$T1, t0 + c(600, 3 * 3600, 0, NA, 0))
  expect_equal(x$T1_from, c("logbook", "track", "logbook", NA, "track"))
  expect_equal(x$T1_basis, c("recorded", "reconstructed", "recorded", "none", "reconstructed"))
  expect_equal(x$T1_outcome, c("agree", "agree", "differ", "none", "one"))
  expect_equal(x$T1_n, c(2, 2, 3, 0, 1))
  expect_equal(x$T1_spread_s, c(600, 0, 7200, NA, 0))
  expect_equal(names(x)[1:2], names(trips))
})

test_that("a gear or a port: the most usual value, by weight, the first that sorts on a tie", {
  g <- data.frame(trip = c(1, 1, 1, 1, 2, 2, 3), witness = c("fishers", "fishers", "fishers", "landing", "fishers", "fishers", "landing"),
                  gear = c("LLS", "GNS", "GNS", "LLS", "LLS", "GNS", NA))
  x <- os_add_trip_value(data.frame(trip = 1:3), g, keys = "trip", rank = c("fishers", "landing"), value = "gear", how = "most")
  expect_equal(x$gear, c("GNS", "GNS", NA))                     # trip 2: a tie, GNS sorts first
  expect_equal(x$gear_share, c(2 / 3, 1 / 2, NA))
  expect_equal(x$gear_outcome, c("differ", "one", "none"))      # trip 3: a row with no gear is no vote
  g$weight <- c(10, 1, 1, 5, 1, 1, 1)
  y <- os_add_trip_value(data.frame(trip = 1:3), g, keys = "trip", rank = c("fishers", "landing"), value = "gear", how = "most")
  expect_equal(y$gear[1], "LLS")
  expect_equal(y$gear_outcome[1], "agree")
})

test_that("what cannot be judged is refused", {
  expect_error(os_add_trip_value(trips, times, c("vid", "trip_id"), rank = c("logbook", "track"), value = "time"), "ranked")
  expect_error(os_add_trip_value(trips, times, c("vid", "trip_id"), rank = rank, value = "place"), "time")
  expect_error(os_add_trip_value(trips, times, c("vid", "trip_id"), rank = rank, value = "speed"), "lacks")
  expect_error(os_add_trip_value(transform(trips, time = 1), times, c("vid", "trip_id"), rank = rank, value = "time"), "already")
})

test_that("it gives ramb's trip time, its place, and its gear", {
  skip_if_not_installed("ramb")
  r <- ramb::rb_add_trip_time(trips, times, keys = c("vid", "trip_id"), rank = rank, basis = basis, tolerance = 3600,
                              name = "T1", place_name = "port1")
  o <- os_add_trip_value(trips, times, keys = c("vid", "trip_id"), rank = rank, value = "time", basis = basis, name = "T1")
  # ramb's place: each witness's place on its earliest row; here a witness has one place a trip but for the logbook
  # of trip 1, whose earliest row says A
  w1 <- times[!(times$trip_id == 1 & times$vid == 1 & times$witness == "logbook" & times$place == "B"), ]
  o <- os_add_trip_value(o, w1, keys = c("vid", "trip_id"), rank = rank, value = "place", how = "most", basis = basis, name = "port1")
  o$port1_share <- NULL
  r <- as.data.frame(r)
  for (k in names(r)) expect_equal(o[[k]], r[[k]], ignore_attr = TRUE, info = k)
  g <- data.frame(trip = c(1, 1, 1, 2), witness = c("fishers", "fishers", "landing", "landing"), gear = c("GNS", "LLS", "LLS", "GNS"))
  rg <- as.data.frame(ramb::rb_add_trip_gear(data.frame(trip = 1:3), g, keys = "trip", rank = c("fishers", "landing")))
  og <- os_add_trip_value(data.frame(trip = 1:3), g, keys = "trip", rank = c("fishers", "landing"), value = "gear", how = "most")
  for (k in names(rg)) expect_equal(og[[k]], rg[[k]], ignore_attr = TRUE, info = k)
})
