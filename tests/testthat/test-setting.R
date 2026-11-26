# os_add_setting(): simulated tracks only. Drafted with AI assistance (Claude), 2026-11-25.

# legs of a track from 13 E 43.6 N, a ping a minute: each leg a speed (kn), minutes and a heading (east, north)
track <- function(legs, vid = 1, start = "2024-01-01", lon0 = 13, lat0 = 43.6) {
  tr <- do.call(rbind, lapply(legs, function(l) data.frame(kn = rep(l[1], l[2]), east = l[3], north = l[4])))
  m <- tr$kn * 1852 / 60
  data.frame(vid = vid, time = as.POSIXct(start, tz = "UTC") + 60 * seq_len(nrow(tr)),
             lon = lon0 + cumsum(m * tr$east) / (111320 * cos(43.6 * pi / 180)), lat = lat0 + cumsum(m * tr$north) / 110540)
}
# out north, a set east at 6 kn, a round north of the line back to its start, the haul east at 1.5 kn, home
set_haul <- function(...) track(list(c(9, 20, 0, 1), c(6, 30, 1, 0), c(9, 20, 0, 1), c(9, 20, -1, 0), c(9, 20, 0, -1), c(1.5, 120, 1, 0), c(9, 20, 0, -1)), ...)
phase <- rep(c("out", "set", "north", "west", "south", "haul", "home"), c(20, 30, 20, 20, 20, 120, 20))

test_that("the set of a haul is the earlier pass along its line, and nothing else", {
  p <- os_add_hauls(os_add_fishing(set_haul()))
  o <- os_add_setting(p, reach_m = 100)
  expect_equal(o[names(p)], p, ignore_attr = TRUE)
  expect_true(all(o$setting[phase %in% c("north", "west", "haul", "home")] == FALSE))
  expect_lte(sum(o$setting[phase == "out"]), 1)   # the position where the set begins
  expect_gte(sum(o$setting[phase == "set"]), 28)
  # the last position of the run in lies on the line too: an event of its own, of a position or two
  expect_lte(sum(o$setting[phase == "south"]), 2)
  expect_true(all(is.na(o$on_later_haul[!is.na(p$haul_id)])))
  expect_true(all(o$setting_basis[!is.na(p$haul_id)] == "none: in a haul"))
  u <- attr(o, "under")
  expect_equal(nrow(u), 1)
  expect_equal(u$under, "a setting event")
  expect_equal(u$share_set, 1)
  expect_equal(u$set_id, 1L)
  expect_equal(u$set_share, 1)
  expect_equal(u$set_m / u$haul_m, 1, tolerance = 0.1)
  # the set began 110 minutes before the haul, and a position is hauled as long after it was set as the haul is slower
  expect_gt(u$soak_s, 60 * 60)
  expect_lt(as.numeric(difftime(u$set_start, min(p$time[phase == "set"]), units = "mins")), 3)
  s <- attr(o, "sets")
  expect_gte(s$pings[1], 28)
  expect_true(all(s$pings[-1] <= 2))
  expect_equal(s$on_later_haul[1], 1)
  r <- attr(o, "ranges")
  expect_equal(r$reach_basis, "given")
  expect_equal(r$length_fit, 1)
})

test_that("rows come back in the order given, whatever it was", {
  p <- os_add_hauls(os_add_fishing(set_haul()))
  k <- sample(nrow(p))
  a <- os_add_setting(p, reach_m = 100)
  b <- os_add_setting(p[k, ], reach_m = 100)
  expect_equal(b$setting, a$setting[k])
  expect_equal(b$set_id, a$set_id[k])
  expect_equal(attr(b, "under"), attr(a, "under"))
})

test_that("gear put back as it is hauled: the haul before is what lies under a haul", {
  # home by a round north of the string, not back along it (a pass along it would itself be a witnessed setting)
  day <- function(d, trip) cbind(track(list(c(9, 20, 0, 1), c(1.5, 120, 1, 0), c(9, 20, 0, 1), c(9, 20, -1, 0), c(9, 40, 0, -1)), start = d), trip = trip)
  p <- rbind(day("2024-01-01", 1L), day("2024-01-02", 2L))
  p <- os_add_hauls(os_add_fishing(p, by = "trip"), by = "trip")
  o <- os_add_setting(p, by = "trip", reach_m = 100)
  u <- attr(o, "under")
  expect_equal(u$under, c("nothing", "an earlier haul"))
  expect_equal(u$soak_s[2], 24 * 3600, tolerance = 0.01)
  expect_gte(u$share_haul[2], 0.9)
  # only the run in's last position or two are called setting
  expect_lte(sum(o$setting), 4)
  expect_true(all(is.na(u$set_share) | u$set_share < 0.2))
})

test_that("several reaches: each vessel takes the one where its sets and hauls are most often of one length", {
  p <- os_add_hauls(os_add_fishing(set_haul()))
  o <- os_add_setting(p, reach_m = c(50, 200))
  r <- attr(o, "ranges")
  expect_true(r$reach_m %in% c(50, 200))
  expect_match(r$reach_basis, "^picked")
  expect_equal(attr(o, "under")$under, "a setting event")
})

test_that("a vessel with no haul has no setting, and says why; bad input stops", {
  p <- set_haul()
  p$haul_id <- NA_integer_
  o <- os_add_setting(p, reach_m = 100)
  expect_false(any(o$setting))
  expect_true(all(o$setting_basis == "none: the vessel has no haul"))
  expect_null(attr(o, "under"))
  expect_error(os_add_setting(set_haul()), "lacks")
  expect_error(os_add_setting(p, reach_m = 0), "reach_m")
  expect_error(os_add_setting(os_add_setting(p, reach_m = 100), reach_m = 100), "already has")
})

test_that("run_in = TRUE takes the run in to a haul out of the setting, and says so; a set hauled later stays", {
  p <- os_add_hauls(os_add_fishing(set_haul()))
  o <- os_add_setting(p, reach_m = 100, run_in = TRUE)
  # the last position of the run in is no setting now; the set, 70 minutes before the haul and 30 long, is
  expect_false(any(o$setting[phase == "south"]))
  expect_true(any(o$setting_basis[phase == "south"] == "none: the run in to a haul"))
  expect_gte(sum(o$setting[phase == "set"]), 28)
  expect_equal(attr(o, "under")$under, "a setting event")
  expect_equal(nrow(attr(o, "sets")), 1)
  # a set that runs straight into its haul is taken for the run in: the argument is for gear hauled on another visit
  q <- os_add_hauls(os_add_fishing(track(list(c(9, 20, 0, 1), c(6, 30, 1, 0), c(1.5, 120, -1, 0), c(9, 20, 0, -1)))))
  expect_equal(attr(os_add_setting(q, reach_m = 100), "under")$under, "a setting event")
  expect_equal(attr(os_add_setting(q, reach_m = 100, run_in = TRUE), "under")$under, "nothing")
  expect_error(os_add_setting(p, run_in = NA), "run_in")
})

test_that("pick = \"open\": a haul closes the set it took, and only that; \"last\" lets two hauls take one set", {
  # trip 1 sets string A east and string B back west, 60 m north of A; trip 2 hauls A, then B: each string lies within
  # reach of both hauls. Between the two the vessel goes a quarter of an hour north and back, further than a string
  # is long, so they are two events
  up <- list(c(9, 15, 0, 1), c(9, 15, 0, -0.9856))                  # a round ending 60 m north of where it began
  t1 <- track(c(list(c(9, 20, 0, 1), c(6, 30, 1, 0)), up, list(c(6, 30, -1, 0), c(9, 20, 0, -1))), start = "2024-01-01")
  t2 <- track(c(list(c(9, 20, 0, 1), c(1.5, 120, 1, 0)), up, list(c(1.5, 120, -1, 0), c(9, 20, 0, -1))), start = "2024-01-02")
  expect_equal((t1$lat[85] - t1$lat[35]) * 110540, 60, tolerance = 0.05)
  p <- rbind(cbind(t1, trip = 1L), cbind(t2, trip = 2L))
  p <- os_add_hauls(os_add_fishing(p, by = "trip"), by = "trip")
  expect_equal(length(unique(stats::na.omit(p$haul_id))), 2)
  open <- attr(os_add_setting(p, by = "trip", reach_m = 100), "under")
  last <- attr(os_add_setting(p, by = "trip", reach_m = 100, pick = "last", pass = FALSE), "under")
  expect_equal(open$under, c("a setting event", "a setting event"))
  expect_equal(last$under, c("a setting event", "a setting event"))
  expect_equal(length(unique(last$set_id)), 1)                      # the string last set, under both hauls
  expect_equal(length(unique(open$set_id)), 2)                      # each haul a set of its own
  expect_true(all(c("share_pass", "share_nothing") %in% names(open)))
})

# a string worked daily for 110 days, the speeds a little uneven (so the setting speed range is measured). Odd days:
# out, a run in east along the string at 6 kn, the haul back west at 1.5 kn, the set east again at 6 kn, home by a round
# north of it (north, west along a line 5.6 km north of the string, south). Even days: out, the haul east from the
# near end with no run in, the set back west, home.
daily <- function() {
  set.seed(1)
  odd <- list(c(9, 20, 0, 1), c(6, 30, 1, 0), c(1.5, 120, -1, 0), c(6, 30, 1, 0), c(9, 20, 0, 1), c(9, 20, -1, 0), c(9, 40, 0, -1))
  even <- list(c(9, 20, 0, 1), c(1.5, 120, 1, 0), c(6, 30, -1, 0), c(9, 20, 0, -1))
  day <- function(i) {
    legs <- if (i %% 2) odd else even
    legs <- unlist(lapply(legs, function(l) lapply(seq_len(l[2]), function(k) c(l[1] * (1 + stats::rnorm(1, 0, 0.03)), 1, l[3], l[4]))), recursive = FALSE)
    cbind(track(legs, start = format(as.Date("2024-01-01") + i)), trip = i)
  }
  do.call(rbind, lapply(1:110, day))
}

test_that("a pass just before the haul gives way to the set of the day before, by the valley of the vessel's own soaks", {
  p <- daily()
  p <- os_add_hauls(os_add_fishing(p, by = "trip"), by = "trip")
  o <- os_add_setting(p, by = "trip", reach_m = 100)
  u <- attr(o, "under")
  r <- attr(o, "ranges")
  expect_equal(r$basis, "measured")
  # the run in is hauled 0 to 150 minutes after it passed (a median 75); the set of the day before, about 22 hours
  expect_true(!is.na(r$soak_valley_s) && r$soak_valley_s > 2 * 3600 && r$soak_valley_s < 20 * 3600)
  # nearly every haul takes the day before's set (the uneven speeds break an event here and there)
  expect_gte(sum(u$under == "a setting event" & u$soak_s > 18 * 3600), 100)
  expect_lte(sum(u$soak_s < 2 * 3600), 3)
  l <- attr(os_add_setting(p, by = "trip", reach_m = 100, pick = "last", pass = FALSE), "under")
  expect_gte(sum(l$soak_s < 2 * 3600), 50)                          # the last thing laid on an odd day is its run in
  expect_true(is.na(attr(os_add_setting(p, by = "trip", reach_m = 100, pick = "last"), "ranges")$soak_valley_s))
})

test_that("pass: a haul with no setting event under it takes the vessel's last pass along its line on an earlier trip", {
  # day 111 hauls along the line the vessel steamed home on, at 9 kn, every odd day: outside its setting speed range
  last_day <- cbind(track(list(c(9, 40, 0, 1), c(1.5, 120, 1, 0), c(9, 20, 0, 1), c(9, 20, -1, 0), c(9, 60, 0, -1)), start = "2024-04-21"), trip = 111L)
  p <- rbind(daily(), last_day)
  p <- os_add_hauls(os_add_fishing(p, by = "trip"), by = "trip")
  o <- os_add_setting(p, by = "trip", reach_m = 100)
  u <- attr(o, "under")
  u <- u[nrow(u), ]
  expect_equal(u$under, "an earlier pass")
  expect_true(is.na(u$set_id))
  expect_gt(u$soak_s, 24 * 3600)                                    # the passage of two days before (day 110 went home south)
  expect_lt(u$soak_s, 72 * 3600)
  n <- attr(os_add_setting(p, by = "trip", reach_m = 100, pass = FALSE), "under")
  expect_equal(n$under[nrow(n)], "nothing")
  expect_error(os_add_setting(p, by = "trip", pass = NA), "pass")
  expect_error(os_add_setting(p, by = "trip", pick = "first"), "arg")
})
