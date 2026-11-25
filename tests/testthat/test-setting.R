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
