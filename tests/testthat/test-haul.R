# os_add_hauls(): simulated calls and tracks only. Drafted with AI assistance (Claude), 2026-11-25.

# calls along a line east at 43.6 N, a ping a minute, each step `step_m` metres (recycled)
calls <- function(work, step_m = 100, step_s = 60, vid = 1) {
  n <- length(work)
  m <- c(0, cumsum(rep_len(step_m, max(n - 1, 0))))[seq_len(n)]
  data.frame(vid = rep(vid, n), time = as.POSIXct("2024-01-01", tz = "UTC") + c(0, cumsum(rep_len(step_s, max(n - 1, 0))))[seq_len(n)],
             lon = 13 + m / (111320 * cos(43.6 * pi / 180)), lat = rep(43.6, n), fishing = work)
}

test_that("a pause covering no more ground than the shorter run beside it stays in the haul; a longer one ends it", {
  o <- os_add_hauls(calls(c(TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE)))
  expect_equal(o$haul_id, rep(1L, 7))
  expect_equal(o$haul_role, c(rep("working", 3), "pause", rep("working", 3)))
  o <- os_add_hauls(calls(c(TRUE, FALSE, FALSE, TRUE)))
  expect_equal(o$haul_id, c(1L, NA, NA, 2L))
  expect_equal(o$haul_role, c("working", NA, NA, "working"))
  # against the shorter side: 200 m of pause beside 400 and 300 m of work
  o <- os_add_hauls(calls(c(FALSE, rep(TRUE, 4), FALSE, FALSE, rep(TRUE, 3), FALSE)))
  expect_equal(o$haul_id, c(NA, rep(1L, 9), NA))
  h <- attr(o, "hauls")
  expect_equal(nrow(h), 1)
  expect_equal(h[c("pings", "working_pings", "pause_pings", "pauses")], data.frame(pings = 9L, working_pings = 7L, pause_pings = 2L, pauses = 1L))
  expect_equal(h$distance_m, 900, tolerance = 0.005)
  expect_equal(h$working_m, 700, tolerance = 0.005)
  expect_equal(h$duration_s, 9 * 60)
  expect_equal(h$working_s, 7 * 60)
  expect_equal(h$from, "fishing")
})

test_that("each pause is held against the runs as they are, in one pass", {
  # 10 pings of work, 2 of pause, 1 of work, 2 of pause, 10 of work: both pauses cover more than the lone ping's run
  o <- os_add_hauls(calls(c(rep(TRUE, 10), FALSE, FALSE, TRUE, FALSE, FALSE, rep(TRUE, 10))))
  expect_equal(o$haul_id, c(rep(1L, 10), NA, NA, 2L, NA, NA, rep(3L, 10)))
  expect_equal(attr(o, "hauls")$pings, c(10L, 1L, 10L))
})

test_that("the length is the ground covered, not the time or the pings", {
  work <- c(rep(TRUE, 5), rep(FALSE, 3), rep(TRUE, 5))
  # steaming: three pings, three minutes, but 1 km a step around them, against 100 m steps in the work
  steam <- os_add_hauls(calls(work, step_m = c(100, 100, 100, 100, 1000, 1000, 1000, 1000, 100, 100, 100, 100)))
  expect_equal(steam$haul_id, c(rep(1L, 5), NA, NA, NA, rep(2L, 5)))
  # a stop: the same pings, an hour each, standing still
  stop <- os_add_hauls(calls(work, step_m = c(100, 100, 100, 100, 0, 0, 0, 0, 100, 100, 100, 100),
                             step_s = c(60, 60, 60, 60, 3600, 3600, 3600, 3600, 60, 60, 60, 60)))
  expect_equal(stop$haul_id, rep(1L, 13))
  h <- attr(stop, "hauls")
  expect_equal(h$distance_m, 800, tolerance = 0.005)
  expect_equal(h$duration_s, 8 * 60 + 4 * 3600)
  # a ping with no position covers no ground: the step it lies in is shared by the pings either side
  p <- calls(c(TRUE, TRUE, NA, TRUE, TRUE))
  p$lon[3] <- NA
  o <- os_add_hauls(p)
  expect_equal(o$haul_id, rep(1L, 5))
  expect_equal(o$haul_role[3], "pause")
  expect_equal(attr(o, "hauls")$distance_m, 400, tolerance = 0.005)
})

test_that("the call is never changed, rows keep their order, groups and vessels are apart", {
  p <- sim_track()
  p$trip[p$phase == "port"] <- NA
  f <- os_add_fishing(p, by = "trip")
  set.seed(4)
  s <- f[sample(nrow(f)), ]
  o <- os_add_hauls(s, by = "trip")
  for (k in names(s)) expect_identical(o[[k]], s[[k]])
  expect_true(all(o$haul_role[o$fishing %in% TRUE] == "working"))
  expect_true(all(is.na(o$haul_id[is.na(o$trip)])))
  h <- attr(o, "hauls")
  expect_equal(names(h), c("vid", "trip", "haul_id", "start", "end", "pings", "working_pings", "pause_pings", "pauses", "distance_m",
                           "working_m", "duration_s", "working_s", "from"))
  expect_equal(h$vid, c(1, 1, 2, 2))
  expect_equal(h$haul_id, c(1L, 2L, 1L, 2L))
  expect_equal(h$trip, c(1L, 2L, 1L, 2L))
  expect_true(all(h$start < h$end))
  # each haul is the simulated haul, give or take its ends
  inside <- !is.na(o$haul_id)
  expect_gt(mean(o$phase[inside] == "haul"), 0.95)
  expect_gt(mean(inside[o$phase == "haul"]), 0.95)
  # ids follow time within the vessel, in the pings and in the table
  one <- o[o$vid == 1 & inside, ]
  expect_true(all(tapply(as.numeric(one$time), one$haul_id, min) == as.numeric(h$start[h$vid == 1])))
  # by = NULL: one group a vessel, and the trips' hauls are still apart (the steaming between them is long)
  expect_equal(nrow(attr(os_add_hauls(f[!is.na(f$trip), ]), "hauls")), 4)
})

test_that("pauses at a group's edge, unjudged pings and calls that are NA", {
  o <- os_add_hauls(calls(c(FALSE, TRUE, NA, TRUE, FALSE)))
  expect_equal(o$haul_id, c(NA, 1L, 1L, 1L, NA))
  expect_equal(o$haul_role, c(NA, "working", "pause", "working", NA))
  p <- calls(c(TRUE, TRUE, TRUE))
  p$time[2] <- NA
  o <- os_add_hauls(p)
  expect_equal(o$haul_id, c(1L, NA, 1L))
  none <- os_add_hauls(calls(rep(FALSE, 5)))
  expect_true(all(is.na(none$haul_id)))
  expect_equal(nrow(attr(none, "hauls")), 0)
  expect_s3_class(attr(none, "hauls")$start, "POSIXct")
  expect_equal(nrow(attr(os_add_hauls(calls(logical(0))), "hauls")), 0)
  expect_equal(os_add_hauls(calls(TRUE))$haul_id, 1L)
})

test_that("any logical call works, and misuse is told", {
  p <- calls(c(TRUE, FALSE, TRUE))
  names(p)[names(p) == "fishing"] <- "sensor"
  expect_equal(attr(os_add_hauls(p, call = "sensor"), "hauls")$from, c("sensor", "sensor"))
  expect_error(os_add_hauls(p), "lacks: fishing")
  expect_error(os_add_hauls(p[c("vid", "time", "sensor")], call = "sensor"), "lacks: lon, lat")
  expect_error(os_add_hauls(transform(p, fishing = 1), call = "fishing"), "logical")
  expect_error(os_add_hauls(transform(p, time = 1:3), call = "sensor"), "POSIXct")
  expect_error(os_add_hauls(os_add_hauls(p, call = "sensor"), call = "sensor"), "already has")
  expect_error(os_add_hauls(p, call = c("a", "b")), "one column")
})
