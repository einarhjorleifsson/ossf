# Simulated tracks only: no real data belongs in this package.
track <- function(vid = 1, seed = 1, slow = 1.2, fast = 8) {
  set.seed(seed)
  kn <- c(rep(fast, 60), rep(slow, 120), rep(fast, 60)) * exp(rnorm(240, 0, 0.1))
  data.frame(vid = vid, time = as.POSIXct("2024-01-01", tz = "UTC") + 60 * seq_along(kn),
             lon = 13 + cumsum(kn * 1852 / 60) / 80000, lat = 43.6, slow = rep(c(FALSE, TRUE, FALSE), c(60, 120, 60)))
}

test_that("the slow stretch is fishing, the range estimated from the speeds", {
  p <- track()
  out <- os_add_fishing(p)
  expect_identical(out[names(p)], p)
  expect_gt(mean(out$fishing[p$slow]), 0.95)
  expect_lt(mean(out$fishing[!p$slow]), 0.05)
  r <- attr(out, "ranges")
  expect_equal(r$basis, "estimated")
  expect_true(r$speed_max_kn > 1.2 && r$speed_max_kn < 8)
  expect_equal(r$window_s, 60)
  expect_true(all(out$fishing_basis == "estimated"))
})

test_that("the speed is the distance over the time, in knots", {
  p <- track()
  out <- os_add_fishing(p, window_s = 0)
  expect_equal(out$speed_kn, out$speed_smooth_kn)
  expect_equal(median(out$speed_kn[!p$slow]), 8, tolerance = 0.05)
  expect_equal(median(out$speed_kn[p$slow]), 1.2, tolerance = 0.05)
})

test_that("a range given is used as it is, and a witness measures one", {
  p <- track()
  g <- os_add_fishing(p, range = data.frame(vid = 1, speed_min_kn = 0.5, speed_max_kn = 2))
  expect_equal(attr(g, "ranges")$basis, "given")
  expect_equal(g$fishing, g$speed_smooth_kn >= 0.5 & g$speed_smooth_kn <= 2)
  m <- os_add_fishing(p, witness = "slow")
  r <- attr(m, "ranges")
  expect_equal(r$basis, "measured")
  expect_true(r$speed_min_kn < 1.2 && r$speed_max_kn > 1.2 && r$speed_max_kn < 8)
  expect_gt(mean(m$fishing == p$slow), 0.95)
})

test_that("rows keep their order, vessels and groups are judged apart, and nothing is dropped", {
  p <- rbind(track(1, 1), track(2, 2, slow = 2.5, fast = 10))
  p$trip <- rep(1:2, each = 240)
  set.seed(3)
  s <- p[sample(nrow(p)), ]
  out <- os_add_fishing(s, by = "trip")
  expect_equal(out$time, s$time)
  expect_equal(nrow(attr(out, "ranges")), 2)
  expect_gt(mean(out$fishing == s$slow), 0.95)
  few <- os_add_fishing(track()[1:20, ])
  expect_true(all(is.na(few$fishing)))
  expect_match(few$fishing_basis[1], "^none: ")
  gone <- track(); gone$lon[5] <- NA
  expect_true(is.na(os_add_fishing(gone)$fishing[5]))
})

test_that("misuse is told", {
  p <- track()
  expect_error(os_add_fishing(p[c("vid", "time")]), "lacks")
  expect_error(os_add_fishing(p, window_s = -1), "window_s")
  expect_error(os_add_fishing(p, range = data.frame(vid = 1)), "range")
  expect_error(os_add_fishing(os_add_fishing(p)), "already has")
})
