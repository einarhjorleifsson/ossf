# os_add_effort() and os_create_cells() on the simulated track. Drafted with AI assistance (Claude), 2026-11-26.

hauls_of <- function(p) {
  p$haul_id <- ifelse(p$phase == "haul", p$trip, NA)
  p
}

test_that("each ping of a haul stands for the stretch to the haul's next ping, and nothing else", {
  p <- hauls_of(sim_track(1:2))
  e <- os_add_effort(p)
  expect_identical(nrow(e), nrow(p))
  expect_identical(e[names(p)], p)
  # a haul of 60 pings a minute apart: 59 minutes from its first ping to its last
  by_haul <- aggregate(effort_s ~ vid + haul_id, e, sum)
  expect_equal(by_haul$effort_s, rep(59 * 60, 4))
  expect_true(all(is.na(e$effort_s[is.na(e$haul_id)])))
  expect_setequal(unique(e$effort_basis), c("not in a unit", "the stretch to the unit's next ping", "the unit's last ping"))
  # 1.5 knots for 59 minutes
  expect_equal(sum(e$effort_m[e$vid == 1 & e$haul_id %in% 1], na.rm = TRUE), 1.5 * 1852 * 59 / 60, tolerance = 0.01)
})

test_that("a unit's quantity is shared over its pings by ground or time, and sums to the unit's", {
  p <- hauls_of(sim_track(1:2))
  u <- data.frame(vid = c(1, 1, 2, 2), haul_id = c(1, 2, 1, 2), soak_h = c(24, 48, NA, 12), kg = c(10, 20, 30, 40))
  e <- os_add_effort(p, units = u)
  got <- aggregate(cbind(soak_h, kg) ~ vid + haul_id, e, sum, na.action = na.pass)
  expect_equal(got$soak_h[order(got$vid, got$haul_id)], c(24, 48, NA, 12))
  expect_equal(got$kg[order(got$vid, got$haul_id)], c(10, 20, 30, 40))
  # by ground, each ping in proportion to its stretch's ground
  i <- which(e$vid == 1 & e$haul_id %in% 1)
  expect_equal(e$soak_h[i], 24 * e$effort_m[i] / sum(e$effort_m[i]))
  # by time: equal steps, so equal shares but the last ping's
  et <- os_add_effort(p, units = u, share = "time")
  expect_equal(unique(round(et$kg[i][-60], 10)), round(10 / 59, 10))
  expect_identical(et$kg[i][60], 0)
  expect_identical(attr(e, "units")$soak_h, c(24, 48, NA, 12))
})

test_that("a unit with no ground shares equally, and a unit with no row in units says so", {
  p <- hauls_of(sim_track(1))
  p$lon[p$haul_id %in% 2] <- p$lon[which(p$haul_id %in% 2)[1]]
  p$lat[p$haul_id %in% 2] <- p$lat[which(p$haul_id %in% 2)[1]]
  e <- os_add_effort(p, units = data.frame(vid = 1, haul_id = 2, soak_h = 30))
  expect_equal(e$soak_h[e$haul_id %in% 2], rep(0.5, 60))
  expect_identical(unique(e$share_basis[e$haul_id %in% 2]), "equal: the unit covers no ground")
  expect_identical(unique(e$share_basis[e$haul_id %in% 1]), "no row in units")
  expect_true(all(is.na(e$soak_h[e$haul_id %in% 1])))
})

test_that("os_add_effort() refuses what it cannot do", {
  p <- hauls_of(sim_track(1))
  expect_error(os_add_effort(p[setdiff(names(p), "lon")]), "lacks: lon")
  expect_error(os_add_effort(transform(p, effort_s = 1)), "already has: effort_s")
  expect_error(os_add_effort(p, units = data.frame(haul_id = 1, g = "a")), "must be numeric")
  expect_error(os_add_effort(p, units = data.frame(vid = 1, haul_id = c(1, 1), s = 1:2)), "a unit twice")
})

test_that("os_create_cells() lays every quantity on squares, spread along the track", {
  p <- hauls_of(sim_track(1:2))
  e <- os_add_effort(p, units = data.frame(vid = c(1, 1, 2, 2), haul_id = c(1, 2, 1, 2), soak_h = c(24, 48, 36, 12)))
  e$hauling_h <- e$effort_s / 3600
  e$gear <- ifelse(e$vid == 1, "GNS", "FPO")
  f <- e[!is.na(e$haul_id), ]
  c1 <- os_create_cells(f, c("hauling_h", "soak_h"), by = "gear", unit = "haul_id", square = 0.001)
  expect_named(c1, c("ix", "iy", "gear", "hauling_h", "soak_h", "n"))
  expect_equal(sum(c1$hauling_h), 4 * 59 / 60)
  expect_equal(c(tapply(c1$soak_h, c1$gear, sum)[c("FPO", "GNS")]), c(FPO = 48, GNS = 72))
  expect_identical(sum(c1$n), nrow(f))
  # 1.5 knots east along one parallel for 59 minutes cross about 2.73 km / 80.5 m = 34 squares, each taken
  g <- c1[c1$gear == "GNS", ]
  expect_gte(length(unique(g$ix)), 33)
  expect_lte(attr(c1, "piece_m"), 0.001 * 110574 / 2)
  # without the unit the quantities stay at the pings
  c0 <- os_create_cells(f, "hauling_h", unit = NULL, square = 0.001)
  expect_equal(sum(c0$hauling_h), sum(c1$hauling_h))
  expect_identical(attr(c0, "pieces"), nrow(f))
})

test_that("os_create_cells() makes H3 cells beside squares, where DuckDB's h3 extension loads", {
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  skip_if(inherits(try(ossf:::.os_load_h3(con), silent = TRUE), "try-error"), "no h3 extension here")
  p <- hauls_of(sim_track(1))
  e <- os_add_effort(p)
  e$hauling_h <- e$effort_s / 3600
  f <- e[!is.na(e$haul_id), ]
  c2 <- os_create_cells(f, "hauling_h", unit = "haul_id", square = 0.001, h3 = 11)
  expect_named(c2, c("ix", "iy", "h3", "hauling_h", "n"))
  expect_true(all(nchar(c2$h3) == 15))
  expect_equal(sum(c2$hauling_h), 2 * 59 / 60)
  h <- os_create_cells(f, "hauling_h", unit = "haul_id", square = NULL, h3 = 9)
  expect_named(h, c("h3", "hauling_h", "n"))
  expect_equal(sum(h$hauling_h), 2 * 59 / 60)
})
