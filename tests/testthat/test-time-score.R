# os_add_time_utc() and os_score(). Drafted with AI assistance (Claude), 2026-11-25.

test_that("local readings go on UTC, the clock changes labelled and placed", {
  # Rome, autumn 2023: the clock went from 03:00 summer time back to 02:00 at 01:00 UTC
  x <- data.frame(vid = c(1, 1, 1, 1, 1, 2),
                  time = c("2023-10-29 01:30:00", "2023-10-29 02:30:00", "2023-10-29 02:10:00", "2023-10-29 02:40:00",
                           "2023-10-29 03:10:00", NA))
  y <- os_add_time_utc(x, "time", "Europe/Rome", by = "vid")
  u <- format(y$time, "%H:%M", tz = "UTC")
  expect_equal(u[1:5], c("23:30", "00:30", "01:10", "01:40", "02:10"))   # first pass summer time, second pass winter
  expect_equal(y$time_clock, c("ok", "repeated hour", "repeated hour", "repeated hour", "ok", NA))
  expect_identical(y$time_written, x$time)
  expect_equal(nrow(attr(y, "clock_changes")), 1)
  expect_equal(attr(y, "clock_changes")$rows_written, 3)
  # spring 2023: 02:00-03:00 skipped
  s <- os_add_time_utc(data.frame(time = c("2023-03-26 01:30:00", "2023-03-26 02:30:00", "2023-03-26 03:30:00")), "time", "Europe/Rome")
  expect_equal(format(s$time, "%H:%M", tz = "UTC"), c("00:30", "01:30", "01:30"))
  expect_equal(s$time_clock, c("ok", "skipped hour", "ok"))
  # a date-time read without a zone (held in UTC) gives the same as its text; a zone with no change, no labels
  p <- os_add_time_utc(data.frame(time = as.POSIXct(x$time[1:5], tz = "UTC")), "time", "Europe/Rome")
  expect_equal(p$time[c(1, 5)], y$time[c(1, 5)])
  i <- os_add_time_utc(data.frame(time = x$time[1:5]), "time", "Atlantic/Reykjavik")
  expect_true(all(i$time_clock == "ok"))
  expect_null(attr(i, "clock_changes"))
  expect_error(os_add_time_utc(x, "time", "Europe/Nowhere"), "zone")
})

test_that("a score puts every row in one outcome", {
  x <- data.frame(vid = c(1, 1, 1, 1, 2, 2), call = c(TRUE, TRUE, FALSE, FALSE, TRUE, NA), wit = c(TRUE, FALSE, TRUE, FALSE, TRUE, TRUE))
  s <- os_score(x, "call", "wit")
  expect_equal(unlist(s[c("rows", "both", "call_only", "witness_only", "neither", "not_judged")]),
               c(rows = 6, both = 2, call_only = 1, witness_only = 1, neither = 1, not_judged = 1))
  expect_equal(s$recall, 2 / 3)
  expect_equal(s$precision, 2 / 3)
  expect_equal(s$f1, 2 / 3)
  v <- os_score(x, "call", "wit", by = "vid")
  expect_equal(v$vid, c(1, 2))
  expect_equal(v$rows, c(4, 2))
  expect_equal(v$f1, c(0.5, 1))
  expect_error(os_score(transform(x, call = as.numeric(call)), "call", "wit"), "logical")
})
