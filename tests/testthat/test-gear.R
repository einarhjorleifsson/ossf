# os_add_gear(): the rows and columns it returns, the groups held out together, and the forest's choice of gear.
# Simulated hauls only. Drafted with AI assistance (Claude), 2026-11-26.

sim_gear_hauls <- function(seed = 1) {
  set.seed(seed)
  trips <- data.frame(trip = 1:40, gear = rep(c("Pots", "Nets"), c(30, 10)))
  h <- trips[rep(1:40, each = 5), ]
  h$coast_m <- ifelse(h$gear == "Pots", 5500, 3300) * exp(rnorm(200, 0, 0.3))
  h$soak_h <- ifelse(h$gear == "Pots", 48, 18) * exp(rnorm(200, 0, 0.5))
  h$month <- factor(sample(1:12, 200, replace = TRUE))
  rownames(h) <- NULL
  h
}

test_that("every row comes back in order, with the three columns and a basis", {
  skip_if_not_installed("ranger")
  h <- sim_gear_hauls()
  h$gear[c(3, 77, 150)] <- NA      # gear not known
  h$soak_h[10] <- NA               # a feature missing
  o <- os_add_gear(h, "gear", c("coast_m", "soak_h", "month"), group = "trip", trees = 50, repeats = 0, threads = 1)
  expect_identical(o[names(h)], h)
  expect_true(all(c("gear_pred", "gear_pred_prob", "gear_pred_basis") %in% names(o)))
  expect_identical(o$gear_pred_basis[c(3, 77, 150)], rep("predicted", 3))
  expect_true(is.na(o$gear_pred[10]) && grepl("^none: ", o$gear_pred_basis[10]))
  known <- !is.na(h$gear) & !is.na(h$soak_h)
  expect_true(all(o$gear_pred_basis[known] == "held out"))
  expect_false(anyNA(o$gear_pred[-10]))
})

test_that("a group is held out whole, and the group must be named", {
  skip_if_not_installed("ranger")
  h <- sim_gear_hauls()
  expect_error(os_add_gear(h, "gear", c("coast_m", "soak_h")), "group")
  o <- os_add_gear(h, "gear", c("coast_m", "soak_h"), group = "trip", trees = 50, repeats = 0, threads = 1)
  f <- attr(o, "folds")
  expect_equal(nrow(f), 40)                    # a row a trip: each trip in one fold
  expect_equal(sum(f$hauls), 200)
  expect_true(length(unique(f$fold)) == 5)
})

test_that("the forest weighs each tree's shares, then averages them, and names the most probable gear", {
  skip_if_not_installed("ranger")
  h <- sim_gear_hauls()
  h$gear[seq(1, 200, 7)] <- NA
  o <- os_add_gear(h, "gear", c("coast_m", "soak_h"), group = "trip", trees = 60, repeats = 0, threads = 1)
  p <- attr(o, "probabilities")
  ok <- !is.na(o$gear_pred)
  expect_true(all(abs(rowSums(p[ok, , drop = FALSE]) - 1) < 1e-9))
  expect_identical(o$gear_pred[ok], colnames(p)[max.col(p[ok, , drop = FALSE], ties.method = "first")])
  # the hauls with no known gear are predicted by the model of every known haul: redo its choice by hand
  m <- attr(o, "model")
  un <- which(o$gear_pred_basis == "predicted")
  x <- data.frame(x1 = h$coast_m[un], x2 = h$soak_h[un])
  a <- predict(m, data = x, predict.all = TRUE)$predictions           # hauls x classes x trees
  lv <- colnames(predict(m, data = x)$predictions)
  n <- table(h$gear)[lv]
  w <- sum(n) / (length(n) * as.vector(n))                            # the classes weighed equally
  a <- sweep(a, 2, w, `*`)
  by_tree <- rowMeans(sweep(a, c(1, 3), apply(a, c(1, 3), sum), `/`), dims = 2)
  expect_equal(unname(p[un, lv]), unname(by_tree), tolerance = 1e-9)
  # and it is not the forest's average weighed afterwards, which names the rarer gear more often
  avg <- sweep(rowMeans(predict(m, data = x, predict.all = TRUE)$predictions, dims = 2), 2, w, `*`)
  expect_gte(sum(lv[max.col(avg)] == "Nets"), sum(o$gear_pred[un] == "Nets"))
})

test_that("the logistic regression runs on the same hauls", {
  skip_if_not_installed("nnet")
  h <- sim_gear_hauls()
  o <- os_add_gear(h, "gear", c("coast_m", "soak_h", "month"), group = "trip", method = "logistic", repeats = 0)
  expect_true(all(o$gear_pred_basis == "held out"))
  expect_gt(mean(o$gear_pred == o$gear), 0.6)
})
