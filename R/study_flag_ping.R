# Copied from fishycode's study of plan 086 (2026-11-22-robust-ping-flags), R/proto.R, by data-raw/copy_from_ramb.R: do not edit here.
# The text is the study's working copy, with the names changed (proto_flag_ping() to os_flag_ping(), proto_ to
# .os_, rb_ to os_); a change is made in the study and the copy made again. The help page is ossf's.

# Copies: runs of pings that repeat another stretch of the vessel exactly, at one time shift -------------------------
# A repeat: another ping of the same vessel with exactly the same values in `cols`, at any time; the nearest such ping
# gives the shift. A ping repeating the ping just before it (in the vessel's time order) is a vessel lying still, not
# a repeat. A run: consecutive pings of the vessel repeating at one and the same shift. No shift is assumed.
.os_copy_runs <- function(x, cols, min_run = 2L) {
  key <- do.call(paste, c(list(x$vid), lapply(x[cols], function(v) format(v, digits = 15)), sep = "\r"))
  d <- tibble(.i = seq_len(nrow(x)), vid = x$vid, time = x$time, key = key) |>
    arrange(vid, time) |>
    mutate(still = key == lag(key, default = ""), .by = vid) |>
    mutate(back = as.numeric(difftime(time, lag(time), units = "secs")),
           on = as.numeric(difftime(lead(time), time, units = "secs")), .by = c(key, still)) |>
    mutate(shift = case_when(still ~ NA_real_, is.na(back) ~ on, is.na(on) ~ -back, on < back ~ on, TRUE ~ -back)) |>
    mutate(.r = cumsum(is.na(shift) | is.na(lag(shift)) | shift != lag(shift)), .by = vid) |>
    mutate(run_n = ifelse(is.na(shift), NA_integer_, n()), .by = c(vid, .r)) |>
    arrange(.i)
  tibble(copy_shift_s = d$shift, copy_run_n = d$run_n, copy = !is.na(d$run_n) & d$run_n >= min_run)
}

# The time from a counter: where a counter runs, it times each step; the written time is kept beside ----------------
# A step's counter runs when it advances by more than 0 and by less than two clock grains away from the written step
# (a jump of an hour, a stop with the engine off, or a counter running back is no witness for that step). Within a run
# of such steps the time is the run's first written time plus the counter's advance; across a break it starts again
# from the written time. The written time is off where the written step and the counter's differ by half a grain.
.os_counter_time <- function(x, counter_s, grain_s) {
  d <- tibble(.i = seq_len(nrow(x)), vid = x$vid, time = x$time, c = counter_s, g = grain_s) |>
    arrange(vid, time) |>
    mutate(dt = as.numeric(difftime(time, lag(time), units = "secs")), dh = c - lag(c),
           runs = !is.na(dh) & dh > 0 & abs(dh - dt) < 2 * g,
           seg = cumsum(!runs), .by = vid) |>
    mutate(time_eff = first(time) + cumsum(ifelse(row_number() == 1, 0, dh)), .by = c(vid, seg)) |>
    mutate(time_eff = as.POSIXct(cummax(as.numeric(time_eff)), origin = "1970-01-01", tz = "UTC"),
           time_off = runs & abs(dh - dt) >= g / 2, .by = vid) |>
    arrange(.i)
  tibble(time_eff = d$time_eff, time_off = d$time_off)
}

# Blocks whose written times are off by the counter: two junctions of opposite sign ----------------------------------
# A junction: a step where the counter moves (up or back) and its step differs from the written step by more than two
# clock grains (a stop with the engine off, where the counter stands still, is no junction). Two consecutive junctions
# of a vessel whose differences cancel within two grains each (+D, then about -D), no further apart than D itself (a
# copy written D later cannot outlast D), bound a block whose written times
# disagree with its counter by D against the pings either side: a case study's hour written twice. The block's pings are
# labelled, their positions kept; which side holds the true time the counter alone does not say.
.os_counter_blocks <- function(x, counter_s, grain_s) {
  d <- tibble(.i = seq_len(nrow(x)), vid = x$vid, time = x$time, c = counter_s, g = grain_s) |>
    arrange(vid, time) |>
    mutate(dt = as.numeric(difftime(time, lag(time), units = "secs")), dh = c - lag(c),
           D = dh - dt, junction = (!is.na(dh) & dh != 0 & abs(D) > 2 * g) %in% TRUE, k = row_number(), .by = vid)
  # a block can be no longer than its own shift: a copy written D later cannot outlast D
  jn <- d |> filter(junction) |> mutate(nextD = lead(D), nextk = lead(k), next_t = lead(time), .by = vid) |>
    filter(!is.na(nextD), abs(D + nextD) <= 4 * g,
           as.numeric(difftime(next_t, time, units = "secs")) <= abs(D) + 2 * g)
  d$block_D <- NA_real_
  for (r in seq_len(nrow(jn))) {
    sel <- d$vid == jn$vid[r] & d$k >= jn$k[r] & d$k < jn$nextk[r]
    d$block_D[sel] <- jn$D[r]
  }
  d <- d |> arrange(.i)
  tibble(counter_block = !is.na(d$block_D), counter_block_s = round(d$block_D))
}

# Is the speed column a speed over ground in knots? ------------------------------------------------------------------
# Per vessel, the recorded speed against the step speed between its own pings, on steps where the vessel moves (the
# step longer than twice the noise) and the recorded speeds at the step's two ends agree within 20 % (steady: ramb's
# os_flag_ping_time() measure). The median ratio is the column's own scale against the track. Outside ramb's own band
# for two speeds that agree (0.8-1.25, os_create_ping_limits()' `ceilings_agree`), the column is not a speed over
# ground in knots for that vessel (another unit, a derived or smoothed speed, through the water), and it is not used as
# a witness: the vessel's limits and the speed test go by its track. Returned per vessel, with the ratio and the count.
.os_speed_check <- function(j, lim, band = c(0.8, 1.25), steady = 0.2) {
  nz <- suppressWarnings(lim |> summarise(nz = max(noise_m, na.rm = TRUE), .by = vid))
  nz$nz[!is.finite(nz$nz)] <- 0
  m <- function(a, b, c, e) 111320 * sqrt((c - a)^2 + (cos(a * pi / 180) * (e - b))^2)
  j |> select(vid, time_eff, lon, lat, speed_rec) |> arrange(vid, time_eff) |>
    left_join(nz, by = "vid") |>
    mutate(dt = as.numeric(difftime(time_eff, lag(time_eff), units = "secs")), step = m(lag(lat), lag(lon), lat, lon),
           kn_step = step / dt * 3600 / 1852, r0 = lag(speed_rec), r1 = speed_rec, .by = vid) |>
    filter(dt > 0, step > 2 * nz, !is.na(r0), !is.na(r1), r0 > 0, r1 > 0,
           abs(r1 - r0) <= steady * pmax(r0, r1)) |>
    summarise(n = n(), ratio = stats::median(((r0 + r1) / 2) / kn_step), .by = vid) |>
    mutate(speed_ok = n >= 30 & ratio >= band[1] & ratio <= band[2])
}

# Two witnesses that read no speed limit: where the neighbours put a ping, and its direction ------------------------
# The two-sided prediction: for each ping, the positions a straight line in time between a ping before and a ping after
# gives at its time, for every pair of the `k` before and `k` after (k x k lines); their median position is where the
# neighbours put it, and one bad neighbour cannot move it. `ping_resid_m` is the distance from the ping to it. A smoother
# in the plainest form: a constant velocity across each pair, robust by the median.
# The direction: each step's bearing against the recorded course at its two ends (where a course is recorded); a ping
# out of place has its step in and its step out both pointing away from the course. `ping_dir_in`, `ping_dir_out`:
# degrees between the step and the course, 0-180. And the turn at the ping (between the step in and the step out),
# for data with no course.
.os_witness <- function(x, course = NULL, k = 3L) {
  d <- tibble(.i = seq_len(nrow(x)), vid = x$vid, t = as.numeric(x$time), lat0 = x$lat,
              X = x$lon * cos(x$lat * pi / 180) * 111320, Y = x$lat * 111320,
              crs = if (!is.null(course)) x[[course]] else NA_real_) |>
    arrange(vid, t)
  sh <- function(v, o) if (o < 0) dplyr::lag(v, -o) else dplyr::lead(v, o)
  px <- py <- NULL
  for (a in seq_len(k)) for (b in seq_len(k)) {
    ok <- (sh(d$vid, -a) == d$vid & sh(d$vid, b) == d$vid) %in% TRUE
    ta <- sh(d$t, -a); tb <- sh(d$t, b)
    f <- ifelse(ok & tb > ta, (d$t - ta) / (tb - ta), NA_real_)
    px <- cbind(px, sh(d$X, -a) + f * (sh(d$X, b) - sh(d$X, -a)))
    py <- cbind(py, sh(d$Y, -a) + f * (sh(d$Y, b) - sh(d$Y, -a)))
  }
  d$pred_x <- matrixStats::rowMedians(px, na.rm = TRUE)
  d$pred_y <- matrixStats::rowMedians(py, na.rm = TRUE)
  d$resid <- sqrt((d$X - d$pred_x)^2 + (d$Y - d$pred_y)^2)
  brg <- function(dx, dy) (atan2(dx, dy) * 180 / pi) %% 360
  adiff <- function(a, b) { z <- abs(a - b) %% 360; pmin(z, 360 - z) }
  d <- d |> mutate(b_in = brg(X - lag(X), Y - lag(Y)), b_out = lead(b_in),
                   ok_in = lag(vid) == vid, ok_out = lead(vid) == vid,
                   crs_ok = !is.na(crs) & crs >= 0 & crs < 360,
                   dir_in = ifelse(ok_in & crs_ok & lag(crs_ok), adiff(b_in, (lag(crs) + 0) %% 360), NA_real_),
                   dir_out = ifelse(ok_out & crs_ok & lead(crs_ok), adiff(b_out, lead(crs)), NA_real_),
                   turn = ifelse(ok_in & ok_out, adiff(b_in, b_out), NA_real_)) |>
    arrange(.i)
  tibble(ping_resid_m = d$resid, ping_dir_in = d$dir_in, ping_dir_out = d$dir_out, ping_turn = d$turn)
}

# The neighbour-pair vote: a ping is impossible when most pairs of its neighbours cannot reach it --------------------
# For each ping, every pair of one of its `k` neighbours before and one of its `k` after: the pair votes "reachable" when
# the vessel could go from the one before to the ping and from the ping to the one after, each leg within its limits
# (2 noise + ceiling x (step + time error), as ramb's local allows a leg), and "unreachable" otherwise. A pair whose own
# two pings cannot reach each other is no witness (one of them is likely wrong) and does not vote. The ping is flagged
# when the unreachable votes outnumber the reachable; with no witness pair it is undecided. No anchor, no forward scan:
# a bad ping cannot carry the blame onto the good ones after it. Nothing is fitted: the limits are the vessel's own,
# `k` is a count.
.os_pair_vote <- function(x, lim, k = 3L) {
  l <- suppressWarnings(lim |> summarise(v = max(ceiling_kn, na.rm = TRUE) * 1852 / 3600, te = max(time_error_s, na.rm = TRUE),
                                         nz = max(noise_m, na.rm = TRUE), .by = vid))
  l$te[!is.finite(l$te)] <- 0; l$nz[!is.finite(l$nz)] <- 0
  d <- tibble(.i = seq_len(nrow(x)), vid = x$vid, t = as.numeric(x$time),
              X = x$lon * cos(x$lat * pi / 180) * 111320, Y = x$lat * 111320) |>
    arrange(vid, t) |> left_join(l, by = "vid")
  sh <- function(v, o) if (o < 0) dplyr::lag(v, -o) else dplyr::lead(v, o)
  allow <- function(dt) 2 * d$nz + d$v * (pmax(dt, 0) + d$te)
  dist <- function(x1, y1, x2, y2) sqrt((x2 - x1)^2 + (y2 - y1)^2)
  yes <- no <- rep(0L, nrow(d))
  for (a in seq_len(k)) for (b in seq_len(k)) {
    ok <- (sh(d$vid, -a) == d$vid & sh(d$vid, b) == d$vid) %in% TRUE
    xa <- sh(d$X, -a); ya <- sh(d$Y, -a); ta <- sh(d$t, -a)
    xb <- sh(d$X, b); yb <- sh(d$Y, b); tb <- sh(d$t, b)
    witness <- ok & dist(xa, ya, xb, yb) <= allow(tb - ta)
    reach <- dist(xa, ya, d$X, d$Y) <= allow(d$t - ta) & dist(d$X, d$Y, xb, yb) <= allow(tb - d$t)
    yes <- yes + (witness & reach) %in% TRUE
    no <- no + (witness & !reach) %in% TRUE
  }
  d$vote <- case_when(no > yes ~ "unreachable", yes + no == 0 ~ "no witness", TRUE ~ "reachable")
  d$yes <- yes; d$no <- no
  d <- d |> arrange(.i)
  tibble(pair_vote = d$vote, pair_yes = d$yes, pair_no = d$no)
}

# A vessel's speed at its cadence with its clock and noise folded in (plan 086 §2c) ---------------------------------
.os_kn_clock <- function(lim) {
  lim |> filter(!is.na(ceiling_kn)) |>
    mutate(kn = ceiling_kn * (dt_median_s + time_error_s) / dt_median_s + 2 * noise_m / dt_median_s * 3600 / 1852) |>
    summarise(kn = max(kn), .by = vid)
}

# The speed test, one variant ---------------------------------------------------------------------------------------
# `method`: "clean25" (ramb's clean at 25 kn, as fishycode's production); "clean_own" (clean, each vessel at its own
# speed with its clock and noise); "local_limits" (ramb's local with the limits, no recorded speed); "local_limits_speed"
# (the same with the recorded speed). `by`: "vessel" (each vessel's whole track) or "trip" (each run in port or at sea
# judged alone). Returns the flag for each row of `j`; with `evidence = TRUE` (plan 086 step A2, 2026-11-23) a data
# frame of ramb's own evidence for each row instead (os_flag_ping_impossible(evidence = TRUE): `ping_flag`,
# `ping_flag_basis`, `ping_flag_side`, `ping_flag_speed_kn` and the legs), for the methods that call ramb's local; the
# others give the flag with the evidence NA.
.os_speed_test <- function(j, method, anchor = "kept", by = "vessel", lim, evidence = FALSE) {
  j$vid0 <- j$vid
  if (by == "trip") j$vid <- paste(j$vid, j$ping_trip, sep = ":")
  # a clock jump (.os_time_test()) starts the test afresh: no leg across it is judged
  if ("clockseg" %in% names(j)) j$vid <- paste(j$vid, j$clockseg, sep = "~")
  key <- distinct(j, vid, vid0)
  lim_j <- key |> inner_join(lim |> rename(vid0 = vid), by = "vid0", relationship = "many-to-many") |> select(-vid0)
  base <- j |> select(.i, vid, time = time_eff, lon, lat, any_of(c("speed_rec", "provider", "stream")))
  feed_arg <- if ("provider" %in% names(base)) quote(provider) else NULL
  run <- function(d, ...) {
    if (is.null(feed_arg)) os_flag_ping_impossible(d, ..., anchor = anchor)
    else os_flag_ping_impossible(d, ..., feed = provider, noise_nm = 0.3, anchor = anchor)
  }
  f <- switch(method,
    clean25 = run(base, method = "clean", kn_max = 25, min_dt_s = 10),
    clean_own = {
      kn <- .os_kn_clock(lim)
      bind_rows(lapply(split(base, j$vid0), function(d) {
        k <- kn$kn[kn$vid == j$vid0[match(d$.i[1], j$.i)]]
        run(d, method = "clean", kn_max = if (length(k)) k else 25, min_dt_s = 10)
      }))
    },
    local_limits = run(base, method = "local", limits = lim_j, evidence = evidence),
    local_limits_veto = {
      f <- run(base, method = "local", limits = lim_j)
      pv <- .os_pair_vote(base, lim_j)
      fl0 <- f$ping_flag[match(base$.i, f$.i)]
      fl0[pv$pair_vote == "reachable"] <- NA
      tibble(.i = base$.i, ping_flag = fl0)
    },
    local_limits_speed_veto = {
      f <- run(base |> rename(spd = speed_rec), method = "local", limits = lim_j, speed = spd)
      pv <- .os_pair_vote(base, lim_j)
      fl0 <- f$ping_flag[match(base$.i, f$.i)]
      fl0[pv$pair_vote == "reachable"] <- NA
      tibble(.i = base$.i, ping_flag = fl0)
    },
    local_limits_speed_resid = {
      # a speed flag dropped where the ping lies within one step's reach of where its neighbours put it (the two-sided
      # prediction): 2 noise + ceiling x (the vessel's usual step + its time error)
      f <- run(base |> rename(spd = speed_rec), method = "local", limits = lim_j, speed = spd)
      w <- .os_witness(base |> transmute(vid, time, lon, lat))
      l <- suppressWarnings(lim_j |> summarise(reach = max(2 * noise_m + ceiling_kn * 1852 / 3600 * (dt_median_s + time_error_s),
                                                           na.rm = TRUE), .by = vid))
      fl0 <- f$ping_flag[match(base$.i, f$.i)]
      fl0[(w$ping_resid_m <= l$reach[match(base$vid, l$vid)]) %in% TRUE] <- NA
      tibble(.i = base$.i, ping_flag = fl0)
    },
    pair_vote = {
      pv <- .os_pair_vote(base, lim_j)
      tibble(.i = base$.i, ping_flag = ifelse(pv$pair_vote == "unreachable", "pair_vote", NA_character_))
    },
    local_limits_speed = run(base |> rename(spd = speed_rec), method = "local", limits = lim_j, speed = spd,
                             evidence = evidence)
  )
  if (!evidence) return(f$ping_flag[match(j$.i, f$.i)])
  ev <- c("ping_flag", "ping_flag_basis", "ping_flag_side", "ping_flag_speed_kn", "ping_flag_prev_dt_s",
          "ping_flag_prev_dist_m", "ping_flag_next_dt_s", "ping_flag_next_dist_m")
  out <- as.data.frame(f)[match(j$.i, f$.i), intersect(ev, names(f)), drop = FALSE]
  for (v in setdiff(ev, names(out))) out[[v]] <- NA
  rownames(out) <- NULL
  out[ev]
}

# Each ping's legs to the nearest unflagged ping before and after it, with their allowance and excess -----------------
# Plan 086 step A2 (2026-11-23): what ramb's evidence lacks, the allowance of each leg and the leg's length over it (the
# excess), for every ping, flagged or not, so a reader can see how far over or under its limits each ping is and cut
# again without a new run. The allowance is ramb's local rule with `limits` as its engine and evidence take it
# (R/ping.R, .os_flag_evidence()): twice the noise plus the speed times the step (at least `min_dt_s`) and the time
# error, the larger of the leg's two ends for each; the speed is 1.5 times the local speed plus 5 kn where the rule
# judged the ping by a recorded speed (`speed_kn`, ramb's `ping_flag_speed_kn`), else the vessel's ceiling. Where the
# limits have no value, ramb's fallbacks (50 m, `kn_max`, no time error). A leg longer than `max_gap_h` is not judged
# by the rule: its excess is given, and left out of `excess`, the larger of the judged legs'. `group`: the runs judged
# apart (the vessel, or the vessel's run in port or at sea). `flagged`: the pings the rule set aside in that view.
# Nothing here is a setting of its own: every constant is ramb's default for `local`.
.os_leg_excess <- function(j, flagged, group, lim, speed_kn = NULL, min_dt_s = 10, kn_max = 25, factor = 1.5,
                             margin_kn = 5, margin_m = 50, max_gap_h = 4, noise_nm = 0.3) {
  n <- nrow(j)
  by_stream <- "stream" %in% names(lim) && "stream" %in% names(j)
  key <- if (by_stream) paste(j$vid, j$stream, sep = "\r") else as.character(j$vid)
  lkey <- if (by_stream) paste(lim$vid, lim$stream, sep = "\r") else as.character(lim$vid)
  m <- match(key, lkey)
  kn <- lim$ceiling_kn[m] * 0.514444; kn[is.na(kn)] <- kn_max * 0.514444
  m0 <- 2 * lim$noise_m[m]; m0[is.na(m0)] <- margin_m
  te <- pmax(lim$grain_s[m], lim$time_error_s[m], na.rm = TRUE); te[is.na(te)] <- 0
  v <- if (is.null(speed_kn)) rep(NA_real_, n) else speed_kn * 0.514444
  o <- order(group, as.numeric(j$time_eff), j$lon, j$lat)
  g <- group[o]; t <- as.numeric(j$time_eff[o]); lo <- j$lon[o]; la <- j$lat[o]
  fd <- if ("provider" %in% names(j)) as.character(j$provider[o]) else rep(NA_character_, n)
  kn <- kn[o]; m0 <- m0[o]; te <- te[o]; v <- v[o]
  kept <- !flagged[o]
  idx <- seq_len(n)
  # the nearest unflagged ping strictly before and strictly after, in the same group (as ramb's evidence finds them)
  pk <- c(0L, cummax(ifelse(kept, idx, 0L)))[idx]
  nk <- c(rev(cummin(rev(ifelse(kept, idx, n + 1L)))), n + 1L)[idx + 1L]
  pk[pk == 0L | g[pmax(pk, 1L)] != g] <- NA_integer_
  nk[nk > n | g[pmin(nk, n)] != g] <- NA_integer_
  leg <- function(jj) {
    ok <- !is.na(jj)
    dt <- dist <- allow <- rep(NA_real_, n)
    a <- idx[ok]; b <- jj[ok]
    dist[ok] <- os_calc_distance(lo[a], la[a], lo[b], la[b])
    dt[ok] <- abs(t[a] - t[b])
    sp <- ifelse(is.na(v[a]), pmax(kn[a], kn[b]), factor * v[a] + margin_kn * 0.514444)
    allow[ok] <- pmax(m0[a], m0[b]) + sp * (pmax(dt[ok], min_dt_s) + pmax(te[a], te[b]))
    # ramb never judges a leg between two feeds within the noise floor (its `feed`, `noise_nm`)
    noise <- rep(FALSE, n)
    noise[ok] <- !is.na(fd[a]) & !is.na(fd[b]) & fd[a] != fd[b] & dist[ok] <= noise_nm * 1852
    data.frame(dt_s = dt, dist_m = dist, allow_m = allow, excess = dist / allow, noise = noise)
  }
  p <- leg(pk); q <- leg(nk)
  judged <- function(l) ifelse(!is.na(l$dt_s) & l$dt_s <= max_gap_h * 3600 & !l$noise, l$excess, NA_real_)
  ex <- pmax(judged(p), judged(q), na.rm = TRUE)
  r <- data.frame(ping_flag_prev_dt_s = p$dt_s, ping_flag_prev_dist_m = p$dist_m, ping_flag_prev_allow_m = p$allow_m,
                  ping_flag_prev_excess = p$excess, ping_flag_next_dt_s = q$dt_s, ping_flag_next_dist_m = q$dist_m,
                  ping_flag_next_allow_m = q$allow_m, ping_flag_next_excess = q$excess, ping_flag_excess = ex)
  r[o, ] <- r
  r
}

# Where the kept pings either side put a ping, and how far it lies from there ----------------------------------------
# Plan 086 step A2: the two-sided prediction of .os_witness(), with the unflagged pings alone as neighbours, so a
# flagged ping is measured against the track that is left and never against another flagged ping. For each ping, the
# `k` unflagged pings before it and the `k` after it in its group (itself never one of them); every pair gives the
# position a straight line in time puts at the ping's time; their median is where the neighbours put it.
# `ping_resid_m` is the distance to it; `ping_resid_rel` the same over the vessel's median for its unflagged pings, so
# it reads alike on every device (1 is a usual ping). The median is taken over the distances that are not exactly 0: a
# device that holds its position while the vessel is still (a case study's) writes exact zeros, and over them the vessel's usual
# would be 0 (found on a case study, 2026-11-23).
.os_neighbour_distance <- function(j, flagged, group, k = 3L) {
  n <- nrow(j)
  o <- order(group, as.numeric(j$time_eff), j$lon, j$lat)
  g <- group[o]; t <- as.numeric(j$time_eff[o]); la0 <- j$lat[o]
  X <- j$lon[o] * cos(la0 * pi / 180) * 111320; Y <- la0 * 111320
  K <- which(!flagged[o])
  nb <- findInterval(seq_len(n) - 0.5, K)   # unflagged pings strictly before
  na <- findInterval(seq_len(n), K)         # unflagged pings at or before (a kept ping counts itself)
  px <- py <- matrix(NA_real_, n, k * k)
  col <- 0L
  for (a in seq_len(k)) for (b in seq_len(k)) {
    col <- col + 1L
    ia <- nb - a + 1L; ib <- na + b
    ok <- ia >= 1L & ib <= length(K)
    pa <- pb <- rep(NA_integer_, n)
    pa[ok] <- K[ia[ok]]; pb[ok] <- K[ib[ok]]
    ok <- ok & g[pmax(pa, 1L)] == g & g[pmax(pb, 1L)] == g
    ok[is.na(ok)] <- FALSE
    f <- rep(NA_real_, n)
    f[ok] <- (t[ok] - t[pa[ok]]) / (t[pb[ok]] - t[pa[ok]])
    f[!is.finite(f)] <- NA_real_
    px[ok, col] <- X[pa[ok]] + f[ok] * (X[pb[ok]] - X[pa[ok]])
    py[ok, col] <- Y[pa[ok]] + f[ok] * (Y[pb[ok]] - Y[pa[ok]])
  }
  rx <- suppressWarnings(matrixStats::rowMedians(px, na.rm = TRUE))
  ry <- suppressWarnings(matrixStats::rowMedians(py, na.rm = TRUE))
  res <- sqrt((X - rx)^2 + (Y - ry)^2)
  out <- rep(NA_real_, n); out[o] <- res
  use <- !flagged & !is.na(out) & out > 0
  med <- tapply(out[use], j$vid[use], stats::median)
  data.frame(ping_resid_m = out, ping_resid_rel = out / as.numeric(med[as.character(j$vid)]))
}

# The time test: steps too long for their written time, on the line, between two stretches at one speed -------------
# For each step, the time it should have taken: its length over the vessel's speed around it, the median step speed
# of the `k` steps before and of the `k` after (the track's own speed, not the device's snapshot). A step is a candidate
# when: the vessel moves (it covers more than twice its noise in its step); the speed before and after agree (within
# the same 20 % that ramb's os_flag_ping_time() takes as steady), so it is not a change of speed; the step's extra time
# (its length over that speed, less its written step) exceeds the vessel's clock error and grain; the step is longer
# than its limits allow (2 noise + speed x (step + time error)); and the pings are on the line (the ping before and the
# ping itself near the line from two before to one after, in order along it: not out and back; "near" is the vessel's
# own wobble under way, its 95th percentile of that distance, and never under twice its noise).
# Consecutive candidates are one event. By the step after it:
#   a clock jump   that step is normal (its extra time within the clock error and grain): the offset persists, the
#                  positions are good, the written times on one side are off; the speed test starts afresh at each of
#                  the event's pings;
#   time off       that step is short by about the event's extra time (a time written late or early, then right): the
#                  event's pings are set aside as references, their positions kept.
# Nothing is fitted: the noise, the time error and the grain are the vessel's own (os_create_ping_limits()); `k` is a
# count of steps, and 20 % is ramb's own measure of steady.
.os_time_test <- function(j, lim, k = 3L, steady = 0.2) {
  l <- suppressWarnings(lim |> summarise(te = max(time_error_s, na.rm = TRUE), g = max(grain_s, na.rm = TRUE),
                                         nz = max(noise_m, na.rm = TRUE), .by = vid))
  l$te[!is.finite(l$te)] <- 0; l$g[!is.finite(l$g)] <- 1; l$nz[!is.finite(l$nz)] <- 0
  m <- function(a, b, c, e) 111320 * sqrt((c - a)^2 + (cos(a * pi / 180) * (e - b))^2)
  d <- j |> select(.i, vid, t = time_eff, lon, lat) |> arrange(vid, t) |>
    left_join(l, by = "vid") |>
    mutate(dt = as.numeric(difftime(t, lag(t), units = "secs")), step = m(lag(lat), lag(lon), lat, lon),
           s = ifelse(dt > 0, step / dt, NA_real_), .by = vid)
  sh <- function(v, o) if (o < 0) dplyr::lag(v, -o) else dplyr::lead(v, o)
  side <- function(offs) {
    sm <- do.call(cbind, lapply(offs, function(o) { v <- sh(d$s, o); ifelse((sh(d$vid, o) == d$vid) %in% TRUE, v, NA_real_) }))
    matrixStats::rowMedians(sm, na.rm = TRUE)
  }
  d$v_b <- side(-k:-1)
  d$v_a <- side(1:k)
  d <- d |> mutate(
    v = (v_b + v_a) / 2,
    steady_ok = !is.na(v_b) & !is.na(v_a) & abs(v_a - v_b) <= steady * pmax(v_a, v_b),
    terr = ifelse(v > 0, step / v - dt, NA_real_),
    moving = !is.na(v) & v * dt > 2 * nz,
    too_long = !is.na(step) & step > 2 * nz + v * (dt + te))
  ln <- d |> mutate(ax = lag(lon, 2), ay = lag(lat, 2), bx = lead(lon), by = lead(lat),
                    px = lag(lon), py = lag(lat), same = lag(vid, 2) == vid & lead(vid) == vid)
  off_t <- function(x0, y0, ax, ay, bx, by) {
    c0 <- cos(ay * pi / 180)
    ux <- (bx - ax) * c0; uy <- by - ay; px <- (x0 - ax) * c0; py <- y0 - ay
    L2 <- ux^2 + uy^2
    tt <- ifelse(L2 > 0, (px * ux + py * uy) / L2, NA_real_)
    list(off = 111320 * sqrt((px - tt * ux)^2 + (py - tt * uy)^2), t = tt)
  }
  o1 <- off_t(ln$px, ln$py, ln$ax, ln$ay, ln$bx, ln$by)
  o2 <- off_t(ln$lon, ln$lat, ln$ax, ln$ay, ln$bx, ln$by)
  # the track's own wobble under way: each vessel's 95th percentile of how far a ping lies off the line from two pings
  # before to one after (most pings are good, so this measures the vessel's ordinary track, not its faults)
  d$off_i <- o2$off
  d$off_b <- o1$off; d$t_b <- o1$t; d$t_i <- o2$t
  d <- d |> mutate(tol = pmax(2 * nz, stats::quantile(off_i[moving %in% TRUE], 0.95, na.rm = TRUE, names = FALSE)),
                   .by = vid)
  d$on_line <- (ln$same %in% TRUE) & o1$off <= d$tol & o2$off <= d$tol & o1$t > 0 & o2$t > o1$t & o2$t < 1
  d <- d |>
    mutate(cand = (moving & steady_ok & too_long & on_line & terr > te + g) %in% TRUE, .by = vid) |>
    mutate(ev = cumsum(cand & !lag(cand, default = FALSE)), .by = vid) |>
    mutate(after = lead(terr), last_in_ev = cand & !lead(cand, default = FALSE), .by = vid)
  evs <- d |> filter(cand) |>
    summarise(J = sum(terr), after = after[last_in_ev][1], te = first(te), g = first(g), .by = c(vid, ev)) |>
    mutate(kind = case_when(!is.na(after) & abs(after) <= te + g ~ "time: a clock jump",
                            !is.na(after) & after < -(J - te - g) ~ "time off",
                            TRUE ~ NA_character_))
  d <- d |> left_join(evs |> select(vid, ev, kind, J), by = c("vid", "ev")) |>
    mutate(time_fault = ifelse(cand, kind, NA_character_),
           clockseg = cumsum(time_fault %in% "time: a clock jump"), .by = vid) |>
    arrange(.i)
  res <- tibble(.i = d$.i, time_fault = d$time_fault, jump_s = ifelse(is.na(d$time_fault), NA_real_, round(d$J, 1)),
                clockseg = d$clockseg)
  attr(res, "detail") <- d          # every test's value per step, for reading a case
  res
}

# Back onto the cleaned track: does a ping a step set aside fit between the pings kept either side? ------------------
# For each set-aside ping, the last kept ping before it and the first after it (same vessel, in time): it fits when both
# legs are within what the vessel's limits allow, 2 noise + ceiling x (step + time error), as `limits` allows a leg in
# ramb's local. Nothing is fitted: the limits are the vessel's own. Of a copied pair, the original usually fits and the
# copy usually does not; a record without a fix fits when its stale position is still where the vessel was.
.os_fit_back <- function(all, kept, lim) {
  l <- suppressWarnings(lim |> summarise(v = max(ceiling_kn, na.rm = TRUE) * 1852 / 3600,
                                         te = max(time_error_s, na.rm = TRUE), nz = max(noise_m, na.rm = TRUE), .by = vid))
  k <- all[kept, c("vid", "time_eff", "lon", "lat")]
  o <- all[!kept, c(".i", "vid", "time_eff", "lon", "lat")]
  if (!nrow(o)) return(logical(0))
  fit <- rep(NA, nrow(o))
  for (v in unique(o$vid)) {
    kv <- k[k$vid == v, ]; kv <- kv[order(kv$time_eff), ]
    ov <- which(o$vid == v)
    if (nrow(kv) < 1) next
    tk <- as.numeric(kv$time_eff); to <- as.numeric(o$time_eff[ov])
    b <- findInterval(to, tk)                       # the kept ping at or before
    a <- b + 1                                      # the kept ping after
    lv <- l[l$vid == v, ]
    if (nrow(lv) != 1 || !is.finite(lv$v)) next
    if (!is.finite(lv$te)) lv$te <- 0
    if (!is.finite(lv$nz)) lv$nz <- 0
    leg_ok <- function(idx) {
      ok <- rep(NA, length(idx))
      has <- idx >= 1 & idx <= nrow(kv)
      dt <- abs(to[has] - tk[idx[has]])
      dm <- 111320 * sqrt((o$lat[ov][has] - kv$lat[idx[has]])^2 +
                            (cos(o$lat[ov][has] * pi / 180) * (o$lon[ov][has] - kv$lon[idx[has]]))^2)
      ok[has] <- dm <= 2 * lv$nz + lv$v * (dt + lv$te)
      ok
    }
    ob <- leg_ok(b); oa <- leg_ok(a)
    fit[ov] <- dplyr::case_when(is.na(ob) & is.na(oa) ~ NA, is.na(ob) ~ oa, is.na(oa) ~ ob, TRUE ~ ob & oa)
  }
  fit
}

#' Flag the pings in one call, every ping in one outcome
#'
#' Runs the flags in a fixed order and gives each ping one outcome and the step that decided it. Nothing is
#' dropped: a ping a step labels is kept with its label, and later steps only stop using it as a reference. The
#' order: what the record says of itself (an invalid position, a row no device wrote, a position with no fix);
#' copies (a stretch written again at one time shift, one report twice across feeds); the time (a counter that
#' runs apart from the written time, a clock jump); then the speed test on what is left, in port and at sea, at
#' each vessel's own limits ([os_create_ping_limits()], [os_flag_ping_impossible()]). A ping no step labels is
#' `"kept"`.
#'
#' The code is the working copy of fishycode's study of plan 086 (its `proto_flag_ping()`), taken as it is: bound
#' for ramb, not yet in it, and not reviewed by a person. It names no dataset's values: the record's own witnesses
#' are given by argument, each a column's name.
#'
#' @param x A data frame of pings: `vid`, `time`, `lon`, `lat`.
#' @param speed,course The recorded speed (knots) and course, as column names, or `NULL`.
#' @param fix A logical column, `TRUE` where the device had a fix, or `NULL`.
#' @param synthetic A logical column, `TRUE` on a row no device wrote (a fill), or `NULL`.
#' @param counter_s A column that counts seconds apart from the written time (an engine-hours counter), or `NULL`.
#' @param in_port A logical column, `TRUE` in a port (from [os_flag_ping_polygon()]), or `NULL`.
#' @param feed,stream The provider and the stream of each ping, as column names, or `NULL`.
#' @param method The speed test: `"local_limits"` (each vessel's own limits and the track around a ping),
#'   `"local_limits_speed"` (the same, judged by the recorded speed), or another the study tried (`"clean25"`,
#'   `"clean_own"`, `"pair_vote"`, and the `_veto` and `_resid` forms of the first two).
#' @param anchor Which side of a jump is believed: `"kept"` (the pings before it) or `"vote"` (the pings after
#'   it decide).
#' @param by `"vessel"` (each vessel's whole track), `"trip"` (each run in port or at sea alone) or `"both"`,
#'   which keeps the two views apart in the outcome.
#' @param steps The steps run besides the speed test: of `"record"`, `"copies"`, `"counter"`,
#'   `"counter_blocks"`, `"zero_speed_na"`, `"speed_check"`, `"time"`.
#' @param copy_min_run The fewest pings in a row that make a copied stretch.
#' @param copy_given A logical column with the copies already found, or `NULL` to find them here.
#' @param feed_priority The feeds, the one to keep first, where a report came twice.
#' @param evidence `TRUE` to add what decided each flagged ping (the side, the local speed, each leg's excess).
#' @param limits_from A logical column: the rows each vessel's limits are measured on (leave out a device carried
#'   on land), or `NULL` for every row. The rows left out are judged as any other.
#'
#' @return `x`, the same rows in the same order, with `ping_outcome` (one a ping), `ping_step` (the step that
#'   decided it), `ping_flag_basis`, `ping_fits` (whether a ping set aside fits the kept track), `ping_speed_stage`,
#'   `ping_time_off`, `ping_time_jump_s`, `ping_trip` (the run in port or at sea) and `time_eff` (the time the speed
#'   test used). Each vessel's limits are the attribute `"limits"`.
#' @import dplyr
#' @export
os_flag_ping <- function(x, speed = NULL, course = NULL, fix = NULL, synthetic = NULL, counter_s = NULL,
                            in_port = NULL, feed = NULL, stream = NULL, method = "local_limits", anchor = "kept",
                            by = "vessel", steps = c("record", "copies", "counter"), copy_min_run = 2L,
                            copy_given = NULL, feed_priority = NULL, evidence = FALSE, limits_from = NULL) {
  n <- nrow(x)
  d <- tibble(.i = seq_len(n), vid = as.character(x$vid), time = x$time, lon = x$lon, lat = x$lat)
  # `limits_from`: a logical column of x, the rows each vessel's limits (its ceiling, clock, noise) are measured on,
  # where some rows say nothing of what the vessel can do: a device carried on land (the maintainer, 2026-11-24: "The
  # only thing we have to worry about here is if we keep it in when we check what a 'vessels' is capable of, like the
  # maximum speed reported"). Default: every row still to judge. The rows left out are judged as any other.
  d$limits_from <- if (!is.null(limits_from)) x[[limits_from]] %in% TRUE else TRUE
  if (!is.null(speed)) d$speed_rec <- x[[speed]]
  if (!is.null(course)) d$course <- x[[course]]
  if (!is.null(feed)) d$provider <- as.character(x[[feed]])
  if (!is.null(stream)) d$stream <- as.character(x[[stream]])
  d$in_port <- if (!is.null(in_port)) x[[in_port]] else NA
  out <- rep(NA_character_, n)
  step <- rep(NA_character_, n)
  basis <- rep(NA_character_, n)   # the witness that decided a ping's outcome (plan 086 step A2); NA for a kept ping
  set <- function(rows, what, s, b = NA_character_) {
    new <- rows & is.na(out)
    out[new] <<- what
    step[new] <<- s
    basis[new] <<- if (length(b) == 1L) b else b[new]
  }

  # 1. what the record says of itself
  inv <- os_flag_ping_invalid(d |> select(.i, vid, time, lon, lat), rules = "lon_eq_lat")
  set(!is.na(inv$ping_flag[match(d$.i, inv$.i)]), "invalid", "1 record", "lon_eq_lat")
  if ("record" %in% steps) {
    if (!is.null(synthetic)) set(x[[synthetic]] %in% TRUE, "synthetic", "1 record", "synthetic")
    if (!is.null(fix)) set(x[[fix]] %in% FALSE, "no fix", "1 record", "no_fix")
  }

  # 2. copies: runs repeating another stretch at one shift; and one report twice across feeds (ramb)
  if ("copies" %in% steps) {
    # `copy_given`: a logical column of x with the copies already found by .os_copy_runs() (a speed-up for grids)
    cp_copy <- if (!is.null(copy_given)) x[[copy_given]] %in% TRUE else
      .os_copy_runs(d, intersect(c("lon", "lat", "speed_rec", "course"), names(d)), copy_min_run)$copy
    set(cp_copy, "copy", "2 copies", "repeat_run")
    if (!is.null(feed) && length(unique(d$provider)) > 1) {
      ok <- is.na(out)
      du <- os_flag_ping_duplicate(d[ok, ] |> select(.i, vid, time, lon, lat, provider), feed = provider,
                                   priority = if (!is.null(feed_priority)) intersect(feed_priority, unique(d$provider)) else
                                     sort(unique(d$provider)), max_dt_s = 10, kn_max = 25, n_neighbours = 3)
      dup <- rep(FALSE, n)
      dup[du$.i[!is.na(du$ping_flag)]] <- TRUE
      set(dup, "duplicate", "2 copies", "priority")
    }
  }

  # the limits, on the pings still to judge (written time first: the clock's grain)
  judged <- is.na(out)
  j <- d[judged, ]
  lim_args <- function(jj) {
    jj <- jj[jj$limits_from, ]
    a <- list(jj |> select(vid, time_eff, lon, lat, any_of(c("stream", "speed_rec"))) |> rename(time = time_eff))
    a$stream <- if ("stream" %in% names(jj)) quote(stream) else NULL
    a$speed <- if ("speed_rec" %in% names(jj)) quote(speed_rec) else NULL
    do.call(os_create_ping_limits, Filter(Negate(is.null), a))
  }
  j$time_eff <- j$time
  lim <- lim_args(j)
  # a recorded speed of 0 is the column's floor (below what it can show), not a witness that the vessel stood still:
  # it is set aside, and the test falls back on the speeds around or the vessel's ceiling
  if ("zero_speed_na" %in% steps && "speed_rec" %in% names(j)) j$speed_rec[j$speed_rec %in% 0] <- NA_real_
  speed_check <- NULL
  if ("speed_check" %in% steps && "speed_rec" %in% names(j)) {
    speed_check <- .os_speed_check(j, lim)
    bad <- setdiff(unique(j$vid), speed_check$vid[speed_check$speed_ok])
    if (length(bad)) {
      j$speed_rec[j$vid %in% bad] <- NA_real_
      lim <- lim_args(j)
    }
  }

  # 3. the counter's time
  time_off <- rep(FALSE, n)
  if ("counter_blocks" %in% steps && !is.null(counter_s)) {
    g <- lim |> summarise(g = max(grain_s, na.rm = TRUE), .by = vid)
    cb <- .os_counter_blocks(j, x[[counter_s]][judged], g$g[match(j$vid, g$vid)])
    set(seq_len(n) %in% j$.i[cb$counter_block], "time: a block off by the counter", "3 counter", "counter")
    j <- j[!cb$counter_block, ]
    judged <- seq_len(n) %in% j$.i
  }
  if ("counter" %in% steps && !is.null(counter_s)) {
    g <- lim |> summarise(g = max(grain_s, na.rm = TRUE), .by = vid)
    ct <- .os_counter_time(j, x[[counter_s]][judged], g$g[match(j$vid, g$vid)])
    j$time_eff <- ct$time_eff
    time_off[judged] <- ct$time_off
    lim <- lim_args(j)
  }

  # 3b. the time test: a clock jump starts the speed test afresh; a single time off is set aside as a reference
  jump_s <- rep(NA_real_, n)
  if ("time" %in% steps) {
    tt <- .os_time_test(j, lim)
    j$clockseg <- tt$clockseg[match(j$.i, tt$.i)]
    tf <- tt$time_fault[match(j$.i, tt$.i)]
    jump_s[j$.i] <- tt$jump_s[match(j$.i, tt$.i)]
    set(seq_len(n) %in% j$.i[tf %in% "time: a clock jump"], "time: a clock jump", "3 time", "track_speed")
    set(seq_len(n) %in% j$.i[tf %in% "time off"], "time off", "3 time", "track_speed")
    j <- j[!(tf %in% "time off"), ]
  }

  # 4. in port or at sea, and the trips (runs of one or the other, per vessel, over the pings to judge)
  j <- j |> arrange(vid, time_eff) |>
    mutate(ping_trip = if (all(is.na(in_port))) 1L else cumsum(coalesce(in_port != lag(in_port), TRUE)), .by = vid) |>
    arrange(.i)

  # 5. the speed test on every ping still to judge, in port and at sea. `by = "both"` runs it over each vessel's whole
  # track and over each run in port or at sea alone, and keeps both views (the maintainer: a jump into port and back
  # "may tell us that something is still wrong"; never delete anything)
  j <- j |> arrange(vid, time_eff) |>
    mutate(edge = !is.na(in_port) & (in_port != lag(in_port, default = NA) | in_port != lead(in_port, default = NA)) %in% TRUE,
           .by = vid) |> arrange(.i)
  # the groups judged apart in each view: the vessel's whole track (cut at a clock jump), and each run in port or at sea
  gw <- if ("clockseg" %in% names(j)) paste(j$vid, j$clockseg, sep = "~") else j$vid
  gt <- paste(gw, j$ping_trip, sep = ":")
  on <- function(v) seq_len(n) %in% j$.i[v]
  sb <- rep("speed", n)                        # the speed flag's basis: ramb's, where its evidence was asked for
  if (by == "both") {
    eva <- .os_speed_test(j, method, anchor, "vessel", lim, evidence = evidence)
    evb <- .os_speed_test(j, method, anchor, "trip", lim, evidence = evidence)
    fa <- if (evidence) eva$ping_flag else eva
    fb <- if (evidence) evb$ping_flag else evb
    fl <- coalesce(fa, fb)
    use_b <- is.na(fa) & !is.na(fb)            # flagged by trip only: that view's evidence; else the whole track's
    if (evidence) {
      ev <- eva; ev[use_b, ] <- evb[use_b, ]
      sb[j$.i] <- coalesce(ev$ping_flag_basis, "speed")
    }
    set(on(!is.na(fa) & !is.na(fb)), "flagged", "5 speed", sb)
    set(on(!is.na(fa) & is.na(fb) & j$edge), "a jump at the port's edge", "5 speed", sb)
    set(on(!is.na(fa) & is.na(fb)), "flagged on the whole track only", "5 speed", sb)
    set(on(is.na(fa) & !is.na(fb)), "flagged by trip only", "5 speed", sb)
  } else {
    eva <- .os_speed_test(j, method, anchor, by, lim, evidence = evidence)
    fl <- if (evidence) eva$ping_flag else eva
    use_b <- rep(FALSE, nrow(j))
    if (evidence) { ev <- eva; sb[j$.i] <- coalesce(ev$ping_flag_basis, "speed") }
    set(on(!is.na(fl) & j$edge), "flagged at the port's edge", "5 speed", sb)
    set(on(!is.na(fl)), "flagged", "5 speed", sb)
  }
  set(rep(TRUE, n), "kept", "kept")

  # 5b. the evidence of every judged ping (plan 086 step A2): ramb's own (the side, the local speed), then what it
  # lacks, each leg's allowance and excess (.os_leg_excess()), from the view that flagged the ping, the whole track
  # for a kept one. Under a recorded speed the allowance of a kept ping is not known (ramb gives the local speed for
  # flagged pings only), so its excess is left NA.
  if (evidence) {
    if (by == "both") {
      ea <- .os_leg_excess(j, !is.na(fa), gw, lim, speed_kn = eva$ping_flag_speed_kn)
      eb <- .os_leg_excess(j, !is.na(fb), gt, lim, speed_kn = evb$ping_flag_speed_kn)
      ex <- ea; ex[use_b, ] <- eb[use_b, ]
    } else {
      ex <- .os_leg_excess(j, !is.na(fl), if (by == "trip") gt else gw, lim, speed_kn = eva$ping_flag_speed_kn)
    }
    if (grepl("speed", method)) ex[is.na(fl), ] <- NA
  }

  # 6. the set-aside pings back onto the cleaned track
  aside <- out %in% c("no fix", "synthetic", "copy", "duplicate")
  fits <- rep(NA, n)
  if (any(aside)) {
    allj <- d |> mutate(time_eff = time)
    allj$time_eff[j$.i] <- j$time_eff
    sub <- allj[out == "kept" | aside, ]
    kflag <- out[sub$.i] == "kept"
    fits[sub$.i[!kflag]] <- .os_fit_back(sub, kflag, lim)
  }

  res <- x
  res$ping_outcome <- out
  res$ping_fits <- fits
  res$ping_step <- step
  res$ping_flag_basis <- basis
  if (evidence) {
    res$ping_flag_view <- NA_character_
    res$ping_flag_view[j$.i] <- ifelse(is.na(fl), NA_character_, ifelse(use_b, "by trip", if (by == "trip") "by trip" else "whole track"))
    res$ping_flag_side <- NA_character_; res$ping_flag_side[j$.i] <- ev$ping_flag_side
    res$ping_flag_speed_kn <- NA_real_; res$ping_flag_speed_kn[j$.i] <- ev$ping_flag_speed_kn
    for (v in names(ex)) { res[[v]] <- NA_real_; res[[v]][j$.i] <- ex[[v]] }
    # where the kept pings put every ping, set aside or not, on the whole track (written time where none was judged)
    allj <- d |> mutate(time_eff = time)
    allj$time_eff[j$.i] <- j$time_eff
    nd <- .os_neighbour_distance(allj, out != "kept", allj$vid)
    res$ping_resid_m <- nd$ping_resid_m
    res$ping_resid_rel <- nd$ping_resid_rel
    attr(res, "ramb_evidence") <- data.frame(.i = j$.i, ev)
  }
  res$ping_speed_stage <- NA_character_
  res$ping_speed_stage[j$.i] <- fl
  res$ping_time_off <- time_off
  res$ping_time_jump_s <- jump_s
  res$ping_trip <- NA_integer_
  res$ping_trip[j$.i] <- j$ping_trip
  res$time_eff <- res$time
  res$time_eff[j$.i] <- j$time_eff
  attr(res, "limits") <- lim
  attr(res, "speed_check") <- speed_check
  res
}

# the columns the code above names as variables
utils::globalVariables(c(".i", ".r", "after", "b_in", "b_out", "c", "cand", "cbind", "ceiling_kn", "crs", "crs_ok", "D", "dh", "dt", "dt_median_s", "ev", "g", "grain_s", "is.null", "J", "junction", "k", "kind", "kn", "kn_step", "last_in_ev", "lat", "lon", "moving", "n", "next_t", "nextD", "noise_m", "nz", "off_i", "ok_in", "ok_out", "on_line", "os_create_ping_limits", "paste", "provider", "r0", "r1", "ratio", "runs", "seg", "shift", "spd", "speed_rec", "steady_ok", "step", "still", "t", "te", "terr", "time", "time_eff", "time_error_s", "time_fault", "too_long", "v", "v_a", "v_b", "vid", "vid0", "X", "Y"))
