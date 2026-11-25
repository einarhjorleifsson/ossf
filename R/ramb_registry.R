# Copied from ramb 2026.11.24 (b777ac4), R/registry.R, by data-raw/copy_from_ramb.R: do not edit here.
# The text is ramb's, with the names changed (rb_ to os_); a change is made in ramb and the copy made again.

.os_step <- function(
  fn,
  stage,
  kind,
  input,
  needs,
  adds,
  after = NA_character_,
  paths = "all"
) {
  list(
    fn = fn,
    stage = stage,
    kind = kind,
    input = input,
    needs = needs,
    adds = adds,
    after = after,
    paths = paths
  )
}

.os_steps <- list(
  # vessel: which ids of two position feeds are one vessel
  .os_step(
    "ramb::rb_link_vessel_tracks",
    "vessel",
    "builder",
    "x",
    c("time", "lon", "lat"),
    c(
      "n_x",
      "hits",
      "opportunity",
      "score",
      "median_m",
      "rank",
      "score_other",
      "strong"
    )
  ),
  # step 0: the streams' clocks (clock.R), before any speed or turn is derived across streams
  .os_step(
    "ramb::rb_add_ping_stream",
    "ping",
    "step",
    "x",
    c("vid", "time"),
    c(
      "stream",
      "stream_dt_s",
      "stream_cadence_s",
      "stream_phase_s",
      "stream_off_grid",
      "stream_crossed"
    )
  ),
  .os_step(
    "ramb::rb_create_ping_lags",
    "ping",
    "builder",
    "x",
    c("vid", "time", "lon", "lat", "stream"),
    c(
      "reference",
      "period",
      "pings",
      "pairs",
      "lag_s",
      "lag_q10_s",
      "lag_q90_s",
      "along_m",
      "across_m",
      "dist_zero_m",
      "dist_lag_m",
      "outcome"
    ),
    after = "ramb::rb_add_ping_stream"
  ),
  .os_step(
    "ramb::rb_add_ping_own_lag",
    "ping",
    "step",
    "x",
    c("vid", "time", "lon", "lat", "stream"),
    c("lag_own_s", "lag_own_basis"),
    after = "ramb::rb_add_ping_stream"
  ),
  .os_step(
    "ramb::rb_add_ping_time",
    "ping",
    "step",
    "x",
    c("vid", "time", "stream"),
    c("time_delivered", "lag_s", "lag_basis"),
    after = "ramb::rb_create_ping_lags"
  ),
  # step 0: each vessel's own limits (fishycode plan 065), measured on its pings before they are flagged
  .os_step(
    "os_create_ping_limits",
    "ping",
    "builder",
    "x",
    c("vid", "time", "lon", "lat"),
    c(
      "pings",
      "ceiling_kn",
      "ceiling_basis",
      "recorded_kn",
      "recorded_n",
      "recorded_moving_n",
      "track_kn",
      "track_n",
      "ceilings_agree",
      "grain_s",
      "dt_median_s",
      "time_error_s",
      "time_error_basis",
      "time_error_n",
      "time_error_lag_n",
      "noise_m",
      "noise_basis"
    ),
    after = "ramb::rb_add_ping_time"
  ),
  # step 0: ping flags
  .os_step(
    "os_flag_ping_invalid",
    "ping",
    "step",
    "x",
    c("lon", "lat"),
    c("ping_flag", "ping_flag_basis")
  ),
  .os_step(
    "os_flag_ping_duplicate",
    "ping",
    "step",
    "x",
    c("vid", "time", "lon", "lat"),
    c("ping_flag", "ping_flag_basis"),
    after = "os_flag_ping_invalid"
  ),
  # step 0: a stretch written twice, read from the pings as the device wrote them (2026-11-24)
  .os_step(
    "os_flag_ping_repeat",
    "ping",
    "step",
    "x",
    c("vid", "time"),
    "ping_repeat_shift_s"
  ),
  # step 0: the pings a land layer puts on land, a label and a test, never a filter (2026-11-24)
  .os_step(
    "ramb::rb_flag_ping_land",
    "ping",
    "step",
    "x",
    c("lon", "lat"),
    c("ping_land", "ping_land_port", "ping_land_m"),
    after = "os_flag_ping_invalid"
  ),
  # scoring a flagger on a planted set (fishycode plan 065): each ping's outcome under a rule
  .os_step(
    "ramb::rb_add_ping_outcome",
    "ping",
    "tool",
    "x",
    character(0),
    character(0)
  ),
  # step 0: a time written off, its position kept (fishycode plan 065), before the impossible positions
  .os_step(
    "os_flag_ping_time",
    "ping",
    "step",
    "x",
    c("vid", "time", "lon", "lat", "speed"),
    c("ping_flag", "ping_flag_basis", "ping_time_error_s"),
    after = "os_flag_ping_duplicate"
  ),
  .os_step(
    "os_flag_ping_impossible",
    "ping",
    "step",
    "x",
    c("vid", "time", "lon", "lat"),
    c("ping_flag", "ping_flag_basis"),
    after = "os_flag_ping_duplicate"
  ),
  # a stream's clock that jumps ahead by a zone's step (ogis's wish list, item 3, 2026-11-18)
  .os_step(
    "ramb::rb_flag_ping_clock_jump",
    "ping",
    "step",
    "x",
    c("vid", "time", "lon", "lat", "speed"),
    c("ping_clock_jump_s", "ping_clock_jump_basis", "ping_clock_shift_s"),
    after = "os_flag_ping_duplicate"
  ),
  # step 0: the speed of the pings left, within each stream (fishycode plan 062 step 4b)
  .os_step(
    "os_add_ping_speed",
    "ping",
    "step",
    "x",
    c("vid", "time", "lon", "lat"),
    c("speed_derived", "speed_derived_span_s", "speed_derived_basis"),
    after = "os_flag_ping_impossible"
  ),
  .os_step(
    "ramb::rb_flag_ping_port",
    "ping",
    "step",
    "pings",
    c("lon", "lat"),
    "port_id",
    after = "os_flag_ping_impossible"
  ),
  # step 1: trips
  .os_step(
    "os_create_port_calls",
    "trip",
    "builder",
    "pings",
    c("vid", "time"),
    c(
      "year",
      "port_call_id",
      "port_id",
      "T_in",
      "T_out",
      "dur_h",
      "rule",
      "basis"
    ),
    after = "ramb::rb_flag_ping_port"
  ),
  # recorded port events judged as events (fishycode plan 048), before os_create_port_calls(events = ) takes `good`
  .os_step(
    "ramb::rb_flag_port_event",
    "trip",
    "step",
    "events",
    c("vid", "time", "io"),
    c("event_flag", "event_track_kn", "good"),
    after = "ramb::rb_flag_ping_port"
  ),
  # the port calls of chunks (os_create_port_calls(window = )) merged into those of the whole series
  .os_step(
    "ramb::rb_merge_port_calls",
    "trip",
    "builder",
    "calls",
    c("vid", "port_id", "T_in", "T_out"),
    character(0),
    after = "os_create_port_calls"
  ),
  .os_step(
    "os_create_voyages",
    "trip",
    "builder",
    "pings",
    c("vid", "time"),
    c(
      "voyage_id",
      "voyage_key",
      "T1",
      "T2",
      "port_from",
      "port_to",
      "n_pings"
    ),
    after = "os_create_port_calls"
  ),
  .os_step(
    "ramb::rb_link_trip_voyages",
    "trip",
    "builder",
    "trips",
    c("vid", "T1", "T2"),
    c(
      "voyage_id",
      "voyage_key",
      "T1_voyage",
      "T2_voyage",
      "overlap_s",
      "overlap_frac",
      "n_candidates",
      "basis"
    ),
    after = "os_create_voyages"
  ),
  # the departure (or the return) from ranked witnesses: the track, the logbook, notifications; T1 and pid1 by default
  .os_step(
    "ramb::rb_add_trip_time",
    "trip",
    "builder",
    "trips",
    character(0),
    c(
      "T1_from",
      "T1_basis",
      "T1_day",
      "T1_n",
      "T1_outcome",
      "T1_spread_s",
      "pid1_from",
      "pid1_basis",
      "pid1_n",
      "pid1_outcome"
    ),
    after = "ramb::rb_link_trip_voyages"
  ),
  # the gear from ranked witnesses: the logbook, landings, the pings; `gear` by default. The gear itself is not
  # listed: it is an input column of the records, and a missing one would name this step.
  .os_step(
    "ramb::rb_add_trip_gear",
    "trip",
    "builder",
    "trips",
    character(0),
    c("gear_from", "gear_basis", "gear_share", "gear_n", "gear_outcome"),
    after = "os_create_voyages"
  ),
  .os_step(
    "ramb::rb_link_trip_landings",
    "trip",
    "builder",
    "trips",
    c("vid", "T2"),
    c("match_type", "dt_days", "n_trips", "n_landings"),
    after = "os_create_voyages"
  ),
  .os_step(
    "ramb::rb_add_logbook_trip",
    "trip",
    "step",
    "ve",
    c("vessel_id", "time"),
    "trip_id"
  ),
  .os_step(
    "os_add_voyages",
    "trip",
    "step",
    "pings",
    c("vid", "time"),
    c(
      "port_call_port_id",
      "port_call_id",
      "voyage_id",
      "voyage_key",
      "trip_id",
      "trip_basis"
    ),
    after = "os_create_voyages"
  ),
  .os_step(
    "ramb::rb_check_trip_crosses_year",
    "trip",
    "tool",
    "eflalo",
    character(0),
    character(0)
  ),
  .os_step(
    "ramb::rb_plot_trip",
    "trip",
    "tool",
    "data",
    c("lon", "lat"),
    character(0)
  ),
  # step 2: logbook records
  .os_step(
    "ramb::rb_limit_record_duration",
    "record",
    "builder",
    "records",
    character(0),
    "duration_source"
  ),
  .os_step(
    "ramb::rb_create_record_windows",
    "record",
    "builder",
    "records",
    c("vid", "gear", "gear_class", "t1", "t2", "t3", "t4", "date"),
    c(
      "phase",
      "w_start",
      "w_end",
      "win_basis",
      "win_closed_by",
      "win_opened_by",
      "n_events",
      "events"
    ),
    after = "ramb::rb_limit_record_duration"
  ),
  .os_step(
    "ramb::rb_link_record_pings",
    "record",
    "builder",
    "pings",
    c("vid", "time"),
    c("phase", "phase_frac", "w_start", "w_end", "fishing"),
    after = "ramb::rb_create_record_windows"
  ),
  .os_step(
    "ramb::rb_link_record_events",
    "record",
    "builder",
    "pings",
    c("vid", "time"),
    c("event", "event_time", "dt_s", "gap_s", "ping_lost"),
    after = "ramb::rb_create_record_windows"
  ),
  .os_step(
    "ramb::rb_create_record_stretches",
    "record",
    "builder",
    "pings",
    c("vid", "time", "speed"),
    c("stretch_id", "s_start", "s_end", "n_pings", "opened_by", "closed_by"),
    after = "ramb::rb_link_record_pings"
  ),
  .os_step(
    "ramb::rb_link_record_stretches",
    "record",
    "builder",
    "records",
    c("vid"),
    c(
      "stretch_id",
      "overlap_s",
      "dt_start_s",
      "dt_end_s",
      "n_candidates",
      "stretch_lost"
    ),
    after = "ramb::rb_create_record_stretches"
  ),
  .os_step(
    "ramb::rb_create_record_pings",
    "record",
    "builder",
    "records",
    c("vid", "start", "end", "lon1", "lat1"),
    c("time", "lon", "lat", "speed", "pos_basis"),
    after = "ramb::rb_create_record_windows"
  ),
  .os_step(
    "ramb::rb_link_record_sets",
    "record",
    "builder",
    "hauls",
    c("vid", "start", "end"),
    c(
      "set_start",
      "set_end",
      "soak_h",
      "set_speed",
      "n_set_pings",
      "buffer_m",
      "set_basis",
      "set_method",
      "set_consolidated"
    ),
    after = "ramb::rb_create_record_windows"
  ),
  .os_step(
    "ramb::rb_add_record",
    "record",
    "step",
    "pings",
    c("vid", "time"),
    c("n_records", "phase", "phase_frac"),
    after = "ramb::rb_link_record_pings"
  ),
  # step 3: gear
  .os_step(
    "ramb::rb_predict_gear_width",
    "gear",
    "tool",
    "metier",
    character(0),
    character(0)
  ),
  .os_step("ramb::rb_fill_gear_width", "gear", "tool", "lb", "gid", "gear.width"),
  .os_step(
    "ramb::rb_get_gear_vocabulary",
    "gear",
    "tool",
    "code_list",
    character(0),
    character(0)
  ),
  .os_step(
    "ramb::rb_standardise_gear_meshsize",
    "gear",
    "tool",
    "lb",
    c("gid", "mesh"),
    "mesh.std"
  ),
  # step 4: fishing
  .os_step(
    "ramb::rb_add_fishing",
    "fishing",
    "step",
    "pings",
    c("vid", "time", "speed"),
    "fishing",
    after = "ramb::rb_link_record_pings"
  ),
  # step 5: effort
  .os_step(
    "ramb::rb_cap_effort",
    "effort",
    "tool",
    "lb",
    c("vid", "gid", "effort", "t1", "t2"),
    character(0)
  ),
  .os_step(
    "ramb::rb_add_effort",
    "effort",
    "step",
    "pings",
    c("vid", "time"),
    c(
      "hours",
      "kw_hours",
      "pressure",
      "pressure_unit",
      "interval_flag",
      "effort_basis"
    ),
    after = "ramb::rb_add_fishing"
  ),
  # step 6: catch. ramb::rb_share_catch() returns a table of its own (one row per fishing ping, unit and species),
  # so it is a builder; its method = "eflalo" keeps its own checks.
  .os_step(
    "ramb::rb_add_catch_units",
    "catch",
    "builder",
    "events",
    c("vid", "start", "end", "level", "resolution"),
    c("start_filled", "unit_id", "n_events"),
    after = "ramb::rb_create_record_windows"
  ),
  .os_step(
    "ramb::rb_share_catch",
    "catch",
    "builder",
    "pings",
    c("vid", "time"),
    c("shared_unit_id", "shared_resolution", "catch_basis", "approach"),
    after = "ramb::rb_add_effort"
  )
)

names(.os_steps) <- vapply(.os_steps, `[[`, "", "fn")

# The registry as a table, one row per function; needs, adds and paths are list columns.
.os_registry <- function() {
  tibble::tibble(
    fn = names(.os_steps),
    stage = unname(vapply(.os_steps, `[[`, "", "stage")),
    kind = unname(vapply(.os_steps, `[[`, "", "kind")),
    input = unname(vapply(.os_steps, `[[`, "", "input")),
    needs = unname(lapply(.os_steps, `[[`, "needs")),
    adds = unname(lapply(.os_steps, `[[`, "adds")),
    after = unname(vapply(.os_steps, `[[`, "", "after")),
    paths = unname(lapply(.os_steps, `[[`, "paths"))
  )
}

# The steps that add a column, for the message that names the step to run first. A builder's columns are on a
# table of its own, so builders are named only when no step adds the column.
.os_added_by <- function(col) {
  adds <- function(kind) {
    hit <- vapply(
      .os_steps,
      function(s) s$kind == kind && col %in% s$adds,
      logical(1)
    )
    names(.os_steps)[hit]
  }
  by <- adds("step")
  if (length(by) == 0) adds("builder") else by
}

# Check a step function's main input against its registry row, before any work is done.
#   x       the input (a data frame or a lazy table; a lazy table is checked by its schema only)
#   fn      the calling function's name, a key of .os_steps
#   cols    the user's names for the grammar's columns, e.g. c(vid = "vessel_id"); unnamed needs are as listed
#   also    columns needed only in this call (an optional input the user switched on), named as `cols`
#   types   expected classes for some grammar columns: "time" (POSIXct) or "numeric"
#   fresh   TRUE: the columns this step adds must not be there yet (it has already run on this table)
#   call    the frame the error is reported from (the step function's)
.os_check_input <- function(
  x,
  fn,
  cols = character(0),
  also = character(0),
  types = character(0),
  fresh = FALSE,
  call = rlang::caller_env()
) {
  row <- .os_steps[[fn]]
  if (is.null(row)) {
    stop(
      sprintf("internal: %s() has no row in the step registry", fn),
      call. = FALSE
    )
  }
  arg <- row$input
  if (!(is.data.frame(x) || inherits(x, "tbl_lazy"))) {
    rlang::abort(
      c(
        sprintf("`%s` must be a data frame or a lazy DuckDB table.", arg),
        x = sprintf("It is a %s.", class(x)[1])
      ),
      call = call
    )
  }
  # a 0-row frame holds the column types of a lazy table without scanning it
  proto <- if (inherits(x, "tbl_lazy")) {
    dplyr::collect(utils::head(x, 0))
  } else {
    x[0, , drop = FALSE]
  }
  have <- names(proto)
  cols <- c(cols, also)
  user <- function(g) if (g %in% names(cols)) cols[[g]] else g
  needs <- unique(c(row$needs, names(also)))
  miss <- needs[!vapply(needs, function(g) user(g) %in% have, logical(1))]
  if (length(miss) > 0) {
    g <- miss[1]
    # the grammar's own input columns are never "added by" a step, though a builder may make them anew; a column the
    # step before makes is named by it (ramb::rb_merge_port_calls() takes port calls, whose `port_id` is not the pings')
    prev <- if (is.na(row$after)) NULL else .os_steps[[row$after]]
    by <- if (g %in% c("vid", "time", "lon", "lat", "speed", "start", "end")) {
      character(0)
    } else if (!is.null(prev) && g %in% prev$adds) {
      row$after
    } else {
      .os_added_by(g)
    }
    hint <- if (length(by) > 0) {
      c(
        i = sprintf(
          "It is added by %s.",
          paste0("`", by, "()`", collapse = " or ")
        )
      )
    } else if (g %in% names(cols)) {
      c(i = sprintf("Name the column that holds it with the `%s` argument.", g))
    } else {
      c(i = "It is an input column: supply it, or rename yours to it.")
    }
    rlang::abort(
      c(
        sprintf("`%s` must have a column `%s`.", arg, user(g)),
        x = sprintf(
          "%s %s missing.",
          paste0("`", vapply(miss, user, ""), "`", collapse = ", "),
          if (length(miss) == 1) "is" else "are"
        ),
        hint
      ),
      call = call
    )
  }
  for (g in names(types)) {
    u <- user(g)
    if (!u %in% have) {
      next
    }
    ok <- switch(
      types[[g]],
      time = inherits(proto[[u]], "POSIXct"),
      numeric = is.numeric(proto[[u]]),
      TRUE
    )
    if (!ok) {
      rlang::abort(
        c(
          sprintf(
            "`%s` must be %s.",
            u,
            if (types[[g]] == "time") "a date-time (POSIXct)" else "numeric"
          ),
          x = sprintf("It is %s.", class(proto[[u]])[1])
        ),
        call = call
      )
    }
  }
  if (fresh) {
    again <- intersect(row$adds, have)
    if (length(again) > 0) {
      rlang::abort(
        c(
          sprintf(
            "`%s` already has %s.",
            arg,
            paste0("`", again, "`", collapse = ", ")
          ),
          i = sprintf(
            "%s() has probably been run on it already; drop those columns to run it again.",
            fn
          )
        ),
        call = call
      )
    }
  }
  invisible(proto)
}

