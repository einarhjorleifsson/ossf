# ossf

*Drafted with AI assistance (Claude), 2026-11-25, at the maintainer's request;
not yet checked by a person.*

The steps from a vessel's pings to its fishing, a lighter alternative to ramb. The name: "o" is not, "ssf" is small-scale
fisheries (the maintainer, 2026-11-25).

## Three kinds of function

- **ossf's own**, written for it and kept simple, in base R (sf for the
  polygons): `os_add_fishing()`, `os_add_hauls()`, `os_flag_ping_polygon()`,
  `os_add_trip_value()`, `os_add_time_utc()`, `os_score()`, and the map,
  `os_map()`.
- **Copies from ramb**, taken as they are with everything they call, the
  compiled code included (`data-raw/copy_from_ramb.R`; the maintainer: "Copy
  as they are"). Only the names differ: `rb_` is `os_`. They run in DuckDB as
  ramb's do and give ramb's results. `data-raw/copy_from_ramb.csv` lists every
  object, with its ramb file and lines.
- **A copy from fishycode's study of plan 086**: `os_flag_ping()`, the flags in one call, bound for ramb and not in it
  yet. Copied by the same script.

A copy is never edited here: a change is made at its source and
`Rscript data-raw/copy_from_ramb.R` run again.

## By step of the flow

- **Prepare:** `os_add_time_utc()` (a local clock reading on UTC, the reading
  kept, the clock's changes labelled); `os_flag_ping_repeat()` (a record
  written again at another time).
- **Flag pings:** `os_flag_ping()`, the flags in one call, every ping in one
  outcome; its parts `os_flag_ping_invalid()`, `os_flag_ping_duplicate()`,
  `os_flag_ping_time()`, `os_flag_ping_impossible()`,
  `os_create_ping_limits()`. `os_flag_ping_polygon()`: in which polygon a
  ping lies (land, a port, an area).
- **Ports:** `os_create_port_calls()`.
- **Trips:** `os_create_voyages()`, `os_add_voyages()`;
  `os_add_trip_value()`, a trip's time, gear or port from ranked witnesses.
- **Fishing:** `os_add_fishing()`: a ping is fishing when the vessel's
  smoothed speed is within its working speed range, the range given, measured
  from a witness, or estimated from the vessel's own speeds.
  `os_add_hauls()`: the fishing pings grouped into hauls afterwards, the call
  never changed; two runs of fishing pings are one haul where the stretch
  between them covers no more ground than the shorter run (a pause is an
  interruption of the work, so the vessel stays at its gear). The speeds on
  their own: `os_add_ping_speed()`, `os_calc_speed_smooth()`,
  `os_calc_distance()`; `os_number_runs()`.
- **Setting (passive gears):** `os_add_setting()`: where and when the gear was
  set, the later haul as the witness. A position in no haul that lies on the
  line of a later haul is marked; the vessel's setting speed range is measured
  from the marked positions; the positions in it are grouped into setting
  events; and each haul is asked what was last laid under it, a setting event
  or an earlier haul (gear put back as it is hauled), with the soak. The reach
  is the gear's scale, picked by vessel where its sets and hauls are most
  often of one length. `run_in = TRUE` takes the run in to a haul out, for
  gear hauled on a later visit.
- **Against a witness:** `os_score()`, every row in one outcome, with recall,
  precision and F1.
- **On a map, after any step:** `os_map()` draws the pings with what the flow
  has added so far (outcome, port, trip, fishing) and a time panel under the
  map, with ogis, served on this computer only.

## One question, one function

Where two ramb functions ask one question, ossf has one function of its own
in their place, checked to give both results:

- `os_flag_ping_polygon()` for ramb's land and port flags.
- `os_add_trip_value()` for ramb's trip time and trip gear (the maintainer:
  "yes, merge them"): which witness's value a trip takes, the witness's value
  its earliest (a time) or its most usual (a gear, a port). One value a call:
  ramb's port beside the departure time is a second call. On real tracks it gives ramb's results on every call made.

ramb's `rb_flag_ping_land()` and `rb_flag_ping_port()` ask one question, which
polygon holds a ping (the maintainer: "a classical ping-in-polygon question.
they should actually really be just one function"). ossf has
`os_flag_ping_polygon()` in their place; on real tracks it gives both ramb flags' results on every record. An edge is straight in longitude and latitude, as in
ramb's port flag; ramb's land flag draws it straight on a projection, which
moves a long edge by tens of metres.

## Why the fishing rule is so little

The rule was grown on a small-scale static-gear fishery's tracks with more in it
(straightness, a weave, end trimming, hauls joined across gaps, pauses), and
each piece was then scored against witnesses that never read the track: a
hauler sensor there, and logbook haul and tow times on the Icelandic fleet.
The speed alone scored as well as the whole rule on both; what moved the score
was the speed range. The record is kept outside this package (fishycode's studies). No case-study data is in this
package; its tests use simulated tracks.

## What is given, not taken from the data

- R's bandwidth rule for the speed distributions, on a grid of 1,024 points.
- A speed range is read from 100 speeds or more.
- Which low point of the speeds is taken when they show more than two kinds
  (the one that best separates the two sides). It failed on some Icelandic
  trawlers, whose speeds have a long tail near zero; small-scale static gear
  is what it was tested on.
- The copies carry ramb's own given values, listed on their help pages.

## Not in it

Setting and soak; gear from the track; effort and catch.
