# ossf — agent notes

*What is true now about ossf, for people and AI assistants alike. Keep it short
and current, and change it with the fact it records. Written 2026-11-25 by
Claude (AI) at the maintainer's word ("setup AGENTS.md for the repo - include
the essentials"); not yet read by the maintainer.*

## What ossf is

An R package with the steps from a vessel's pings to its fishing, a lighter alternative to ramb. The name: "o" is not, "ssf"
is small-scale fisheries. The functions by step are listed in `README.md`.

Created 2026-11-25; a git repository since the same day, its first commit at the maintainer's word. Not installed:
it is loaded from source (`pkgload::load_all("~/R/Pakkar/ossf")`). Commit, push or install only at the
maintainer's word. The compiled files (`src/*.o`, `src/*.so`) are not tracked.

## Three kinds of function

- **ossf's own** (`R/fishing.R`, `haul.R`, `setting.R`, `polygon.R`,
  `trip_value.R`, `time.R`, `score.R`, `map.R`, `effort.R`): written for ossf, base R where it can (sf for
  polygons, ogis for the map, which is only suggested). Edited here.
- **Copies of ramb** (`R/ramb_*.R`, `src/`, `inst/COPYRIGHTS`): every ramb function the flow's steps call, with everything it calls, the C++ included,
  text unchanged but for the names (`rb_` to `os_`). Made by
  `data-raw/copy_from_ramb.R` from ramb's source (`RAMB_SRC`, default
  `~/R/Pakkar/ramb`); `data-raw/copy_from_ramb.csv` lists each object with its
  ramb file and lines. Each file's head names the ramb version and commit.
- **A copy of fishycode's study of plan 086** (`R/study_flag_ping.R`):
  `proto_flag_ping()` as `os_flag_ping()`, made by the same script from
  `$FISHYDATA/archive/2026-11-22-robust-ping-flags/R/proto.R` (or
  `OSSF_STUDY`). Its help page is written in the script.

**A copy is never edited here.** A change is made at its source and the
script run again, then `Rcpp::compileAttributes()` and
`roxygen2::roxygenise()`. The script writes a compiled source file only when
it changed, so the compiled code is not rebuilt for nothing.

Where two ramb functions ask one question, ossf has one of its own instead of
copies, and the script leaves those out (its `replaced`):
`os_flag_ping_polygon()` for ramb's land and port flags,
`os_add_trip_value()` for its trip time and trip gear (the maintainer: "they
should actually really be just one function"; "yes, merge them").

## Rules

- **No case-study data in this package**, ever: the data are their owners'.
  Tests use simulated tracks (`tests/testthat/helper-sim.R`).
- **Few principles, simple code.** Two cases, reworded 2026-11-25 at the
  maintainer's word ("Yes, reword it", after asking whether the rule kept a
  setting function out and constrained the thinking):
  - *A clause added to a function that exists* stays only if it earns its
    place in an ablation (the function scored with and without it); agreement
    with a reference's labels alone is not a reason (the reference is not the
    truth). This is the brake on a rule growing clause by clause.
  - *A new function* (a new thing the package can do) is judged on all the
    evidence there is: outside witnesses, agreement with other methods, and
    consistency inside the data (a line is as long hauled as set; every haul
    has a set). An outside witness is not required, and one that exists is
    itself checked (Iceland's logged set times were in part computed by the
    app). Its given values are named on the help page.
- **Label, never drop.** Every function returns every row it was given, in
  the order given; a value that cannot be judged is `NA` with why.
- **Every value says where it came from** (a `_basis` or `_from` column).
- **No hard values**: from the data where possible; a given value is named on
  the help page.
- Maps are served on this computer only (`127.0.0.1`), never published.
- Every file says it was drafted with AI assistance (Claude) and when.

- The setting function, at the user's word (2026-11-25: "Think we should put
  the Haul-witnessed approach as a function in ossf now"): `os_add_setting()`,
  from fishycode's study
  `$FISHYDATA/archive/2026-11-25-setting-iceland-longline/` (its README holds
  what was tried and what was not taken). Held against it again by the study's `scripts/12_ossf_function.R`. The step `effort` calls it since 2026-11-26, for each haul's soak.

## Checking

- `testthat::test_dir("tests/testthat")` after `load_all()`: the own
  functions, and each merged one against ramb's pair where ramb is installed.
- `R CMD check` (build in a temporary folder): the only warning left is R's own
  `Boolean.h` with this compiler, which ramb has too.
- Against ramb on real data: checks kept outside this package ran every ramb call the steps make, ramb's and ossf's side by side. On 2026-11-25 every call was identical.

## Where things are

- ramb, the source of the copies: `~/R/Pakkar/ramb`.
- fishycode, the Icelandic flow, whose sessions do this work:
  `~/stasi/fishycode`.

## Open

- Nothing is added to ossf on the way (the maintainer, 2026-11-25: "there should be no
  missing functions in ossf - if there are flag that rather than proceeding
  directly through").
- The fishing rule ossf offers in place of the study's, at the user's word
  (2026-11-25: "lets implement your recommendation as a function in ossf"):
  `os_add_fishing()` (the call) then `os_add_hauls()` (hauls from the call,
  joined by ground).
- ossf's `os_add_fishing()` is ossf's own, not ramb's `rb_add_fishing()` under
  the new prefix: the one name in ossf that does not map to its ramb namesake.
- `os_add_ping_speed()` and `os_calc_speed_smooth()` repeat what
  `os_add_fishing()` already returns. The fishing step no longer calls them
  (2026-11-25), so they may go; not done, the maintainer's word first.
- The prefix `os_` and the title are provisional.
