# The functions ossf takes from ramb, copied as they are ---------------------------------------------------------------
#
# Input:  ramb's source tree (RAMB_SRC, by default ~/R/Pakkar/ramb): R/*.R, src/, inst/COPYRIGHTS.
# Output: R/ramb_<file>.R (each function below with everything it calls, text unchanged but for the names), src/ (the
#         compiled code they call, with RcppExports made anew), inst/COPYRIGHTS, and data-raw/copy_from_ramb.csv (what
#         was copied, from which file and lines).
#
# The maintainer (2026-11-25): "Check what ramb functions we have been using and copy them to ossf", and of how:
# "Copy as they are" (every function with everything it calls, the compiled code included). So nothing here is
# rewritten: a top-level object of ramb's R files is taken with the comment block above it, when it is one of the seeds
# or is named by one already taken. The names change, and nothing else: rb_ to os_ and .rb_ to .os_ for what is copied;
# a ramb function that is not copied is written ramb::rb_...() so that a help page does not point at nothing; the
# package's own name in code (options, the namespace) becomes ossf. The examples are left out: they run on ramb's
# example data, which ossf does not carry.
#
# The copies are never edited in ossf. A change is made in ramb, and this script run again.
# Run from ossf's root: Rscript data-raw/copy_from_ramb.R, then Rcpp::compileAttributes() and roxygen2::roxygenise()
# (the script does both). Drafted with AI assistance (Claude), 2026-11-25.

ramb <- path.expand(Sys.getenv("RAMB_SRC", "~/R/Pakkar/ramb"))
stopifnot("run from ossf's root" = file.exists("DESCRIPTION") && grepl("^Package: ossf", readLines("DESCRIPTION", 1)))

# The seeds: what the flow calls ------------------------------------------------------------------------------------------
seeds <- c(
  # the steps' own calls: prepare, flag_pings, port, trip, fishing
  "rb_flag_ping_repeat", "rb_create_port_calls", "rb_create_voyages",
  "rb_add_voyages", "rb_add_ping_speed", "rb_number_runs",
  # the calls of the working copies the steps flag_pings and fishing source (fishycode's study of plan 086)
  "rb_calc_distance", "rb_create_ping_limits", "rb_flag_ping_duplicate", "rb_flag_ping_impossible", "rb_flag_ping_invalid",
  "rb_flag_ping_time", ".rb_flag_evidence", "rb_calc_speed_smooth",
  # the hook that closes the package's own DuckDB connection when the package is unloaded
  ".onUnload"
)
# not copied: rb_add_fishing(), whose place ossf's own os_add_fishing() takes (the same name after the change); and
# rb_flag_ping_land() and rb_flag_ping_port(), which the steps call but which ask one question (the maintainer,
# 2026-11-25: "a classical ping-in-polygon question. they should actually really be just one function"): ossf's own
# os_flag_ping_polygon() answers it, and a help page that names either is pointed there
own <- c("os_add_fishing", "os_flag_ping_polygon", "os_add_trip_value")
replaced <- c(rb_flag_ping_land = "os_flag_ping_polygon", rb_flag_ping_port = "os_flag_ping_polygon")
# and rb_add_trip_time() and rb_add_trip_gear(), which the steps call but which ask one question too, which witness's
# value a trip takes (the maintainer, 2026-11-25: "yes, merge them"): ossf's own os_add_trip_value()
replaced <- c(replaced, rb_add_trip_time = "os_add_trip_value", rb_add_trip_gear = "os_add_trip_value")

# ramb's top-level objects, each with the comment block above it ----------------------------------------------------------
files <- list.files(file.path(ramb, "R"), "\\.R$", full.names = TRUE)
files <- files[!basename(files) %in% c("RcppExports.R", "ramb-package.R")]
items <- list()
for (f in files) {
  tx <- readLines(f, warn = FALSE)
  p <- parse(f, keep.source = TRUE)
  sr <- attr(p, "srcref")
  for (i in seq_along(p)) {
    e <- p[[i]]
    a <- sr[[i]][1]
    b <- sr[[i]][3]
    named <- is.call(e) && (identical(e[[1]], as.name("<-")) || identical(e[[1]], as.name("="))) && is.name(e[[2]])
    k <- a - 1
    while (k >= 1 && grepl("^\\s*#", tx[k])) k <- k - 1
    # a replacement at the top level (`names(.rb_steps) <- ...`) belongs to the object it changes: copied with it
    target <- NA_character_
    if (!named && is.call(e) && identical(e[[1]], as.name("<-")) && is.call(e[[2]])) {
      lhs <- e[[2]]
      while (is.call(lhs)) lhs <- lhs[[2]]
      if (is.name(lhs)) target <- as.character(lhs)
    }
    items[[length(items) + 1]] <- list(file = basename(f), name = if (named) as.character(e[[2]]) else NA_character_,
                                       target = target, from = k + 1, to = b, text = tx[(k + 1):b], uses = unique(all.names(e)))
  }
}
nm <- vapply(items, `[[`, "", "name")
cpp <- parse(file.path(ramb, "R", "RcppExports.R"))
cpp <- vapply(cpp, function(e) as.character(e[[2]]), "")
known <- c(stats::na.omit(nm), cpp)
if (anyDuplicated(stats::na.omit(nm))) stop("ramb defines twice: ", paste(unique(nm[duplicated(nm) & !is.na(nm)]), collapse = ", "))

# The closure: every object a taken one names ------------------------------------------------------------------------------
miss <- setdiff(seeds, known)
if (length(miss)) stop("not in ramb: ", paste(miss, collapse = ", "))
taken <- character()
todo <- seeds
while (length(todo)) {
  g <- todo[1]
  todo <- todo[-1]
  if (g %in% taken) next
  taken <- c(taken, g)
  if (g %in% nm) todo <- c(todo, setdiff(intersect(items[[match(g, nm)]]$uses, known), taken))
}
skipped <- intersect(taken, c("rb_add_fishing", names(replaced)))
if (length(skipped)) stop("a copied function calls one that is not copied: ", paste(skipped, collapse = ", "))
taken_cpp <- intersect(taken, cpp)
taken_r <- setdiff(taken, cpp)

# The names' change ---------------------------------------------------------------------------------------------------------
exported <- sub("^export\\((.*)\\)$", "\\1", grep("^export\\(", readLines(file.path(ramb, "NAMESPACE")), value = TRUE))
rename <- function(tx) {
  rox <- grepl("^\\s*#'", tx)
  # a help page's link to a ramb function that is not copied: named as ramb's, in code type
  m <- gregexpr("\\[\\.?rb_[A-Za-z0-9_]+\\(\\)\\]", tx)
  regmatches(tx, m) <- lapply(regmatches(tx, m), function(h) {
    f <- sub("^\\[(.*)\\(\\)\\]$", "\\1", h)
    ifelse(f %in% taken_r, h, sprintf("`ramb::%s()`", f))
  })
  # only the names of ramb's own objects change: one that is copied takes os_, one that is not is written as ramb's.
  # Every other rb_ word (a column or a table name inside the SQL, a temporary column) stays as it is: it is the
  # code's own business, and changing it in one place and not another would break the query
  tx <- gsub("ramb:::?(\\.?rb_[A-Za-z0-9_]+)", "\\1", tx)
  for (r in names(replaced)) tx[rox] <- gsub(paste0("(?<![A-Za-z0-9_])", r, "(?![A-Za-z0-9_])"), replaced[[r]], tx[rox], perl = TRUE)
  m <- gregexpr("(?<![A-Za-z0-9_])\\.?rb_[A-Za-z0-9_]+", tx, perl = TRUE)
  regmatches(tx, m) <- lapply(regmatches(tx, m), function(h) {
    ifelse(h %in% taken, sub("^(\\.?)rb_", "\\1os_", h),
           ifelse(h %in% known, paste0(ifelse(h %in% exported, "ramb::", "ramb:::"), h), h))
  })
  # the package's own name where the code reads it
  tx[!rox] <- gsub("\"ramb\\.", "\"ossf.", tx[!rox])
  tx[!rox] <- gsub("asNamespace\\(\"ramb\"\\)", "asNamespace(\"ossf\")", tx[!rox])
  tx[!rox] <- gsub("package = \"ramb\"", "package = \"ossf\"", tx[!rox])
  tx[!rox] <- gsub("packageVersion\\(\"ramb\"\\)", "packageVersion(\"ossf\")", tx[!rox])
  tx <- gsub("options\\(ramb\\.", "options(ossf.", tx)
  # a comment that names the data a method was tried on is worded generically: ossf names no case study
  tx <- gsub("\\S+'s case study", "a case study", tx, perl = TRUE)
  tx <- gsub("\\bCS[0-9]+'s\\b", "a case study's", tx, perl = TRUE)
  tx <- gsub("\\bCS[0-9]+\\b", "a case study", tx, perl = TRUE)
  tx
}
drop_examples <- function(tx) {
  out <- logical(length(tx))
  inside <- FALSE
  for (i in seq_along(tx)) {
    if (grepl("^\\s*#'\\s*@examples", tx[i])) inside <- TRUE
    else if (inside && (grepl("^\\s*#'\\s*@[a-zA-Z]", tx[i]) || !grepl("^\\s*#'", tx[i]))) inside <- FALSE
    out[i] <- inside
  }
  tx[!out]
}

# Written -------------------------------------------------------------------------------------------------------------------
version <- read.dcf(file.path(ramb, "DESCRIPTION"))[, "Version"]
commit <- tryCatch(system2("git", c("-C", ramb, "rev-parse", "--short", "HEAD"), stdout = TRUE), error = function(e) "unknown")
dirty <- length(system2("git", c("-C", ramb, "status", "--porcelain", "--untracked-files=no", "--", "R", "src"), stdout = TRUE)) > 0
unlink(list.files("R", "^ramb_.*\\.R$", full.names = TRUE))
log <- list()
for (f in unique(vapply(items, `[[`, "", "file"))) {
  it <- items[vapply(items, function(x) x$file == f && ((!is.na(x$name) && x$name %in% taken_r) || x$target %in% taken_r), NA)]
  if (!length(it)) next
  head <- c(sprintf("# Copied from ramb %s (%s%s), R/%s, by data-raw/copy_from_ramb.R: do not edit here.", version, commit,
                    if (dirty) ", working tree changed" else "", f),
            "# The text is ramb's, with the names changed (rb_ to os_); a change is made in ramb and the copy made again.", "")
  body <- unlist(lapply(it, function(x) c(rename(drop_examples(x$text)), "")))
  writeLines(c(head, body), file.path("R", paste0("ramb_", f)))
  it_name <- vapply(it, function(x) if (is.na(x$name)) paste0("(", x$target, " changed)") else x$name, "")
  log[[f]] <- data.frame(ramb_file = f, ramb_name = it_name,
                         from = as.integer(vapply(it, `[[`, 1, "from")), to = as.integer(vapply(it, `[[`, 1, "to")))
}
log <- do.call(rbind, log)
log$ossf_name <- sub("^(\\.?)rb_", "\\1os_", log$ramb_name)
log$exported <- log$ramb_name %in% exported
clash <- intersect(log$ossf_name, own)
if (length(clash)) stop("a copy takes the name of ossf's own: ", paste(clash, collapse = ", "))
utils::write.csv(log, "data-raw/copy_from_ramb.csv", row.names = FALSE)

# The compiled code: a source file is taken when it exports a function that is called, with the headers it includes -----
dir.create("src", showWarnings = FALSE)
src <- list.files(file.path(ramb, "src"), "\\.(cpp|c|h)$", full.names = TRUE)
src <- src[basename(src) != "RcppExports.cpp"]
exports_of <- function(f) {
  tx <- readLines(f, warn = FALSE)
  at <- grep("^// \\[\\[Rcpp::export", tx)
  if (!length(at)) return(character())
  # the signature may start a line or two below the tag
  vapply(at, function(i) {
    sig <- paste(tx[(i + 1):min(i + 4, length(tx))], collapse = " ")
    sub("^.*?([A-Za-z_][A-Za-z0-9_]*)\\s*\\(.*$", "\\1", sig, perl = TRUE)
  }, "")
}
need <- src[vapply(src, function(f) any(exports_of(f) %in% taken_cpp), NA)]
repeat {
  inc <- unique(unlist(lapply(need, function(f) sub('^#include "(.*)"$', "\\1", grep('^#include "', readLines(f, warn = FALSE), value = TRUE)))))
  more <- file.path(ramb, "src", c(inc, sub("\\.h$", ".c", inc), sub("\\.h$", ".cpp", inc)))
  more <- setdiff(more[file.exists(more)], need)
  if (!length(more)) break
  need <- c(need, more)
}
extra <- setdiff(unlist(lapply(need, exports_of)), taken_cpp)
if (length(extra)) message("compiled and exported with their file, though not called: ", paste(extra, collapse = ", "))
# a file is written only when it changed, and one no longer needed is removed: an untouched file keeps its time, so
# the compiled code is not built again for nothing (and two R sessions loading the package do not both build it)
for (f in need) {
  to <- file.path("src", basename(f))
  if (!file.exists(to) || !identical(unname(tools::md5sum(f)), unname(tools::md5sum(to)))) file.copy(f, to, overwrite = TRUE)
}
gone <- setdiff(list.files("src", "\\.(cpp|c|h)$"), c(basename(need), "RcppExports.cpp"))
unlink(file.path("src", gone))
dir.create("inst", showWarnings = FALSE)
writeLines(c("ossf carries copies of ramb's code (data-raw/copy_from_ramb.R), the third-party code below with it, where",
             "the copied functions use it. ramb's notice follows as it is; read \"ramb\" as ramb and its copies in ossf.", "",
             readLines(file.path(ramb, "inst", "COPYRIGHTS"), warn = FALSE)), "inst/COPYRIGHTS")
# the names R CMD check is told are columns, not variables: ramb's list whole
gl <- readLines(file.path(ramb, "R", "globals.R"), warn = FALSE)
writeLines(c(sprintf("# Copied from ramb %s (%s), R/globals.R, by data-raw/copy_from_ramb.R: do not edit here.", version, commit), "", rename(gl),
             "", "# a column ramb's R/whack.R makes and names, missing from its list", "utils::globalVariables(\".rb_fc\")"),
           "R/ramb_globals.R")

cat(sprintf("ramb %s (%s%s): %d objects of %d files copied (%d exported), %d source files\n", version, commit,
            if (dirty) ", working tree changed" else "", nrow(log), length(unique(log$ramb_file)), sum(log$exported), length(need)))
print(table(log$ramb_file))
cat("source files:", paste(basename(need), collapse = " "), "\n")
used <- unlist(lapply(list.files("R", "^ramb_.*\\.R$", full.names = TRUE), function(f) {
  tx <- grep("^\\s*#", readLines(f, warn = FALSE), invert = TRUE, value = TRUE)
  unlist(regmatches(tx, gregexpr("[A-Za-z][A-Za-z0-9.]*(?=::)", tx, perl = TRUE)))
}))
cat("packages the copies name:\n")
print(sort(table(used), decreasing = TRUE))

# The flags in one call: the working copy of fishycode's study of plan 086 ----------------------------------------------
# The step flag_pings sources proto_flag_ping() from that study (R/proto.R): one function that runs the flags in
# a fixed order and puts every ping in one outcome, on ramb's flags above. It is ramb-bound and not in ramb yet, so it
# is copied from the study, as it is: proto_flag_ping() is os_flag_ping(), its helpers proto_<x>() are .os_<x>(), the
# ramb functions take their os_ names. Its summary by month (proto_quality(), which needs tidyr) is left out. The help
# page is written here, the code is the study's. Skipped, with the file as it is, where the study is not at hand.
study <- Sys.getenv("OSSF_STUDY", file.path(Sys.getenv("FISHYDATA"), "archive", "2026-11-22-robust-ping-flags"))
proto <- file.path(study, "R", "proto.R")
if (!file.exists(proto)) {
  message("the study's R/proto.R is not at hand (FISHYDATA or OSSF_STUDY): R/study_flag_ping.R left as it is")
} else {
  tx <- readLines(proto, warn = FALSE)
  p <- parse(proto, keep.source = TRUE)
  sr <- attr(p, "srcref")
  body <- character()
  pnames <- character()
  for (i in seq_along(p)) {
    e <- p[[i]]
    if (!(is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]))) next   # the library() calls
    n <- as.character(e[[2]])
    if (n == "proto_quality") next
    pnames <- c(pnames, n)
    a <- sr[[i]][1]
    k <- a - 1
    while (k >= 1 && grepl("^\\s*#", tx[k])) k <- k - 1
    block <- tx[(k + 1):sr[[i]][3]]
    if (n == "proto_flag_ping") block <- block[!grepl("^#", block)]   # its heading comment gives way to the help page
    body <- c(body, if (n == "proto_flag_ping") "@@HELP@@", block, "")
  }
  to <- stats::setNames(sub("^proto_", ".os_", pnames), pnames)
  to[["proto_flag_ping"]] <- "os_flag_ping"
  clash <- intersect(to, c(log$ossf_name, own, ".os_speed", ".os_dist_m", ".os_smooth", ".os_min_n", ".os_range_estimated", ".os_range_measured"))
  if (length(clash)) stop("the study's names clash with ossf's: ", paste(clash, collapse = ", "))
  body <- rename(body)
  for (n in names(to)) body <- gsub(paste0("(?<![A-Za-z0-9_.])", n, "(?![A-Za-z0-9_])"), to[[n]], body, perl = TRUE)
  # a comment that names the data a method was tried on is worded generically: ossf names no case study
  body <- gsub("\\S+'s case study", "a case study", body, perl = TRUE)
  body <- gsub("\\bCS[0-9]+'s\\b", "a case study's", body, perl = TRUE)
  body <- gsub("\\bCS[0-9]+\\b", "a case study", body, perl = TRUE)
  left <- unique(unlist(regmatches(body[!grepl("^\\s*#", body)], gregexpr("ramb:::?\\.?rb_[A-Za-z0-9_]+", body[!grepl("^\\s*#", body)]))))
  if (length(left)) stop("the study's code calls ramb functions that are not copied: ", paste(left, collapse = ", "))
  help <- c(
    "#' Flag the pings in one call, every ping in one outcome",
    "#'",
    "#' Runs the flags in a fixed order and gives each ping one outcome and the step that decided it. Nothing is",
    "#' dropped: a ping a step labels is kept with its label, and later steps only stop using it as a reference. The",
    "#' order: what the record says of itself (an invalid position, a row no device wrote, a position with no fix);",
    "#' copies (a stretch written again at one time shift, one report twice across feeds); the time (a counter that",
    "#' runs apart from the written time, a clock jump); then the speed test on what is left, in port and at sea, at",
    "#' each vessel's own limits ([os_create_ping_limits()], [os_flag_ping_impossible()]). A ping no step labels is",
    "#' `\"kept\"`.",
    "#'",
    "#' The code is the working copy of fishycode's study of plan 086 (its `proto_flag_ping()`), taken as it is: bound",
    "#' for ramb, not yet in it, and not reviewed by a person. It names no dataset's values: the record's own witnesses",
    "#' are given by argument, each a column's name.",
    "#'",
    "#' @param x A data frame of pings: `vid`, `time`, `lon`, `lat`.",
    "#' @param speed,course The recorded speed (knots) and course, as column names, or `NULL`.",
    "#' @param fix A logical column, `TRUE` where the device had a fix, or `NULL`.",
    "#' @param synthetic A logical column, `TRUE` on a row no device wrote (a fill), or `NULL`.",
    "#' @param counter_s A column that counts seconds apart from the written time (an engine-hours counter), or `NULL`.",
    "#' @param in_port A logical column, `TRUE` in a port (from [os_flag_ping_polygon()]), or `NULL`.",
    "#' @param feed,stream The provider and the stream of each ping, as column names, or `NULL`.",
    "#' @param method The speed test: `\"local_limits\"` (each vessel's own limits and the track around a ping),",
    "#'   `\"local_limits_speed\"` (the same, judged by the recorded speed), or another the study tried (`\"clean25\"`,",
    "#'   `\"clean_own\"`, `\"pair_vote\"`, and the `_veto` and `_resid` forms of the first two).",
    "#' @param anchor Which side of a jump is believed: `\"kept\"` (the pings before it) or `\"vote\"` (the pings after",
    "#'   it decide).",
    "#' @param by `\"vessel\"` (each vessel's whole track), `\"trip\"` (each run in port or at sea alone) or `\"both\"`,",
    "#'   which keeps the two views apart in the outcome.",
    "#' @param steps The steps run besides the speed test: of `\"record\"`, `\"copies\"`, `\"counter\"`,",
    "#'   `\"counter_blocks\"`, `\"zero_speed_na\"`, `\"speed_check\"`, `\"time\"`.",
    "#' @param copy_min_run The fewest pings in a row that make a copied stretch.",
    "#' @param copy_given A logical column with the copies already found, or `NULL` to find them here.",
    "#' @param feed_priority The feeds, the one to keep first, where a report came twice.",
    "#' @param evidence `TRUE` to add what decided each flagged ping (the side, the local speed, each leg's excess).",
    "#' @param limits_from A logical column: the rows each vessel's limits are measured on (leave out a device carried",
    "#'   on land), or `NULL` for every row. The rows left out are judged as any other.",
    "#'",
    "#' @return `x`, the same rows in the same order, with `ping_outcome` (one a ping), `ping_step` (the step that",
    "#'   decided it), `ping_flag_basis`, `ping_fits` (whether a ping set aside fits the kept track), `ping_speed_stage`,",
    "#'   `ping_time_off`, `ping_time_jump_s`, `ping_trip` (the run in port or at sea) and `time_eff` (the time the speed",
    "#'   test used). Each vessel's limits are the attribute `\"limits\"`.",
    "#' @import dplyr",
    "#' @export"
  )
  at <- match("@@HELP@@", body)
  body <- c(body[seq_len(at - 1)], help, body[-seq_len(at)])
  # the study's dplyr calls name columns as variables: told to R CMD check, as ramb's globals.R does for its own
  env <- new.env()
  eval(parse(text = body), env)
  g <- unique(unlist(lapply(ls(env, all.names = TRUE), function(f) codetools::findGlobals(get(f, env), merge = FALSE)$variables)))
  # used as variables, not called: a name R also has as a function (D, g, shift) is still a column here
  g <- sort(setdiff(g, c(ls(env, all.names = TRUE), "T", "F", "pi", "letters", "LETTERS")))
  body <- c(body, "# the columns the code above names as variables", sprintf("utils::globalVariables(c(%s))",
                                                                              paste(sprintf('"%s"', g), collapse = ", ")))
  writeLines(c(sprintf("# Copied from fishycode's study of plan 086 (%s), R/proto.R, by data-raw/copy_from_ramb.R: do not edit here.",
                       basename(study)),
               "# The text is the study's working copy, with the names changed (proto_flag_ping() to os_flag_ping(), proto_ to",
               "# .os_, rb_ to os_); a change is made in the study and the copy made again. The help page is ossf's.", "", body),
             "R/study_flag_ping.R")
  cat(sprintf("the study's flags in one call: %d functions copied to R/study_flag_ping.R\n", length(pnames)))
}
