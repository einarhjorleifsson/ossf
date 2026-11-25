# Copied from ramb 2026.11.24 (b777ac4), R/connection.R, by data-raw/copy_from_ramb.R: do not edit here.
# The text is ramb's, with the names changed (rb_ to os_); a change is made in ramb and the copy made again.

.os_db <- new.env(parent = emptyenv())

.os_connection <- function() {
  con <- .os_db$con
  if (!is.null(con) && DBI::dbIsValid(con)) {
    return(con)
  }
  con <- DBI::dbConnect(duckdb::duckdb())
  DBI::dbExecute(con, sprintf("SET memory_limit = '%s'", .os_memory_limit(con)))
  .os_db$con <- con
  con
}

.os_disconnect <- function() {
  con <- .os_db$con
  .os_db$con <- NULL
  if (!is.null(con) && DBI::dbIsValid(con)) {
    DBI::dbDisconnect(con, shutdown = TRUE)
  }
  invisible()
}

.onUnload <- function(libpath) .os_disconnect()

# The memory ramb's instance may use: options(ossf.memory_limit), a DuckDB size such as "8GB", else a quarter of the
# machine's memory. DuckDB gives every in-memory instance 80 % of it by default, knowing nothing of the others in the
# session (duckdbfs's, the caller's), and takes no percentage, so the quarter is 5/16 of that default.
.os_memory_limit <- function(con) {
  limit <- getOption("ossf.memory_limit")
  if (!is.null(limit)) {
    return(limit)
  }
  default <- DBI::dbGetQuery(
    con,
    "SELECT current_setting('memory_limit') AS v"
  )$v
  bytes <- .os_parse_size(default)
  if (is.na(bytes)) {
    return(default)
  }
  sprintf("%.0fMiB", bytes * 5 / 16 / 2^20)
}

# A DuckDB size ("19.1 GiB", "8GB", "512 MiB") in bytes; NA if it is not one.
.os_parse_size <- function(x) {
  m <- regmatches(x, regexec("^\\s*([0-9.]+)\\s*([KMGT]i?B|bytes)?\\s*$", x))[[
    1
  ]]
  if (length(m) == 0) {
    return(NA_real_)
  }
  unit <- c(
    KiB = 2^10,
    MiB = 2^20,
    GiB = 2^30,
    TiB = 2^40,
    KB = 1e3,
    MB = 1e6,
    GB = 1e9,
    TB = 1e12,
    bytes = 1
  )
  as.numeric(m[2]) * if (m[3] == "") 1 else unit[[m[3]]]
}

# Load a DuckDB extension on `con`, installing it first if it is not there. Without access to DuckDB's repository
# there are three ways in, each tested on 2026-10-27: a file copied into the extension folder counts as installed;
# options(ossf.extension_repository) names a folder or URL laid out as DuckDB's, installed from with INSTALL ... FROM,
# which leaves the connection's settings as they were (SET custom_extension_repository would not, on a caller's
# connection); and DuckDB's downloads go through the web proxy in HTTP_PROXY. ramb loads core extensions only
# (spatial, httpfs): DuckDB 2.0 loads a copied community extension only with the .info file its install writes beside it.
.os_extension <- function(con, name) {
  load <- function() {
    !inherits(
      try(DBI::dbExecute(con, sprintf("LOAD %s", name)), silent = TRUE),
      "try-error"
    )
  }
  if (load()) {
    return(invisible(TRUE))
  }
  repo <- getOption("ossf.extension_repository")
  sql <- if (is.null(repo)) {
    sprintf("INSTALL %s", name)
  } else {
    sprintf("INSTALL %s FROM %s", name, DBI::dbQuoteString(con, repo))
  }
  got <- try(DBI::dbExecute(con, sql), silent = TRUE)
  if (!inherits(got, "try-error") && load()) {
    return(invisible(TRUE))
  }
  version <- DBI::dbGetQuery(con, "SELECT version() AS v")$v
  platform <- DBI::dbGetQuery(con, "PRAGMA platform")$platform
  dir <- DBI::dbGetQuery(
    con,
    "SELECT current_setting('extension_directory') AS v"
  )$v
  why <- if (inherits(got, "try-error")) {
    conditionMessage(attr(got, "condition"))
  } else {
    "It installed but did not load."
  }
  rlang::abort(
    c(
      sprintf(
        "DuckDB's `%s` extension could not be installed and loaded.",
        name
      ),
      x = strsplit(why, "\n")[[1]][1],
      i = "Without access to DuckDB's extension repository, any one of these works:",
      "*" = sprintf(
        "Download http://extensions.duckdb.org/%s/%s/%s.duckdb_extension.gz on another machine, unpack it and copy it into %s.",
        version,
        platform,
        name,
        file.path(dir, version, platform)
      ),
      "*" = "Set the environment variable HTTP_PROXY to your network's web proxy, if it has one.",
      "*" = sprintf(
        "Set options(ossf.extension_repository = <folder or URL>), laid out as <repository>/%s/%s/%s.duckdb_extension.",
        version,
        platform,
        name
      )
    ),
    call = NULL
  )
}

