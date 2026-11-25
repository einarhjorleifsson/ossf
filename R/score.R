# A call against a witness, every row in one outcome. ossf's own, in base R.
#
# Made general from the scoring that checks wrote again by hand (recall, precision, F1), at the maintainer's word (2026-11-25). Drafted with AI assistance (Claude),
# 2026-11-25; not yet checked by a person.

#' Score a call against a witness, every row in one outcome
#'
#' Each row falls in one outcome: both say yes, the call only, the witness only, neither, or not judged (either is
#' missing). From them: recall (of the witness's yes, the share the call found), precision (of the call's yes, the
#' share the witness confirms) and F1 (their harmonic mean). A witness is not the truth: a score says how far two
#' sources agree, and a low precision may be the witness's miss.
#'
#' @param x A data frame.
#' @param call,witness Logical columns of `x`, as strings: the call (`"fishing"`) and the witness (`"hauler"`).
#' @param by Columns to score apart (`c("vid", "gear")`), or `NULL` for one row.
#'
#' @return A data frame, a row a group: `rows`, `both`, `call_only`, `witness_only`, `neither`, `not_judged` (which
#'   sum to `rows`), `recall`, `precision` and `f1`.
#' @export
#' @examples
#' x <- data.frame(vid = c(1, 1, 1, 2, 2), fishing = c(TRUE, TRUE, FALSE, TRUE, NA), hauler = c(TRUE, FALSE, FALSE, TRUE, TRUE))
#' os_score(x, "fishing", "hauler", by = "vid")
os_score <- function(x, call, witness, by = NULL) {
  if (!is.data.frame(x)) stop("`x` must be a data frame.", call. = FALSE)
  miss <- setdiff(c(call, witness, by), names(x))
  if (length(miss)) stop("`x` lacks: ", paste(miss, collapse = ", "), call. = FALSE)
  if (!is.logical(x[[call]]) || !is.logical(x[[witness]])) stop("`call` and `witness` must name logical columns.", call. = FALSE)
  a <- x[[call]]
  b <- x[[witness]]
  judged <- !is.na(a) & !is.na(b)
  grp <- if (is.null(by)) list(all = rep(1L, nrow(x))) else x[by]
  one <- function(i) {
    j <- judged[i]
    tp <- sum(a[i][j] & b[i][j])
    fp <- sum(a[i][j] & !b[i][j])
    fn <- sum(!a[i][j] & b[i][j])
    recall <- tp / (tp + fn)
    precision <- tp / (tp + fp)
    data.frame(rows = length(i), both = tp, call_only = fp, witness_only = fn, neither = sum(!a[i][j] & !b[i][j]),
               not_judged = sum(!j), recall = recall, precision = precision,
               f1 = if (is.na(recall) || is.na(precision) || recall + precision == 0) NA_real_ else 2 * recall * precision / (recall + precision))
  }
  idx <- split(seq_len(nrow(x)), grp, drop = TRUE, lex.order = TRUE)
  out <- do.call(rbind, lapply(idx, one))
  if (!is.null(by)) {
    keys <- do.call(rbind, lapply(idx, function(i) x[i[1], by, drop = FALSE]))
    out <- cbind(keys, out)
  }
  rownames(out) <- NULL
  out
}
