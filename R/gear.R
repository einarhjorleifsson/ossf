# A haul's gear from what the track says of it: Lattanzi et al.'s models, in R. ossf's own.
# Lattanzi et al. 2026's Python model in R, recoded from the public code of the paper
# (figshare 10.6084/m9.figshare.29647448, CC BY 4.0; fishycode's $FISHYDATA/raw/external/cnr/figshare_29647448/).
# Its three models that matter: the fine-tuned random forest of
# 3_fine_tuning_best_model_predict_hauls_gears_v2_gc.py (its choice for nets against pots), the regularised XGBoost of
# 3_fine_tuning_best_model_predict_hauls_gears_v2_sp.py (its choice for the specific gears), and the logistic regression
# of predicting_gears_functions.py, the paper's statistical baseline; the predictors prepared as that file does, and
# the grouped permutation importance of its calculate_permutation_importance_grouped(). Recoded, not copied: the
# Python's scikit-learn forest is ranger's here, its logistic regression nnet's multinom(), its XGBoost the R package
# of the same library. Lattanzi P., Mendo T., Galdelli A., Tassetti A.N. (2026). Machine learning-based prediction of
# passive gears from vessel tracking data in small-scale multi-gear fisheries. Ecological Informatics 94: 103670.
# https://doi.org/10.1016/j.ecoinf.2026.103670
#
# What differs from the Python, and why:
# - Every haul with a known gear is predicted by a model that never saw its group (out-of-fold), not one 70/30 split:
#   the paper's own finding is that hauls split at random score higher than hauls held out by trip or by vessel
#   (its Fig. 16), because a trip's hauls share its place, day and gear. The group is the user's to name, and must be.
# - XGBoost's rounds are found on a fifth of the training groups, not on the test set: the Python stopped its boosting
#   on the hauls it then scored, so its score had seen them.
# - The forest and XGBoost take the numbers as they are: the Python's log(x + 1) and min-max scaling change no split.
#   The logistic regression takes them so scaled, as the Python. No median imputation: the Python drops every haul with
#   a missing predictor before it imputes, so it imputes nothing; here such a haul is kept and labelled.
# - The logistic regression is the Python's default configuration (L2, C = 1); its tuned grid is not searched.
#
# Drafted with AI assistance (Claude), 2026-11-25; not yet checked by a person.

#' A haul's gear from what the track says of it
#'
#' Predicts the gear of each haul (or any unit: a set, a trip) from its features, by a model trained on the hauls whose
#' gear is known: the models of Lattanzi et al. (2026), translated from the paper's public Python code, with the
#' classes weighed equally. The gear may be a category (nets, pots) or a specific gear (GNS, GTR, FPO): one call a
#' level. Three methods (`method`):
#' * `"forest"`: a random forest of classification trees, the paper's choice for nets against pots (ranger).
#' * `"xgboost"`: gradient-boosted shallow trees, the paper's choice for the specific gears (xgboost).
#' * `"logistic"`: a multinomial logistic regression, the paper's statistical baseline (nnet, shipped with R).
#'
#' **Held out by group.** Each haul whose gear is known is predicted by a model trained on the other folds, its whole
#' group (a trip, a vessel) in one fold, so no haul is judged by a model that saw its group. Hauls of one trip share
#' its place, its day and nearly always its gear, so a model that saw a trip's other hauls knows the answer from where
#' and when; the paper found the same (its Fig. 16: lower scores grouped by trip, lower still by vessel). Name the trip
#' to ask how well a new trip of a known vessel is told, the vessel to ask about a vessel never seen. The haul's own id
#' as `group` gives the Python's random split of hauls. The folds are made as scikit-learn's `GroupKFold` makes them
#' (the largest group first, each to the fold with the fewest hauls so far), the groups shuffled by `seed` first.
#'
#' **Predicted.** A haul whose gear is not known is predicted by the model trained on every haul whose gear is.
#'
#' **What gives the answer away.** A feature made from the gear itself (whether the trip changes gear, the trip's
#' gear, whether the haul has a gear at all) predicts the gear for free, and so does a haul that exists only because
#' its gear was known (hauls cut from a call that used the gear). The function cannot see this: choose the features
#' and the hauls so.
#'
#' **The predictors.** A feature that is not a number (a character, factor or logical column) is a category, one
#' predictor column a level, as the Python's one-hot encoding: give the month and the hour as factors to have them as
#' categories, as the paper does. A level not met in training is a haul with none of that feature's columns set, as
#' scikit-learn's `handle_unknown = "ignore"`. The forest and XGBoost take the numbers as they are (the Python's
#' log(x + 1) and min-max scaling change no split); the logistic regression takes each as log(x + 1) where no training
#' haul's value is negative, then scaled to 0-1 over the training hauls, as the Python.
#'
#' **The classes weighed equally** (`class_weight = "balanced"`: each class's weight the hauls over the classes times
#' its hauls), in each method as the Python weighs them: the forest in the splits (ranger's `class.weights`) and in the
#' probabilities (each tree's share of each class weighed by its weight, then averaged over the trees, as scikit-learn
#' weighs each leaf's class counts); XGBoost and the logistic regression by each haul's weight in the fit.
#'
#' **The methods' values**, from the paper's code; tuned on its ten vessels:
#' * the forest: `trees` trees (`n_estimators`, 1000), each grown on a bootstrap sample to at most `depth` levels
#'   (`max_depth`, 10), each leaf holding at least `leaf` hauls (`min_samples_leaf`, 5), each split choosing among a
#'   `share` of the predictor columns (`max_features`, 0.5).
#' * XGBoost: `max_depth = 3`, `learning_rate = 0.03`, `subsample = 0.5`, `colsample_bytree = 0.5`,
#'   `min_child_weight = 10`, `reg_alpha = 10`, `reg_lambda = 200`, `gamma = 0`, each replaced by an argument of the
#'   same name in `...`. The rounds: boosting stops once 100 rounds bring no gain in log loss on a fifth of the
#'   training groups held aside (at most 40,000 rounds), and the model is fitted again on every training haul with
#'   that many rounds. With fewer than five training groups there is no stop set, and 1,000 rounds are run.
#' * the logistic regression: the Python's default, an L2 penalty with `C = 1`: in nnet's terms `decay = 0.5`, which
#'   penalises the intercepts too, as scikit-learn does not; `maxit = 1000`. Either replaced in `...`.
#'
#' The importance of each feature (attribute `"importance"`) is the paper's grouped permutation importance, on the
#' held-out hauls: each feature's columns shuffled together among a fold's held-out hauls, `repeats` times, and the
#' drop in the share of them predicted right. Other given values: `folds = 5` (the paper's cross-validation's),
#' `repeats = 10` (its permutations'), `seed = 42`.
#'
#' @param hauls A data frame, a row a haul.
#' @param gear The name of the column of `hauls` holding the known gear, `NA` where it is not known.
#' @param features The names of the columns to predict from.
#' @param group The name of the column (or names of the columns) whose hauls are held out together: a trip, a vessel.
#'   A haul with no group (`NA`) is a group of its own.
#' @param method `"forest"` (the default), `"xgboost"` or `"logistic"`, as above.
#' @param folds The number of folds, at most the number of groups.
#' @param trees,depth,leaf,share The forest, as above.
#' @param repeats How often each feature is shuffled for its importance; `0` for none.
#' @param seed The seed of the folds' shuffle and of the models. The caller's random numbers are left as they were.
#' @param threads The threads the forest and XGBoost may use; `NULL`, their own default (all).
#' @param name The name of the column added, and the stem of the others.
#' @param ... Further arguments to the method: to [ranger::ranger()] for the forest; XGBoost's parameters by their
#'   names (`max_depth = 4`); to [nnet::multinom()] for the logistic regression (`decay = 0.1`).
#'
#' @return `hauls`, the same rows in the same order, with three columns added: `name` (the gear predicted, `NA` where
#'   none is), `<name>_prob` (the model's probability of that gear) and `<name>_basis`: `"held out"` (the gear known,
#'   predicted by a model that never saw its group), `"predicted"` (the gear not known, predicted by the model of every
#'   haul whose gear is), or `"none: "` and why (a feature missing, naming which; no haul with a known gear).
#'   Attributes: `"probabilities"`, a matrix a row a haul and a column a gear; `"folds"`, a row a group with its fold
#'   and hauls; `"importance"`, a row a feature, the mean drop in the share right (`drop`) and its standard deviation
#'   (`sd`); `"model"`, the model of every haul whose gear is known (ranger's, xgboost's or nnet's); `"settings"`, the
#'   values used (for XGBoost, the rounds of each fold's model and of the last).
#' @export
#'
#' @examples
#' if (requireNamespace("ranger", quietly = TRUE)) {
#'   # 40 trips of 5 hauls; pots hauled further out and soaked longer than nets
#'   set.seed(1)
#'   trips <- data.frame(trip = 1:40, gear = rep(c("Pots", "Nets"), c(28, 12)))
#'   hauls <- trips[rep(1:40, each = 5), ]
#'   hauls$distance_m <- ifelse(hauls$gear == "Pots", 5500, 3300) * exp(rnorm(200, 0, 0.3))
#'   hauls$soak_h <- ifelse(hauls$gear == "Pots", 48, 18) * exp(rnorm(200, 0, 0.5))
#'   hauls$month <- factor(sample(1:12, 200, replace = TRUE))
#'   hauls$gear[sample(200, 20)] <- NA # gear not known on 20 hauls
#'   out <- os_add_gear(hauls, "gear", c("distance_m", "soak_h", "month"), group = "trip", trees = 200, threads = 1)
#'   table(out$gear_pred_basis)
#'   table(known = out$gear, predicted = out$gear_pred)
#'   attr(out, "importance")
#'   # the paper's statistical baseline on the same folds
#'   lr <- os_add_gear(hauls, "gear", c("distance_m", "soak_h", "month"), group = "trip", method = "logistic")
#'   table(known = lr$gear, predicted = lr$gear_pred)
#' }
os_add_gear <- function(hauls, gear, features, group, method = c("forest", "xgboost", "logistic"), folds = 5L,
                        trees = 1000L, depth = 10L, leaf = 5L, share = 0.5, repeats = 10L, seed = 42L, threads = NULL,
                        name = "gear_pred", ...) {
  method <- match.arg(method)
  engine <- c(forest = "ranger", xgboost = "xgboost", logistic = "nnet")[[method]]
  if (!requireNamespace(engine, quietly = TRUE)) stop("os_add_gear(method = \"", method, "\") needs the package ", engine, ".", call. = FALSE)
  if (!is.data.frame(hauls)) stop("`hauls` must be a data frame.", call. = FALSE)
  if (missing(group)) stop("`group` must be named: the hauls held out together (a trip, a vessel; the haul's id for none).", call. = FALSE)
  miss <- setdiff(c(gear, features, group), names(hauls))
  if (length(miss)) stop("`hauls` lacks: ", paste(miss, collapse = ", "), call. = FALSE)
  if (gear %in% features) stop("The gear cannot be its own feature.", call. = FALSE)
  if (!length(features) || anyDuplicated(features)) stop("`features` must name each column once.", call. = FALSE)
  added <- paste0(name, c("", "_prob", "_basis"))
  if (length(intersect(added, names(hauls)))) stop("`hauls` already has: ", paste(intersect(added, names(hauls)), collapse = ", "), call. = FALSE)
  if (folds < 2 || trees < 1 || leaf < 1 || share <= 0 || share > 1 || repeats < 0) stop("`folds` must be 2 or more, `trees` and `leaf` 1 or more, `share` in (0, 1], `repeats` 0 or more.", call. = FALSE)

  # the caller's random numbers are left as they were
  had_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  if (had_seed) old_seed <- get(".Random.seed", envir = globalenv(), inherits = FALSE)
  on.exit(if (had_seed) assign(".Random.seed", old_seed, envir = globalenv()) else suppressWarnings(rm(".Random.seed", envir = globalenv())))
  set.seed(seed)

  d <- as.data.frame(hauls, stringsAsFactors = FALSE)
  n <- nrow(d)
  X <- d[features]
  is_num <- vapply(X, function(v) is.numeric(v) && !is.factor(v), TRUE)
  for (f in features[!is_num]) X[[f]] <- as.character(X[[f]])
  y <- as.character(d[[gear]])
  complete <- stats::complete.cases(X)
  known <- !is.na(y) & complete
  classes <- sort(unique(y[known]))
  g <- do.call(paste, c(lapply(d[group], as.character), sep = "\r"))
  no_group <- Reduce(`|`, lapply(d[group], is.na))
  g[no_group] <- paste0("\u0001row", which(no_group))

  pred <- rep(NA_character_, n)
  prob <- rep(NA_real_, n)
  P <- matrix(NA_real_, n, length(classes), dimnames = list(NULL, classes))
  basis <- rep(NA_character_, n)
  if (any(!complete)) {
    gone <- apply(is.na(X[!complete, , drop = FALSE]), 1, function(r) paste(features[r], collapse = ", "))
    basis[!complete] <- paste0("none: a feature missing (", gone, ")")
  }
  basis[complete & is.na(y)] <- "predicted"
  basis[known] <- "held out"
  imp <- data.frame(feature = features, drop = NA_real_, sd = NA_real_)
  fold_tab <- data.frame(group = character(0), fold = integer(0), hauls = integer(0))
  model <- NULL
  rounds <- NULL
  if (!any(known)) {
    basis[complete] <- "none: no haul with a known gear"
  } else {
    # the folds, as GroupKFold: the largest group first, each to the fold with the fewest hauls so far
    sizes <- table(g[known])
    ug <- sample(names(sizes))
    ug <- ug[order(-as.integer(sizes[ug]))]
    k <- min(as.integer(folds), length(ug))
    load <- integer(k)
    fold_of <- integer(length(ug))
    for (i in seq_along(ug)) { j <- which.min(load); fold_of[i] <- j; load[j] <- load[j] + sizes[[ug[i]]] }
    names(fold_of) <- ug
    fold_tab <- data.frame(group = ug, fold = unname(fold_of), hauls = as.integer(sizes[ug]))
    fold <- rep(NA_integer_, n)
    fold[known] <- fold_of[g[known]]

    forest <- list(trees = trees, depth = depth, leaf = leaf, share = share)
    fit <- function(rows) .os_gear_fit(method, X[rows, , drop = FALSE], y[rows], g[rows], is_num, forest, seed, threads, ...)
    hit_drop <- matrix(0, length(features), max(repeats, 1L))
    for (j in seq_len(k)) {
      te <- which(fold %in% j)
      m <- fit(known & !(fold %in% j))
      rounds <- c(rounds, m$rounds)
      pj <- .os_gear_predict(m, X[te, , drop = FALSE], classes)
      P[te, ] <- pj
      truth <- y[te]
      right <- classes[max.col(pj, ties.method = "first")] == truth
      if (repeats > 0) {
        for (fi in seq_along(features)) for (r in seq_len(repeats)) {
          Xp <- X[te, , drop = FALSE]
          Xp[[features[fi]]] <- Xp[[features[fi]]][sample.int(length(te))]
          pp <- .os_gear_predict(m, Xp, classes)
          hit_drop[fi, r] <- hit_drop[fi, r] + sum(right) - sum(classes[max.col(pp, ties.method = "first")] == truth)
        }
      }
    }
    n_held <- sum(known)
    if (repeats > 0) imp <- data.frame(feature = features, drop = rowMeans(hit_drop) / n_held,
                                       sd = apply(hit_drop / n_held, 1, function(v) if (length(v) > 1) stats::sd(v) else NA_real_))
    model <- fit(known)
    rounds <- c(rounds, model$rounds)
    todo <- which(complete & is.na(y))
    if (length(todo)) P[todo, ] <- .os_gear_predict(model, X[todo, , drop = FALSE], classes)
    done <- which(complete)
    pick <- max.col(P[done, , drop = FALSE], ties.method = "first")
    pred[done] <- classes[pick]
    prob[done] <- P[cbind(done, pick)]
    model <- model$model
  }

  out <- .os_put_columns(hauls, stats::setNames(list(pred, prob, basis), added))
  attr(out, "probabilities") <- P
  attr(out, "folds") <- fold_tab
  attr(out, "importance") <- imp
  attr(out, "model") <- model
  attr(out, "settings") <- c(list(gear = gear, features = features, group = group, method = method,
                                  folds = nrow(unique(fold_tab["fold"])), repeats = repeats, seed = seed, classes = classes),
                             switch(method, forest = list(trees = trees, depth = depth, leaf = leaf, share = share),
                                    xgboost = list(rounds = rounds), logistic = list()),
                             list(...))
  out
}

# One model on the hauls given: the categories one column a level (from these hauls), the classes weighed equally
.os_gear_fit <- function(method, X, y, g, is_num, forest, seed, threads, ...) {
  levels <- lapply(names(X)[!is_num], function(f) sort(unique(X[[f]])))
  names(levels) <- names(X)[!is_num]
  cls <- sort(unique(y))
  m <- list(method = method, model = NULL, only = NULL, levels = levels, is_num = is_num, scale = NULL, rounds = NULL)
  if (length(cls) < 2) { m$only <- cls; return(m) }
  if (method == "logistic") m$scale <- .os_gear_scaling(X, is_num)
  xm <- .os_gear_columns(.os_gear_scale(X, m$scale), is_num, levels)
  yy <- factor(y, levels = cls)
  w_cls <- stats::setNames(length(y) / (length(cls) * as.vector(table(yy))), cls)
  m$weights <- w_cls
  w <- unname(w_cls[y])
  extra <- list(...)
  if (method == "forest") {
    args <- list(x = xm, y = yy, num.trees = forest$trees, max.depth = forest$depth, min.bucket = forest$leaf, min.node.size = 2,
                 mtry = max(1L, floor(forest$share * ncol(xm))), probability = TRUE, class.weights = unname(w_cls),
                 replace = TRUE, sample.fraction = 1, seed = seed)
    if (!is.null(threads)) args$num.threads <- threads
    m$model <- do.call(ranger::ranger, utils::modifyList(args, extra))
  } else if (method == "xgboost") {
    xx <- as.matrix(xm)
    lab <- as.integer(yy) - 1L
    two <- length(cls) == 2
    par <- utils::modifyList(list(max_depth = 3, learning_rate = 0.03, subsample = 0.5, colsample_bytree = 0.5,
                                  min_child_weight = 10, reg_alpha = 10, reg_lambda = 200, gamma = 0), extra)
    par <- c(par, list(objective = if (two) "binary:logistic" else "multi:softprob", eval_metric = if (two) "logloss" else "mlogloss",
                       seed = seed), if (!two) list(num_class = length(cls)), if (!is.null(threads)) list(nthread = threads))
    dm <- function(i, weighted = TRUE) xgboost::xgb.DMatrix(xx[i, , drop = FALSE], label = lab[i], weight = if (weighted) w[i], nthread = threads)
    # the rounds, from a fifth of the training groups held aside (the stop set carries no weights, as the Python's)
    ug <- unique(g)
    rounds <- 1000L
    if (length(ug) >= 5) {
      st <- g %in% sample(ug, round(length(ug) / 5))
      b <- xgboost::xgb.train(par, dm(!st), nrounds = 40000L, evals = list(stop = dm(st, weighted = FALSE)),
                              early_stopping_rounds = 100L, verbose = 0)
      rounds <- as.integer(attributes(b)$early_stop$best_iteration)
    }
    m$rounds <- rounds
    m$model <- xgboost::xgb.train(par, dm(rep(TRUE, nrow(xx))), nrounds = rounds, verbose = 0)
  } else {
    args <- utils::modifyList(list(decay = 0.5, maxit = 1000L, trace = FALSE, MaxNWts = 100000L), extra)
    m$model <- do.call(nnet::multinom, c(list(formula = .y ~ ., data = cbind(xm, .y = yy), weights = w), args))
  }
  m
}

# The probabilities of every class (the classes of the whole call, 0 for one the model never saw)
.os_gear_predict <- function(m, X, classes) {
  out <- matrix(0, nrow(X), length(classes), dimnames = list(NULL, classes))
  if (!nrow(X)) return(out)
  if (!is.null(m$only)) { out[, m$only] <- 1; return(out) }
  xm <- .os_gear_columns(.os_gear_scale(X, m$scale), m$is_num, m$levels)
  cls <- names(m$weights)
  if (m$method == "forest") {
    # scikit-learn's way: each tree's shares weighed by the class weights, then averaged over the trees (a leaf's
    # value there is its weighed class counts). Weighing the forest's average instead names the rare gears far too
    # often: one tree in three half for a rare gear, and the average carries it. In pieces, to bound the memory.
    lv <- colnames(stats::predict(m$model, data = xm[1, , drop = FALSE])$predictions) # the trees' class order
    w <- m$weights[lv]
    p <- matrix(0, nrow(xm), length(lv), dimnames = list(NULL, lv))
    piece <- max(1L, floor(2e7 / (length(lv) * m$model$num.trees)))
    for (i in split(seq_len(nrow(xm)), ceiling(seq_len(nrow(xm)) / piece))) {
      a <- stats::predict(m$model, data = xm[i, , drop = FALSE], predict.all = TRUE)$predictions
      a <- array(a, c(length(i), length(lv), m$model$num.trees))     # hauls x classes x trees
      a <- sweep(a, 2, w, `*`)
      p[i, ] <- rowMeans(sweep(a, c(1, 3), apply(a, c(1, 3), sum), `/`), dims = 2)
    }
  } else {
    p <- if (m$method == "xgboost") stats::predict(m$model, as.matrix(xm)) else stats::predict(m$model, newdata = xm, type = "probs")
    p <- if (length(cls) == 2) cbind(1 - as.vector(p), as.vector(p)) else matrix(p, nrow(xm), length(cls))
    colnames(p) <- cls
  }
  out[, colnames(p)] <- p
  out
}

# The logistic regression's numbers: log(x + 1) where no training value is negative, then 0-1 over the training hauls
.os_gear_scaling <- function(X, is_num) {
  lapply(stats::setNames(names(X)[is_num], names(X)[is_num]), function(f) {
    v <- X[[f]]
    lg <- all(v >= 0)
    v <- if (lg) log1p(v) else v
    list(log = lg, lo = min(v), hi = max(v))
  })
}
.os_gear_scale <- function(X, scale) {
  for (f in names(scale)) {
    s <- scale[[f]]
    v <- if (s$log) log1p(pmax(X[[f]], 0)) else X[[f]]
    X[[f]] <- if (s$hi > s$lo) (v - s$lo) / (s$hi - s$lo) else 0
  }
  X
}

# The predictor columns, a data frame: a number as it is, a category one 0/1 column a level met in training
.os_gear_columns <- function(X, is_num, levels) {
  cols <- list()
  for (f in names(X)) {
    if (is_num[[f]]) cols[[length(cols) + 1L]] <- as.numeric(X[[f]])
    else for (l in levels[[f]]) cols[[length(cols) + 1L]] <- as.numeric(X[[f]] == l)
  }
  names(cols) <- paste0("x", seq_along(cols))
  as.data.frame(cols)
}
