## ----setup, include = FALSE-----------------------------------------------------------------------
# Without the suggested packages, the code is shown but not run, and the
# numbers in the text are not computed
suggested <- rlang::is_installed("dplyr")
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", eval = suggested)
if (!suggested) knitr::knit_hooks$set(evaluate.inline = \(code, envir) "NA")
old_options <- options(width = 100, pillar.sigfig = 3)

## ----library--------------------------------------------------------------------------------------
library(decisions)

## ----saved, include = FALSE-----------------------------------------------------------------------
saved <- readRDS(system.file("extdata", "study_again.rds", package = "decisions"))
show <- function(x) knitr::kable(x, digits = 3, format.args = list(big.mark = ","))
# Helpers for the numbers in the text, so that the text follows the data
fmt <- function(x, digits = 2) formatC(x, digits = digits, format = "f")
pick <- function(table, model_name, dataset_name) {
  table[table$model == model_name & table$dataset == dataset_name, ]
}
model_label <- c(jev = "Jev", openai = "gpt-6-luna", clef = "Clef-Flash")
dataset_label <- c(
  extrusion = "extrusion images", microstructure = "microstructures",
  pan_images = "pans", pipe_staple = "pipe staples", textiles = "textiles"
)
name_setups <- function(table) {
  knitr::combine_words(paste(model_label[table$model], "on the", dataset_label[table$dataset]))
}
run_dates <- format(sort(unique(c(saved$run_dates$first, saved$run_dates$last))))
wording_summary <- saved$wording |>
  dplyr::summarise(
    candidates = dplyr::n(),
    generic_auc = auc[candidate == 1],
    final = max(candidate[kept]),
    final_auc = auc[candidate == max(candidate[kept])],
    .by = c(model, dataset)
  )
calibration_wide <- saved$calibration |>
  dplyr::summarise(
    n = n[1],
    auc = estimate[metric == "roc_auc" & probabilities == "before"],
    auc_after = estimate[metric == "roc_auc" & probabilities == "after"],
    brier_before = estimate[metric == "brier_class" & probabilities == "before"],
    brier_after = estimate[metric == "brier_class" & probabilities == "after"],
    log_loss_before = estimate[metric == "mn_log_loss" & probabilities == "before"],
    log_loss_after = estimate[metric == "mn_log_loss" & probabilities == "after"],
    .by = c(model, dataset)
  )
fold_summary <- saved$folds |>
  dplyr::summarise(
    folds = dplyr::n(), mean = mean(auc), sd = stats::sd(auc),
    min = min(auc), max = max(auc), .by = c(model, dataset)
  )
cost_summary <- saved$calls |>
  dplyr::summarise(
    images = sum(images), input_tokens = sum(input_tokens),
    reported_cost = if (all(is.na(cost))) NA_real_ else sum(cost),
    seconds = sum(seconds), .by = model
  ) |>
  dplyr::mutate(
    tokens_per_image = input_tokens / images, seconds_per_image = seconds / images
  )

## ----paths----------------------------------------------------------------------------------------
image_dir <- "qe_genai/data"
state_dir <- "state"
setup_dir <- "setups"
test_dir <- "test"

## ----folders, eval = FALSE------------------------------------------------------------------------
# dir.create(state_dir)
# dir.create(setup_dir)
# dir.create(test_dir)

## ----sets, script = TRUE--------------------------------------------------------------------------
sets <- system.file("extdata", "study_sets.csv", package = "decisions") |>
  utils::read.csv() |>
  tibble::as_tibble() |>
  dplyr::mutate(
    truth = factor(
      dplyr::if_else(class == "nominal", "normal", "defective"),
      levels = c("normal", "defective")
    ),
    path = file.path(image_dir, image_path)
  )

by_dataset <- split(sets, sets$dataset)

table(sets$dataset, sets$role)

## ----question, script = TRUE----------------------------------------------------------------------
quality_question <- function(descriptions) {
  list(quality = question_choice(
    "Inspect `inspection_image` for manufacturing defects. Select exactly one category.",
    descriptions
  ))
}

generic <- c(
  normal = "No visible manufacturing defects; the part appears conforming.",
  defective = "At least one visible manufacturing defect or anomaly."
)

## ----deciders, eval = FALSE, script = TRUE--------------------------------------------------------
# deciders <- list(
#   jev = decider_typesafe("jev-1.14.0"),
#   openai = decider_openai("gpt-6-luna"),
#   clef = decider_systemone(
#     "http://localhost:30000", "Cloudflare/clef-flash", image_format = "images"
#   )
# )

## ----model-name, eval = FALSE---------------------------------------------------------------------
# model_name <- "jev"

## ----setup-functions, eval = FALSE, script = TRUE-------------------------------------------------
# make_setup <- function(model, dataset, descriptions, k, max_size) {
#   references <- by_dataset[[dataset]] |>
#     dplyr::filter(role == "reference", rank <= k) |>
#     dplyr::arrange(rank)
#   decision_setup(
#     deciders[[model]], quality_question(descriptions),
#     references = list(
#       normal = references$path[references$truth == "normal"],
#       defective = references$path[references$truth == "defective"]
#     ),
#     max_size = max_size, upscale = TRUE
#   )
# }
# 
# score_rows <- function(setup, rows, path) {
#   score_setup(
#     setup, image = rows$path, truth = rows$truth,
#     role = dplyr::if_else(rows$role == "reference", "learning", rows$role),
#     path = path
#   ) |>
#     dplyr::mutate(image_path = list(rows$image_path))
# }
# 
# state_file <- function(...) {
#   file.path(state_dir, paste0(paste(..., sep = "_"), ".json"))
# }

## ----candidates, eval = FALSE---------------------------------------------------------------------
# saved <- readRDS(system.file("extdata", "study_again.rds", package = "decisions"))
# candidates <- dplyr::select(
#   saved$wording, model, dataset, candidate, normal, defective, reason
# )

## ----keep-rule, eval = FALSE, script = TRUE-------------------------------------------------------
# keep_rule <- function(auc, gain = 0.02) {
#   best <- purrr::accumulate(auc, \(best, x) {
#     if (round(x - best, 10) >= gain) x else best
#   })
#   c(TRUE, diff(best) > 0)
# }

## ----wording-setups, eval = FALSE, script = TRUE--------------------------------------------------
# score_candidate <- function(model, dataset, candidate, normal, defective) {
#   setup <- make_setup(
#     model, dataset, c(normal = normal, defective = defective),
#     k = 2, max_size = 768
#   )
#   design <- dplyr::filter(
#     by_dataset[[dataset]],
#     role == "learning" | (role == "reference" & rank > 2)
#   )
#   score_rows(setup, design, state_file("wording", model, dataset, candidate))
# }
# 
# wording <- dplyr::filter(candidates, model == model_name)

## ----wording-score, eval = FALSE, script = TRUE---------------------------------------------------
# wording <- wording |>
#   dplyr::mutate(
#     result = purrr::pmap(
#       list(model, dataset, candidate, normal, defective), score_candidate
#     ),
#     auc = purrr::map_dbl(result, "estimate")
#   )

## ----wording-keep, eval = FALSE, script = TRUE----------------------------------------------------
# wording <- dplyr::mutate(wording, kept = keep_rule(auc), .by = dataset)
# 
# final_wording <- wording |>
#   dplyr::filter(kept) |>
#   dplyr::slice_tail(n = 1, by = dataset) |>
#   dplyr::select(model, dataset, normal, defective) |>
#   dplyr::bind_rows(tibble::tibble(
#     model = model_name, dataset = "pan_images",
#     normal = generic[["normal"]], defective = generic[["defective"]]
#   ))

## ----wording-table, echo = FALSE------------------------------------------------------------------
show(wording_summary)

## ----wording-facts, include = FALSE---------------------------------------------------------------
gain <- wording_summary |>
  dplyr::mutate(gain = final_auc - generic_auc) |>
  dplyr::arrange(dplyr::desc(gain))
generic_only <- dplyr::filter(wording_summary, candidates == 1)
generic_kept <- dplyr::filter(wording_summary, candidates > 1, final == 1)
clef_textiles <- pick(saved$wording, "clef", "textiles")
held_back <- dplyr::filter(
  clef_textiles, !kept, auc == max(clef_textiles$auc[!clef_textiles$kept])
)
kept_one <- dplyr::filter(clef_textiles, kept) |> dplyr::slice_tail(n = 1)

## ----textiles-wording, echo = FALSE---------------------------------------------------------------
saved$chosen |>
  dplyr::filter(dataset == "textiles") |>
  dplyr::select(model, normal, defective) |>
  show()

## ----grid-setups, eval = FALSE, script = TRUE-----------------------------------------------------
# score_grid <- function(model, dataset, normal, defective, k, max_size) {
#   setup <- make_setup(
#     model, dataset, c(normal = normal, defective = defective), k, max_size
#   )
#   learning <- dplyr::filter(by_dataset[[dataset]], role == "learning")
#   score_rows(setup, learning, state_file("grid", model, dataset, k, max_size))
# }
# 
# grid <- final_wording |>
#   dplyr::cross_join(tibble::tibble(
#     k = rep(c(1, 2, 4), each = 2), max_size = rep(c(512, 768), times = 3)
#   ))

## ----grid-score, eval = FALSE, script = TRUE------------------------------------------------------
# grid <- grid |>
#   dplyr::mutate(result = purrr::pmap(
#     list(model, dataset, normal, defective, k, max_size), score_grid
#   ))

## ----grid-choose, eval = FALSE, script = TRUE-----------------------------------------------------
# grid <- dplyr::bind_cols(
#   dplyr::select(grid, -result),
#   purrr::list_rbind(grid$result) |>
#     dplyr::select(-metric) |>
#     dplyr::rename(auc = estimate)
# )
# 
# chosen <- grid |>
#   dplyr::arrange(dataset, dplyr::desc(round(auc, 10)), k, max_size) |>
#   dplyr::slice_head(n = 1, by = dataset)

## ----grid-table, echo = FALSE---------------------------------------------------------------------
saved$grid |>
  dplyr::summarise(
    `k 1, 512` = auc[k == 1 & max_size == 512],
    `k 1, 768` = auc[k == 1 & max_size == 768],
    `k 2, 512` = auc[k == 2 & max_size == 512],
    `k 2, 768` = auc[k == 2 & max_size == 768],
    `k 4, 512` = auc[k == 4 & max_size == 512],
    `k 4, 768` = auc[k == 4 & max_size == 768],
    .by = c(model, dataset)
  ) |>
  show()

## ----chosen-table, echo = FALSE-------------------------------------------------------------------
saved$chosen |>
  dplyr::select(model, dataset, k, max_size, auc) |>
  show()

## ----grid-facts, include = FALSE------------------------------------------------------------------
chosen_k <- \(model_name, dataset_name) pick(saved$chosen, model_name, dataset_name)$k
token_ratio <- saved$grid |>
  dplyr::summarise(
    ratio = input_tokens[max_size == 512] / input_tokens[max_size == 768],
    .by = c(model, dataset, k)
  )
perfect <- dplyr::filter(saved$grid, round(auc, 10) == 1)

## ----calibration-setups, eval = FALSE, script = TRUE----------------------------------------------
# calibration_rows <- function(dataset, k) {
#   rows <- by_dataset[[dataset]]
#   if (dataset == "pan_images") {
#     dplyr::filter(rows, role == "learning" | (role == "reference" & rank > k))
#   } else {
#     dplyr::filter(rows, role == "calibration")
#   }
# }
# 
# score_calibration <- function(model, dataset, normal, defective, k, max_size) {
#   setup <- make_setup(
#     model, dataset, c(normal = normal, defective = defective), k, max_size
#   )
#   rows <- calibration_rows(dataset, k)
#   score_rows(setup, rows, state_file("calibration", model, dataset, k, max_size))
# }
# 
# calibration <- dplyr::select(chosen, model, dataset, normal, defective, k, max_size)

## ----calibration-score, eval = FALSE, script = TRUE-----------------------------------------------
# calibration <- calibration |>
#   dplyr::mutate(result = purrr::pmap(
#     list(model, dataset, normal, defective, k, max_size), score_calibration
#   ))

## ----platt-facts, include = FALSE-----------------------------------------------------------------
logistic_warned <- dplyr::filter(saved$platt, logistic_warning)
logistic_saturated <- dplyr::filter(saved$platt, logistic_saturated)
stopifnot(all(paste(logistic_warned$model, logistic_warned$dataset) %in%
  paste(logistic_saturated$model, logistic_saturated$dataset)))
sci <- \(x) formatC(x, format = "e", digits = 1)
separated <- dplyr::filter(calibration_wide, round(auc, 10) == 1)
worse <- dplyr::filter(
  calibration_wide, brier_after > brier_before | log_loss_after > log_loss_before
)
steep <- dplyr::filter(saved$platt, lowest < 1e-12 | 1 - highest < 1e-12)
steepest <- dplyr::slice_max(saved$platt, slope, n = 1)
near_bounds <- sum(saved$platt$near_bounds)
auc_same <- isTRUE(all.equal(calibration_wide$auc, calibration_wide$auc_after))
auc_changed <- dplyr::filter(calibration_wide, abs(auc - auc_after) > 1e-12)
grid_auc <- \(model_name, dataset_name) pick(saved$chosen, model_name, dataset_name)$auc
calibration_auc <- \(model_name, dataset_name) pick(calibration_wide, model_name, dataset_name)$auc

## ----calibration-fit, eval = FALSE, script = TRUE-------------------------------------------------
# clip <- function(prob) pmin(pmax(prob, 0.005), 0.995)
# 
# platt_fit <- function(prob, truth) {
#   n_defective <- sum(truth == "defective")
#   n_normal <- sum(truth == "normal")
#   target <- dplyr::if_else(
#     truth == "defective", (n_defective + 1) / (n_defective + 2), 1 / (n_normal + 2)
#   )
#   stats::glm(target ~ stats::qlogis(clip(prob)), family = stats::quasibinomial())
# }
# 
# platt_apply <- function(prob, intercept, slope) {
#   stats::plogis(intercept + slope * stats::qlogis(clip(prob)))
# }

## ----calibration-platt, eval = FALSE, script = TRUE-----------------------------------------------
# platt_row <- function(model, dataset, k, result, ...) {
#   truth <- calibration_rows(dataset, k)$truth
#   fit <- platt_fit(result$answers[[1]]$.pred_defective, truth)
#   tibble::tibble(
#     model = model, dataset = dataset,
#     n_normal = sum(truth == "normal"), n_defective = sum(truth == "defective"),
#     intercept = stats::coef(fit)[[1]], slope = stats::coef(fit)[[2]]
#   )
# }
# 
# platt <- calibration |>
#   purrr::pmap(platt_row) |>
#   purrr::list_rbind()

## ----calibration-write, eval = FALSE, script = TRUE-----------------------------------------------
# calibration |>
#   dplyr::select(model, dataset, normal, defective, k, max_size) |>
#   purrr::pwalk(\(model, dataset, normal, defective, k, max_size) {
#     setup <- make_setup(
#       model, dataset, c(normal = normal, defective = defective), k, max_size
#     )
#     write_setup(setup, file.path(setup_dir, paste0(model, "_", dataset, ".json")))
#   })

## ----platt-table, echo = FALSE--------------------------------------------------------------------
saved$platt |>
  dplyr::mutate(
    min_fitted = formatC(min_fitted, format = "g", digits = 3),
    `1 - max_fitted` = formatC(1 - max_fitted, format = "g", digits = 3)
  ) |>
  dplyr::select(
    model, dataset, n_normal, n_defective, intercept, slope, min_fitted,
    `1 - max_fitted`
  ) |>
  show()

## ----calibration-metrics, eval = FALSE, script = TRUE---------------------------------------------
# calibration_metrics <- function(model, dataset, k, result, intercept, slope,
#                                 ...) {
#   answers <- dplyr::mutate(
#     result$answers[[1]], truth = calibration_rows(dataset, k)$truth
#   )
#   calibrated <- dplyr::mutate(
#     answers, .pred_defective = platt_apply(.pred_defective, intercept, slope)
#   )
#   metrics <- yardstick::metric_set(
#     yardstick::roc_auc, yardstick::brier_class, yardstick::mn_log_loss
#   )
#   dplyr::bind_rows(
#     before = metrics(answers, truth, .pred_defective, event_level = "second"),
#     after = metrics(calibrated, truth, .pred_defective, event_level = "second"),
#     .id = "probabilities"
#   ) |>
#     dplyr::mutate(model = model, dataset = dataset, n = nrow(answers), .before = 1)
# }
# 
# calibration_summary <- calibration |>
#   dplyr::left_join(platt, by = c("model", "dataset")) |>
#   purrr::pmap(calibration_metrics) |>
#   purrr::list_rbind()

## ----calibration-table, echo = FALSE--------------------------------------------------------------
calibration_wide |>
  dplyr::select(-auc_after) |>
  show()

## ----folds, eval = FALSE, script = TRUE-----------------------------------------------------------
# fold_auc <- function(model, dataset, answers) {
#   learning <- dplyr::filter(by_dataset[[dataset]], role == "learning")
#   scored <- dplyr::mutate(answers, truth = learning$truth)
#   set.seed(2026)
#   rsample::vfold_cv(scored, v = 5, repeats = 10, strata = truth) |>
#     dplyr::mutate(
#       model = model, dataset = dataset,
#       auc = purrr::map_dbl(splits, \(split) {
#         held_out <- rsample::assessment(split)
#         yardstick::roc_auc_vec(
#           held_out$truth, held_out$.pred_defective, event_level = "second"
#         )
#       })
#     ) |>
#     dplyr::select(model, dataset, id, id2, auc)
# }
# 
# folds <- chosen |>
#   dplyr::select(model, dataset, answers) |>
#   purrr::pmap(fold_auc) |>
#   purrr::list_rbind()

## ----folds-table, echo = FALSE--------------------------------------------------------------------
fold_summary |>
  dplyr::left_join(
    dplyr::select(saved$chosen, model, dataset, grid_auc = auc),
    by = c("model", "dataset")
  ) |>
  show()

## ----fold-facts, include = FALSE------------------------------------------------------------------
extrusion_folds <- dplyr::filter(saved$folds, dataset == "extrusion")
extrusion_sd <- mean(dplyr::filter(fold_summary, dataset == "extrusion")$sd)
other_sd <- max(dplyr::filter(fold_summary, dataset %in% c("microstructure", "textiles"))$sd)
pans_ones <- mean(dplyr::filter(saved$folds, dataset == "pan_images")$auc == 1)
staple_ones <- mean(dplyr::filter(saved$folds, dataset == "pipe_staple")$auc == 1)

## ----test-facts, include = FALSE------------------------------------------------------------------
test_dates <- saved$run_dates |>
  dplyr::filter(step == "test") |>
  dplyr::reframe(date = c(first, last)) |>
  dplyr::pull(date) |>
  unique() |>
  sort() |>
  format()
test_metric <- function(kind, name) {
  dplyr::filter(saved$test$metrics, probabilities == kind, metric == name)$estimate
}
test_table <- saved$test$metrics |>
  dplyr::distinct(model, dataset, n) |>
  dplyr::mutate(
    auc = test_metric("raw", "roc_auc"),
    raw_accuracy = test_metric("raw", "accuracy"),
    accuracy = test_metric("calibrated", "accuracy"),
    sensitivity = test_metric("calibrated", "sensitivity"),
    specificity = test_metric("calibrated", "specificity"),
    brier = test_metric("calibrated", "brier_class"),
    log_loss = test_metric("calibrated", "mn_log_loss")
  )
raw_table <- saved$test$metrics |>
  dplyr::distinct(model, dataset) |>
  dplyr::mutate(
    accuracy = test_metric("raw", "accuracy"),
    sensitivity = test_metric("raw", "sensitivity"),
    specificity = test_metric("raw", "specificity"),
    brier = test_metric("raw", "brier_class"),
    log_loss = test_metric("raw", "mn_log_loss")
  )
test_images_per_model <- unique(saved$test$calls$images)
test_rows_per_model <- unique(dplyr::summarise(saved$test$runs, n = sum(n), .by = model)$n)
stopifnot(length(test_images_per_model) == 1, length(test_rows_per_model) == 1)
auc_range <- \(datasets) range(test_table$auc[test_table$dataset %in% datasets])
lowest_raw <- dplyr::slice_min(raw_table, accuracy, n = 1)
lowest_max_prob <- saved$test$answers |>
  dplyr::filter(model == lowest_raw$model, dataset == lowest_raw$dataset) |>
  dplyr::pull(prob) |>
  max()
stopifnot(lowest_max_prob < 0.5)
# Where Platt scaling helps and where it hurts, with a check of each claim
platt_effect <- dplyr::inner_join(
  dplyr::select(test_table, model, dataset, accuracy, brier, log_loss),
  dplyr::select(raw_table, model, dataset, accuracy, brier, log_loss),
  by = c("model", "dataset"), suffix = c("_platt", "_raw")
) |>
  dplyr::mutate(
    accuracy_change = round(accuracy_platt - accuracy_raw, 10),
    scores_better = brier_platt < brier_raw & log_loss_platt < log_loss_raw,
    scores_worse = brier_platt > brier_raw & log_loss_platt > log_loss_raw
  )
accuracy_up <- dplyr::filter(platt_effect, accuracy_change > 0)
accuracy_same <- dplyr::filter(platt_effect, accuracy_change == 0)
platt_hurts <- dplyr::filter(platt_effect, accuracy_change < 0)
stopifnot(
  # Each setup has both scores lower, or both scores higher
  all(platt_effect$scores_better | platt_effect$scores_worse),
  # The setups with a lower accuracy are the setups with higher scores
  setequal(
    paste(platt_hurts$model, platt_hurts$dataset),
    with(dplyr::filter(platt_effect, scores_worse), paste(model, dataset))
  ),
  # The text names them and explains them with the fits of the pans
  nrow(platt_hurts) > 0, all(platt_hurts$dataset == "pan_images"),
  nrow(accuracy_same) > 0
)
from_to <- function(table, column) {
  knitr::combine_words(paste(
    model_label[table$model], fmt(table[[paste0(column, "_raw")]], 3), "to",
    fmt(table[[paste0(column, "_platt")]], 3)
  ))
}
pan_fit_rows <- unique(with(dplyr::filter(saved$platt, dataset == "pan_images"), n_normal + n_defective))
test_n <- \(dataset_name) unique(test_table$n[test_table$dataset == dataset_name])

## ----test-score, eval = FALSE, script = TRUE------------------------------------------------------
# test_file <- function(model, dataset) {
#   file.path(test_dir, paste0(model, "_", dataset, ".rds"))
# }
# 
# score_test <- function(model, dataset) {
#   path <- test_file(model, dataset)
#   if (file.exists(path)) {
#     # The test rows were scored: read the result. No request goes out.
#     return(dplyr::mutate(readRDS(path), calls = 0))
#   }
#   setup <- read_setup(file.path(setup_dir, paste0(model, "_", dataset, ".json")))
#   test <- dplyr::filter(by_dataset[[dataset]], role == "test")
#   evaluate_once(
#     setup, image = test$path, truth = test$truth, event = "defective",
#     path = path
#   )
# }

## ----test-run, eval = FALSE, script = TRUE--------------------------------------------------------
# test <- chosen |>
#   dplyr::select(model, dataset) |>
#   dplyr::mutate(result = purrr::map2(model, dataset, score_test))

## ----test-metrics, eval = FALSE, script = TRUE----------------------------------------------------
# at_half <- function(prob) {
#   factor(
#     dplyr::if_else(prob >= 0.5, "defective", "normal"),
#     levels = c("normal", "defective")
#   )
# }
# 
# test_metrics <- function(model, dataset, result, intercept, slope, ...) {
#   truth <- dplyr::filter(by_dataset[[dataset]], role == "test")$truth
#   metrics <- yardstick::metric_set(
#     yardstick::roc_auc, yardstick::accuracy, yardstick::sensitivity,
#     yardstick::specificity, yardstick::brier_class, yardstick::mn_log_loss
#   )
#   score <- function(prob) {
#     scored <- tibble::tibble(truth = truth, prob = prob, predicted = at_half(prob))
#     metrics(scored, truth, prob, estimate = predicted, event_level = "second")
#   }
#   raw <- result$answers[[1]]$.pred_defective
#   dplyr::bind_rows(
#     raw = score(raw),
#     calibrated = score(platt_apply(raw, intercept, slope)),
#     .id = "probabilities"
#   ) |>
#     dplyr::mutate(model = model, dataset = dataset, n = length(truth), .before = 1)
# }
# 
# test_summary <- test |>
#   dplyr::left_join(platt, by = c("model", "dataset")) |>
#   purrr::pmap(test_metrics) |>
#   purrr::list_rbind()

## ----test-table, echo = FALSE---------------------------------------------------------------------
show(test_table)

## ----test-raw-table, echo = FALSE-----------------------------------------------------------------
show(raw_table)

## ----test-cost-table, echo = FALSE----------------------------------------------------------------
show(saved$test$calls)

## ----cost-table, echo = FALSE---------------------------------------------------------------------
show(cost_summary)

## ----calls-table, echo = FALSE--------------------------------------------------------------------
show(saved$calls)

## ----cost-facts, include = FALSE------------------------------------------------------------------
per_image <- \(model_name, column) cost_summary[[column]][cost_summary$model == model_name]
token_factor <- per_image("jev", "tokens_per_image") /
  mean(c(per_image("openai", "tokens_per_image"), per_image("clef", "tokens_per_image")))

## ----reset, include = FALSE---------------------------------------------------
options(old_options)

