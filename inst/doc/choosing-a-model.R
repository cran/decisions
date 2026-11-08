## ----setup, include = FALSE-----------------------------------------------------------------------
# Without the suggested packages, the code is shown but not run
knitr::opts_chunk$set(
  collapse = TRUE, comment = "#>",
  eval = rlang::is_installed(c("dplyr", "yardstick"))
)
old_options <- options(width = 100, pillar.sigfig = 3)

## ----library--------------------------------------------------------------------------------------
library(decisions)

## ----injuries, script = TRUE----------------------------------------------------------------------
set.seed(2026)
injuries <- ohio_injuries |>
  dplyr::slice_sample(n = 20, by = collapsed_nature_title)

dplyr::count(injuries, collapsed_nature_title)

## ----saved, echo = FALSE--------------------------------------------------------------------------
saved <- readRDS(system.file("extdata", "choosing-a-model.rds", package = "decisions"))
stopifnot(identical(injuries$id, saved$injuries$id))

## ----question, script = TRUE----------------------------------------------------------------------
nature <- question_choice(
  "Which injury does this report of a work injury describe? Choose the main injury.",
  c(
    amputation = "A body part is cut or torn off, in whole or in part.",
    fracture = "A bone is broken or cracked.",
    `crushing injury` = "A body part is squeezed hard between objects, and the tissue is damaged.",
    `thermal burn` = "Heat, flame, hot liquid or a hot surface burns the skin.",
    concussion = "A blow or a shake to the head hurts the brain."
  )
)

## ----decide, eval = FALSE, script = TRUE----------------------------------------------------------
# deciders <- list(
#   jev = decider_typesafe(),
#   microsoft = decider_openrouter("microsoft/microsoft-decision-1"),
#   openai = decider_openai()
# )
# 
# answers <- purrr::map(deciders, \(decider) decide(decider, nature, text = injuries$final_narrative))

## ----show-decide, echo = FALSE--------------------------------------------------------------------
answers <- saved$answers

## ----results, script = TRUE-----------------------------------------------------------------------
results <- answers |>
  dplyr::bind_rows(.id = "model") |>
  dplyr::mutate(truth = rep(injuries$collapsed_nature_title, times = length(answers)))

## ----columns, script = TRUE-----------------------------------------------------------------------
grep("^\\.pred_", names(results), value = TRUE)

## ----performance, script = TRUE-------------------------------------------------------------------
metrics <- yardstick::metric_set(yardstick::accuracy, yardstick::roc_auc)

results |>
  dplyr::group_by(model) |>
  metrics(
    truth,
    .pred_amputation, .pred_fracture, .pred_crushing_injury, .pred_thermal_burn, .pred_concussion,
    estimate = .pred_class
  ) |>
  dplyr::select(model, .metric, .estimate)

## ----errors, script = TRUE------------------------------------------------------------------------
results |>
  dplyr::group_by(model) |>
  dplyr::summarise(rows = dplyr::n(), errors = sum(!is.na(.error)))

## ----confusion, script = TRUE---------------------------------------------------------------------
results |>
  split(results$model) |>
  purrr::map(\(x) yardstick::conf_mat(x, truth, .pred_class)$table)

## ----cost, script = TRUE--------------------------------------------------------------------------
results |>
  dplyr::group_by(model) |>
  dplyr::summarise(
    input_tokens = sum(.input_tokens, na.rm = TRUE),
    cost_usd = if (all(is.na(.cost))) NA_real_ else sum(.cost, na.rm = TRUE),
    median_seconds = median(.seconds, na.rm = TRUE)
  )

## ----reset, include = FALSE---------------------------------------------------
options(old_options)

