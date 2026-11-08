## ----setup, include = FALSE-----------------------------------------------------------------------
# Without the suggested packages, the code is shown but not run
knitr::opts_chunk$set(
  collapse = TRUE, comment = "#>",
  eval = rlang::is_installed(c("dplyr", "magick", "yardstick"))
)
old_options <- options(width = 100, pillar.sigfig = 3)

## ----library--------------------------------------------------------------------------------------
library(decisions)

## ----data, script = TRUE--------------------------------------------------------------------------
textile_dir <- system.file("extdata", "textiles", package = "decisions")

textiles <- textiles |>
  dplyr::mutate(path = file.path(textile_dir, file))

dplyr::count(textiles, role, class)

## ----saved, echo = FALSE--------------------------------------------------------------------------
saved <- readRDS(system.file("extdata", "images.rds", package = "decisions"))
stopifnot(identical(dplyr::filter(textiles, role == "input")$file, saved$inputs$file))

## ----montage, script = TRUE, fig.alt = "Four textile images in two rows. The top row shows two regular weaves. The bottom row shows two weaves with a flaw.", out.width = "50%"----
reference_files <- textiles |>
  dplyr::filter(role == "reference") |>
  dplyr::arrange(class, reference_rank)

reference_files$path |>
  magick::image_read() |>
  magick::image_montage(tile = "2x2", geometry = "250x250+4+4")

## ----references, script = TRUE--------------------------------------------------------------------
references <- split(reference_files$path, reference_files$class)

inputs <- dplyr::filter(textiles, role == "input")

## ----question, script = TRUE----------------------------------------------------------------------
weave <- question_choice(
  "Does the woven pattern in `inspection_image` have a flaw?",
  c(
    nominal = "The weave is regular. The pattern repeats with no break.",
    defective = "The weave has a flaw. A part of the pattern is broken, or the whole pattern is distorted."
  )
)

## ----trim, script = TRUE--------------------------------------------------------------------------
trim_border <- function(image) {
  info <- magick::image_info(image)
  c(12, 12, info$width - 24, info$height - 24)
}

## ----decide, eval = FALSE, script = TRUE----------------------------------------------------------
# deciders <- list(jev = decider_typesafe(), openai = decider_openai())
# 
# answers <- purrr::map(deciders, \(decider) {
#   decide(
#     decider, weave,
#     image = inputs$path, references = references,
#     max_size = 768, upscale = TRUE
#   )
# })

## ----show-decide, echo = FALSE--------------------------------------------------------------------
answers <- saved$answers

## ----clef, eval = FALSE, script = TRUE------------------------------------------------------------
# clef <- decider_systemone(
#   "http://localhost:30000", "Cloudflare/clef-flash",
#   image_format = "images"
# )
# 
# settings <- list(
#   `max_size 768` = list(max_size = 768),
#   `max_size 512` = list(max_size = 512),
#   `crop trim` = list(max_size = 768, crop = trim_border)
# )
# 
# clef_answers <- purrr::map(settings, \(setting) {
#   rlang::exec(
#     decide, clef, weave,
#     image = inputs$path, references = references, upscale = TRUE,
#     !!!setting
#   )
# })

## ----show-clef, echo = FALSE----------------------------------------------------------------------
clef_answers <- saved$clef_answers

## ----results, script = TRUE-----------------------------------------------------------------------
all_answers <- c(answers, clef_answers)

results <- all_answers |>
  dplyr::bind_rows(.id = "model") |>
  dplyr::mutate(
    model = factor(model, levels = names(all_answers)),
    truth = rep(inputs$class, times = length(all_answers))
  )

## ----performance, script = TRUE-------------------------------------------------------------------
results |>
  dplyr::group_by(model) |>
  dplyr::summarise(
    roc_auc = yardstick::roc_auc_vec(truth, .pred_defective, event_level = "second"),
    errors = sum(!is.na(.error)),
    input_tokens = sum(.input_tokens, na.rm = TRUE),
    cost_usd = if (all(is.na(.cost))) NA_real_ else sum(.cost, na.rm = TRUE),
    median_seconds = median(.seconds, na.rm = TRUE)
  )

## ----reset, include = FALSE---------------------------------------------------
options(old_options)

