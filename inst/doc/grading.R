## ----setup, include = FALSE-----------------------------------------------------------------------
# Without the suggested packages, the code is shown but not run
knitr::opts_chunk$set(
  collapse = TRUE, comment = "#>",
  eval = rlang::is_installed(c("dplyr", "yardstick"))
)
old_options <- options(width = 100, pillar.sigfig = 3)

## ----library--------------------------------------------------------------------------------------
library(decisions)

## ----rubric, script = TRUE------------------------------------------------------------------------
rubric <- list(
  keeps_row = question_noul("Does the text say that the failed input keeps its row in the output?"),
  na_answers = question_noul("Does the text say that the answers of the failed row are NA?"),
  error_text = question_noul("Does the text say that the reason of the failure is in the .error column?"),
  others_continue = question_noul("Does the text say that the other inputs of the batch still get their answers?"),
  stops_all = question_noul("Does the text say that decide() stops with an error, or returns nothing, when one call fails?"),
  drops_row = question_noul("Does the text say that decide() removes the failed input from the output?")
)

## ----rubric-text, script = TRUE-------------------------------------------------------------------
rubric_text <- tibble::tribble(
  ~question, ~kind, ~if_above, ~if_below,
  "keeps_row", "idea", "Covered: the failed input keeps its row.", "Missing: say what happens to the row of the failed input.",
  "na_answers", "idea", "Covered: the answers of the failed row are NA.", "Missing: say what the answers of the failed row are.",
  "error_text", "idea", "Covered: the reason is in the .error column.", "Missing: say where to find the reason of the failure.",
  "others_continue", "idea", "Covered: the other inputs still get their answers.", "Missing: say what happens to the other inputs.",
  "stops_all", "wrong", "Check again: one failed call does not stop decide().", NA_character_,
  "drops_row", "wrong", "Check again: decide() does not remove the row of a failed call.", NA_character_
)

## ----samples, script = TRUE-----------------------------------------------------------------------
samples <- rubric_answers
samples[1:3, ]

## ----setup-object, script = TRUE------------------------------------------------------------------
setup <- decision_setup(decider_typesafe(), rubric)

## ----score, eval = FALSE, script = TRUE-----------------------------------------------------------
# scored <- decide(setup, text = samples$answer)

## ----saved, echo = FALSE--------------------------------------------------------------------------
saved <- readRDS(system.file("extdata", "grading.rds", package = "decisions"))
scored <- saved$scored

## ----probabilities, script = TRUE-----------------------------------------------------------------
dplyr::select(scored, dplyr::ends_with("_.pred_true"), .error)

## ----thresholds, script = TRUE--------------------------------------------------------------------
thresholds <- purrr::map(names(rubric), \(question) {
  tibble::tibble(
    truth = factor(samples[[question]], levels = c(TRUE, FALSE)),
    probability = scored[[paste0(question, "_.pred_true")]]
  ) |>
    yardstick::roc_curve(truth, probability) |>
    dplyr::mutate(j = sensitivity + specificity - 1) |>
    dplyr::filter(dplyr::near(j, max(j)), is.finite(.threshold)) |>
    dplyr::summarise(threshold = median(.threshold), j = max(j))
}) |>
  purrr::set_names(names(rubric)) |>
  dplyr::bind_rows(.id = "question")

thresholds

## ----feedback-rubric, script = TRUE---------------------------------------------------------------
feedback_rubric <- rubric_text |>
  dplyr::left_join(thresholds[c("question", "threshold")], by = "question")

feedback_rubric

## ----give-feedback, script = TRUE-----------------------------------------------------------------
give_feedback <- function(scored, rubric) {
  if (!is.na(scored$.error)) {
    return("The check did not work. Try again.")
  }
  probability <- purrr::map_dbl(rubric$question, \(question) scored[[paste0(question, "_.pred_true")]])
  lines <- ifelse(probability >= rubric$threshold, rubric$if_above, rubric$if_below)
  lines[!is.na(lines)]
}

## ----new, eval = FALSE, script = TRUE-------------------------------------------------------------
# new_answers <- c(
#   "The row of the failed call stays in the output. Its answers are NA, and .error has the reason.",
#   "decide() stops with an error at the first failed call, and you get nothing back.",
#   "It keeps going with the other inputs."
# )
# 
# new_scored <- decide(setup, text = new_answers)
# 
# purrr::map(seq_along(new_answers), \(i) give_feedback(new_scored[i, ], feedback_rubric))

## ----show-new, echo = FALSE-----------------------------------------------------------------------
new_scored <- saved$new_scored
new_answers <- saved$new_answers
purrr::map(seq_along(new_answers), \(i) give_feedback(new_scored[i, ], feedback_rubric))

## ----write-setup, eval = FALSE, script = TRUE-----------------------------------------------------
# write_setup(setup, "rubric_setup.json")

## ----save-rubric, eval = FALSE--------------------------------------------------------------------
# saveRDS(feedback_rubric, "rubric.rds")

## ----app, eval = FALSE----------------------------------------------------------------------------
# library(shiny)
# library(decisions)
# 
# rubric <- readRDS("rubric.rds")
# 
# ui <- fluidPage(
#   titlePanel("Check your answer"),
#   p("What does decide() do when one call of a batch fails?"),
#   textAreaInput("answer", "Your answer", width = "100%", rows = 6),
#   actionButton("check", "Check"),
#   textOutput("provider"),
#   uiOutput("feedback")
# )
# 
# server <- function(input, output, session) {
#   # Replace the function with the key source of your host
#   setup <- read_setup("rubric_setup.json", credentials = \() Sys.getenv("TYPESAFE_API_KEY"))
# 
#   output$provider <- renderText(
#     paste0("Your answer goes to ", setup@decider@provider, " (", setup@decider@base_url, ").")
#   )
# 
#   output$feedback <- renderUI({
#     req(nzchar(input$answer))
#     scored <- decide(setup, text = input$answer)
#     tags$ul(lapply(give_feedback(scored, rubric), tags$li))
#   }) |>
#     bindEvent(input$check)
# }
# 
# shinyApp(ui, server)

## ----reset, include = FALSE---------------------------------------------------
options(old_options)

