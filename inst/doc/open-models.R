## ----setup, include = FALSE-----------------------------------------------------------------------
# Without the suggested packages, the code is shown but not run
knitr::opts_chunk$set(
  collapse = TRUE, comment = "#>", eval = rlang::is_installed("dplyr")
)
old_options <- options(width = 100, pillar.sigfig = 3)

## ----library--------------------------------------------------------------------------------------
library(decisions)

## ----decider, script = TRUE-----------------------------------------------------------------------
laya <- decider_systemone("http://localhost:8000", "english", input = "text")
laya

## ----tickets, script = TRUE-----------------------------------------------------------------------
tickets <- tibble::tibble(
  ticket = 1:10,
  text = c(
    "I was charged twice for order 4471. Please refund the extra charge today.",
    "My parcel has not arrived. The tracking page has said 'in transit' for two weeks.",
    "The app crashes each time I open the settings page. I cannot log in, and I have a meeting in ten minutes.",
    "Thank you for the quick delivery. The product works well.",
    "I want my money back. The blender broke after one day.",
    "How do I change the email address on my account?",
    "The invoice shows the wrong company name. Please send a new one when you can.",
    "The package arrived with a broken seal. Please send a replacement. There is no hurry.",
    "Your website shows an error when I pay by card. I must order before the sale ends tonight.",
    "Can you tell me which sizes the jacket comes in?"
  )
)

refund <- question_noul("Does the customer ask for a refund?")

topic <- question_choice(
  "Which team should answer this ticket?",
  c(
    billing = "Charges, invoices and refunds.",
    shipping = "Parcels and delivery.",
    technical = "Errors, and how to use the product."
  )
)

urgency <- question_score(
  "How urgent is this ticket?",
  c(
    low = "The customer can wait several days.",
    medium = "The customer needs an answer today.",
    high = "The customer needs an answer now."
  )
)

## ----decide, eval = FALSE, script = TRUE----------------------------------------------------------
# answers <- decide(
#   laya,
#   list(refund = refund, topic = topic, urgency = urgency),
#   text = tickets$text,
#   max_active = 1
# )

## ----saved, echo = FALSE--------------------------------------------------------------------------
saved <- readRDS(system.file("extdata", "open-models.rds", package = "decisions"))
answers <- saved$answers

## ----answers, script = TRUE-----------------------------------------------------------------------
dplyr::bind_cols(
  tickets["ticket"],
  answers[c("refund_.pred_class", "topic_.pred_class", "urgency_.pred_class", ".seconds")]
)

## ----seconds, script = TRUE-----------------------------------------------------------------------
dplyr::summarise(
  answers,
  rows = dplyr::n(),
  errors = sum(!is.na(.error)),
  input_tokens = sum(.input_tokens),
  median_seconds = median(.seconds),
  max_seconds = max(.seconds)
)

## ----reset, include = FALSE---------------------------------------------------
options(old_options)

