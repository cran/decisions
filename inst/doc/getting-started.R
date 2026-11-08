## ----setup, include = FALSE-----------------------------------------------------------------------
# Without the suggested packages, the code is shown but not run
knitr::opts_chunk$set(
  collapse = TRUE, comment = "#>",
  eval = rlang::is_installed(c("dplyr", "yardstick"))
)
old_options <- options(width = 100, pillar.sigfig = 3)

## ----library--------------------------------------------------------------------------------------
library(decisions)

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

## ----decider, script = TRUE-----------------------------------------------------------------------
jev <- decider_typesafe()
jev

## ----credentials, eval = FALSE--------------------------------------------------------------------
# jev <- decider_typesafe(credentials = \() Sys.getenv("MY_JEV_KEY"))

## ----questions, script = TRUE---------------------------------------------------------------------
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

## ----one-noul, eval = FALSE, script = TRUE--------------------------------------------------------
# one_noul <- decide(jev, refund, text = tickets$text[1])
# one_noul

## ----saved, echo = FALSE--------------------------------------------------------------------------
saved <- readRDS(system.file("extdata", "getting-started.rds", package = "decisions"))

## ----show-one-noul, echo = FALSE------------------------------------------------------------------
saved$one_noul

## ----one-choice, eval = FALSE, script = TRUE------------------------------------------------------
# one_choice <- decide(jev, topic, text = tickets$text[2])
# one_choice

## ----show-one-choice, echo = FALSE----------------------------------------------------------------
saved$one_choice

## ----one-score, eval = FALSE, script = TRUE-------------------------------------------------------
# one_score <- decide(jev, urgency, text = tickets$text[3])
# one_score

## ----show-one-score, echo = FALSE-----------------------------------------------------------------
saved$one_score

## ----many-noul, eval = FALSE, script = TRUE-------------------------------------------------------
# many_noul <- decide(jev, refund, text = tickets$text)
# dplyr::bind_cols(tickets["ticket"], many_noul)

## ----show-many-noul, echo = FALSE-----------------------------------------------------------------
many_noul <- saved$many_noul
dplyr::bind_cols(tickets["ticket"], many_noul)

## ----many-two, eval = FALSE, script = TRUE--------------------------------------------------------
# many_two <- decide(jev, list(topic = topic, urgency = urgency), text = tickets$text)
# names(many_two)

## ----show-many-two, echo = FALSE------------------------------------------------------------------
many_two <- saved$many_two
names(many_two)

## ----many-two-table, script = TRUE----------------------------------------------------------------
dplyr::bind_cols(
  tickets["ticket"],
  many_two[c("topic_.pred_class", "urgency_.pred_class", "urgency_.score")]
)

## ----recalls, script = TRUE-----------------------------------------------------------------------
set.seed(2026)
recalls <- ohio_recalls |>
  dplyr::slice_sample(n = 10, by = classification) |>
  dplyr::mutate(
    text = paste0("Product: ", product_description, "\nReason: ", reason_for_recall)
  )

## ----recall-question, script = TRUE---------------------------------------------------------------
risk <- question_score(
  "How serious are the health consequences of using this recalled food?",
  c(
    `Class III` = "Using the food is not likely to cause adverse health consequences.",
    `Class II` = "Using the food may cause temporary or reversible health problems, or serious harm is remote.",
    `Class I` = "Using the food is likely to cause serious health problems or death."
  )
)

## ----recall-decide, eval = FALSE, script = TRUE---------------------------------------------------
# recall_out <- decide(jev, risk, text = recalls$text)
# recall_out <- dplyr::bind_cols(recalls["classification"], recall_out)

## ----recall-saved, echo = FALSE-------------------------------------------------------------------
saved_recalls <- readRDS(system.file("extdata", "getting-started-recalls.rds", package = "decisions"))
recall_out <- saved_recalls$recall_out

## ----recall-metrics, script = TRUE----------------------------------------------------------------
table(FDA = recall_out$classification, Jev = recall_out$.pred_class)
yardstick::accuracy(recall_out, classification, .pred_class)
yardstick::kap(recall_out, classification, .pred_class, weighting = "quadratic")

## ----recall-score, script = TRUE------------------------------------------------------------------
recall_out |>
  dplyr::slice_head(n = 2, by = classification) |>
  dplyr::select(classification, .pred_class, .score)

## ----path, eval = FALSE---------------------------------------------------------------------------
# decide(jev, refund, text = tickets$text, path = "refund-answers.json")

## ----skill, eval = FALSE--------------------------------------------------------------------------
# skill <- system.file("skills", "decisions", package = "decisions")
# 
# # Claude Code
# dir.create(".claude/skills", recursive = TRUE, showWarnings = FALSE)
# file.copy(skill, ".claude/skills", recursive = TRUE)
# 
# # Other agents
# dir.create(".agents/skills", recursive = TRUE, showWarnings = FALSE)
# file.copy(skill, ".agents/skills", recursive = TRUE)

## ----reset, include = FALSE---------------------------------------------------
options(old_options)

