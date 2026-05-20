# analysis_party_topic.R
#
# Mixed effects model: does speech duration differ by party × topic?
# Outcome:   log(duration_sec)
# Key terms: party * topics (interaction)
# Controls:  gender, period_int, debate_type, log(speech_limit)
# Random:    (1 | pad_intern)  — absorbs individual MP speaking style
#
# Reference party: ÖVP
# Restricted to 6 major parties (≥ 2,000 speeches): ÖVP, SPÖ, FPÖ, GRÜNE, NEOS, BZÖ
# Smaller parties (Stronach, JETZT/Pilz, LIF, FPÖ/BZÖ) excluded to avoid
# empty party × topic cells that cause matrix singularity.

library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(nlme)

HERE <- tryCatch(
  dirname(rstudioapi::getSourceEditorContext()$path),
  error = function(e) getwd()
)

df <- readRDS(file.path(HERE, "df_speech_dataset.rds"))

# ── Party harmonisation ───────────────────────────────────────────────────────
harmonise_party <- function(x) case_when(
  str_detect(x, "Volkspartei")                          ~ "ÖVP",
  str_detect(x, "Sozialdemokratis")                     ~ "SPÖ",
  str_detect(x, "Freiheitlich") & str_detect(x, "BZÖ") ~ "FPÖ/BZÖ",
  str_detect(x, "Freiheitlich")                         ~ "FPÖ",
  str_detect(x, "Grün")                                 ~ "GRÜNE",
  str_detect(x, "NEOS")                                 ~ "NEOS",
  str_detect(x, "BZÖ")                                  ~ "BZÖ",
  TRUE                                                  ~ NA_character_
)

# ── Prepare model data ────────────────────────────────────────────────────────
df_model <- df |>
  filter(!is.na(parl_group), !map_lgl(topics, is.null), duration_sec > 10) |>
  mutate(
    party       = harmonise_party(parl_group),
    limit_sec   = suppressWarnings(as.integer(speech_limit) * 60L),
    log_limit   = if_else(!is.na(limit_sec) & limit_sec > 0, log(limit_sec), NA_real_),
    log_dur     = log(duration_sec),
    debate_type = factor(debate_type),
    pad_intern  = factor(pad_intern)
  ) |>
  filter(!is.na(party), !is.na(log_limit)) |>
  unnest(topics) |>
  mutate(
    party  = factor(party, levels = c("ÖVP", "SPÖ", "FPÖ", "GRÜNE", "NEOS", "BZÖ")),
    topics = factor(topics)
  ) |>
  filter(!is.na(party))

message(sprintf("Model data: %d rows, %d MPs, %d parties, %d topics",
                nrow(df_model),
                n_distinct(df_model$pad_intern),
                nlevels(df_model$party),
                nlevels(df_model$topics)))
print(count(df_model, party))

# ── Fit model ─────────────────────────────────────────────────────────────────
fit <- lme(
  log_dur ~ party * topics + gender + period_int + debate_type + log_limit,
  random    = ~ 1 | pad_intern,
  data      = df_model,
  method    = "REML",
  na.action = na.omit,
  control   = lmeControl(opt = "optim", maxIter = 300)
)

# ── Results ───────────────────────────────────────────────────────────────────
tt <- as.data.frame(summary(fit)$tTable)
tt$term <- rownames(tt)

# Main party effects (vs ÖVP)
main_party <- tt[grepl("^party[A-Z]", tt$term) & !grepl(":", tt$term), ]
main_party$party <- str_remove(main_party$term, "party")
main_party <- main_party[order(main_party$Value), ]

cat("\n--- Main party effects vs ÖVP ---\n")
print(
  main_party[, c("party", "Value", "Std.Error", "t-value", "p-value")],
  row.names = FALSE, digits = 3
)

# Party × topic interactions (top 20 by significance)
gi <- tt[grep("party.*:topics", tt$term), ]
gi$party <- str_extract(gi$term, "party[^:]+") |> str_remove("party")
gi$topic <- str_extract(gi$term, "topics.+")   |> str_remove("topics")
gi <- gi[order(gi$`p-value`), ]

cat("\n--- Party × Topic interactions (top 20 by p-value) ---\n")
print(
  head(gi[, c("party", "topic", "Value", "Std.Error", "t-value", "p-value")], 20),
  row.names = FALSE, digits = 3
)

sig <- gi[gi$`p-value` < 0.05, ]
if (nrow(sig) > 0) {
  cat("\nSignificant party × topic interactions (p < 0.05):\n")
  print(sig[, c("party", "topic", "Value", "p-value")], row.names = FALSE, digits = 3)
} else {
  cat("\nNo party × topic interactions reach p < 0.05.\n")
}

cat("\nRandom effects (MP-level variance):\n")
print(VarCorr(fit))

saveRDS(fit, file.path(HERE, "fit_party_topic.rds"))
message("Model saved to fit_party_topic.rds")
