# analysis_gender_topic.R
#
# Mixed effects model: does speech duration differ by gender × topic?
# Outcome:   log(duration_sec)
# Key terms: gender * topics (interaction)
# Controls:  parl_group, period_int, debate_type, log(speech_limit)
# Random:    (1 | pad_intern)  — absorbs individual MP speaking style
#
# Reference gender: female  →  positive gendermale coeff = men speak longer

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

# ── Prepare model data ────────────────────────────────────────────────────────
df_model <- df |>
  filter(!is.na(gender), !map_lgl(topics, is.null), duration_sec > 10) |>
  mutate(
    limit_sec   = suppressWarnings(as.integer(speech_limit) * 60L),
    log_limit   = if_else(!is.na(limit_sec) & limit_sec > 0, log(limit_sec), NA_real_),
    log_dur     = log(duration_sec),
    gender      = factor(gender, levels = c("female", "male")),
    debate_type = factor(debate_type),
    pad_intern  = factor(pad_intern)
  ) |>
  unnest(topics) |>
  mutate(topics = factor(topics)) |>
  filter(!is.na(log_limit))

message(sprintf("Model data: %d rows, %d MPs, %d topics",
                nrow(df_model),
                n_distinct(df_model$pad_intern),
                nlevels(df_model$topics)))

# ── Fit model ─────────────────────────────────────────────────────────────────
fit <- lme(
  log_dur ~ gender * topics + parl_group + period_int + debate_type + log_limit,
  random    = ~ 1 | pad_intern,
  data      = df_model,
  method    = "REML",
  na.action = na.omit,
  control   = lmeControl(opt = "optim", maxIter = 200)
)

# ── Results ───────────────────────────────────────────────────────────────────
tt <- as.data.frame(summary(fit)$tTable)
tt$term <- rownames(tt)

# Overall gender effect (on reference topic)
main <- tt["gendermale", ]
cat(sprintf(
  "\nOverall gender effect (vs female, on reference topic):\n  β = %.4f, SE = %.4f, t = %.2f, p = %.4f\n",
  main$Value, main$`Std.Error`, main$`t-value`, main$`p-value`
))

# Gender × topic interactions
gi <- tt[grep("gendermale:topics", tt$term), ]
gi$topic        <- sub("gendermale:topics", "", gi$term)
gi$total_effect <- main$Value + gi$Value
gi <- gi[order(gi$total_effect), ]

cat("\n--- Gender × Topic: total gender effect per topic (male vs female) ---\n")
cat("(negative = women speak longer on this topic after controls)\n\n")
print(
  gi[, c("topic", "total_effect", "Std.Error", "t-value", "p-value")],
  row.names = FALSE, digits = 3
)

# Highlight significant interactions
sig <- gi[gi$`p-value` < 0.05, ]
if (nrow(sig) > 0) {
  cat("\nSignificant interactions (p < 0.05):\n")
  print(sig[, c("topic", "total_effect", "p-value")], row.names = FALSE, digits = 3)
} else {
  cat("\nNo topic-specific gender interactions reach p < 0.05.\n")
}

# Random effects summary
cat("\nRandom effects (MP-level variance):\n")
print(VarCorr(fit))

saveRDS(fit, file.path(HERE, "fit_gender_topic.rds"))
message("Model saved to fit_gender_topic.rds")
