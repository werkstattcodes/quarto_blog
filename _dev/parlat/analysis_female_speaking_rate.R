# analysis_female_speaking_rate.R
#
# For each party × legislative period: compare the female share of MPs
# (representation) with the female share of speeches (participation).
# A ratio > 1 means female MPs speak more per capita than their male
# colleagues; < 1 means they speak less.
#
# Key finding: ÖVP and SPÖ are close to parity; GRÜNE women tend to over-
# participate; FPÖ women consistently under-participate, hitting zero
# in period XXII after the BZÖ split; NEOS started very low and is converging.
#
# Input:   _cache_mps_all.rds   (MP roster with gender per party × period)
#          df_speech_dataset.rds (speech-level data)

library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(ggplot2)
library(scales)
library(RColorBrewer)

HERE <- tryCatch(
  dirname(rstudioapi::getSourceEditorContext()$path),
  error = function(e) getwd()
)

# ── Helpers ───────────────────────────────────────────────────────────────────
to_period_int <- function(x) {
  suppressWarnings(as.integer(x)) |>
    (\(v) if_else(is.na(v), as.integer(as.roman(x)), v))()
}

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

PARTIES <- c("ÖVP", "SPÖ", "FPÖ", "GRÜNE", "NEOS", "BZÖ")

# ── MP roster: n per party × period × gender ──────────────────────────────────
df_mps_all <- readRDS(file.path(HERE, "_cache_mps_all.rds"))

df_roster <- df_mps_all |>
  unnest(mp_details) |>
  ungroup() |>
  mutate(
    period_int = to_period_int(legis_period),
    party      = harmonise_party(parl_group)
  ) |>
  filter(!is.na(party), !is.na(gender), party %in% PARTIES) |>
  distinct(pad_intern, period_int, party, gender) |>
  ungroup() |>
  count(party, period_int, gender, name = "n_mps")

# ── Speech counts: n per party × period × gender ──────────────────────────────
df_speeches <- readRDS(file.path(HERE, "df_speech_dataset.rds"))

df_spk <- df_speeches |>
  filter(!is.na(gender), !is.na(parl_group), duration_sec > 10) |>
  mutate(party = harmonise_party(parl_group)) |>
  filter(!is.na(party), party %in% PARTIES) |>
  count(party, period_int, gender, name = "n_speeches")

# ── Join and compute rates ────────────────────────────────────────────────────
df_rates <- df_roster |>
  left_join(df_spk, by = c("party", "period_int", "gender")) |>
  replace_na(list(n_speeches = 0L)) |>
  mutate(rate = n_speeches / n_mps)

df_ratio <- df_rates |>
  pivot_wider(names_from = gender,
              values_from = c(n_mps, rate, n_speeches)) |>
  filter(!is.na(rate_female), !is.na(rate_male),
         rate_male > 0, n_mps_female >= 3) |>
  mutate(
    activity_ratio      = rate_female / rate_male,
    female_mp_share     = n_mps_female / (n_mps_female + n_mps_male),
    female_speech_share = n_speeches_female / (n_speeches_female + n_speeches_male),
    deviation           = female_speech_share - female_mp_share,
    period_label        = paste0("GP ", as.roman(period_int))
  )

# ── Print summary ─────────────────────────────────────────────────────────────
cat("--- Female MP share vs speech share by party × period ---\n")
print(
  df_ratio |>
    select(party, period_label, female_mp_share, female_speech_share,
           activity_ratio, deviation) |>
    arrange(party, period_int),
  n = 60, digits = 3
)

# ── Plot 1: Scatter — female MP share vs female speech share ──────────────────
party_colors <- c(
  ÖVP   = "#62a8e5", SPÖ  = "#E41A1C", FPÖ  = "#2171b5",
  GRÜNE = "#4daf4a", NEOS = "#fa9fb5", BZÖ  = "#ff7f00"
)

p1 <- ggplot(df_ratio,
             aes(x = female_mp_share, y = female_speech_share,
                 colour = party, label = period_label)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              colour = "grey50", linewidth = 0.5) +
  geom_point(size = 2.5, alpha = 0.85) +
  geom_text(size = 2.2, vjust = -0.7, show.legend = FALSE) +
  scale_x_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0, NA), expand = expansion(mult = c(0.02, 0.05))) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0, NA), expand = expansion(mult = c(0.02, 0.05))) +
  scale_colour_manual(values = party_colors, name = NULL) +
  labs(
    x = "Female share of MPs",
    y = "Female share of plenary speeches",
    title = "Are female MPs speaking at their representation rate?",
    subtitle = "Each point = one party × legislative period; diagonal = parity"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title.position = "plot",
    legend.position     = "bottom"
  )

print(p1)

# ── Plot 2: Activity ratio by party over legislative periods ──────────────────
p2 <- df_ratio |>
  mutate(party = factor(party, levels = PARTIES)) |>
  ggplot(aes(x = period_int, y = activity_ratio, colour = party)) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50", linewidth = 0.5) +
  geom_line(linewidth = 0.8, alpha = 0.8) +
  geom_point(size = 2.5) +
  scale_x_continuous(
    breaks = 20:27,
    labels = paste0("GP ", as.roman(20:27))
  ) +
  scale_y_continuous(labels = label_number(suffix = "×")) +
  scale_colour_manual(values = party_colors, name = NULL) +
  labs(
    x = NULL, y = "Speeches per female MP / speeches per male MP",
    title = "FPÖ women speak the least relative to their numbers; ÖVP women the most",
    subtitle = "Activity ratio = speeches per female MP ÷ speeches per male MP; 1 = parity"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title.position = "plot",
    axis.text.x         = element_text(size = 8),
    legend.position     = "bottom"
  )

print(p2)

cat("\nDone. Two plots printed.\n")
