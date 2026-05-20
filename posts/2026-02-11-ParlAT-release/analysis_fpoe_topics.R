# analysis_fpoe_topics.R
#
# Descriptive analysis of FPÖ's topical evolution across legislative periods XX–XXVII.
# Input:  df_speech_dataset.rds
# Output: four ggplot figures (not saved to disk — inspect interactively)
#
# FPÖ party group names across periods:
#   XX–XXI  : "Klub der Freiheitlichen Partei Österreichs"
#   XXII    : "Freiheitlicher Parlamentsklub - BZÖ" (pre-split April 2005)
#             "Freiheitlicher Parlamentsklub" (post-split)
#   XXIII–XXVII: "Freiheitlicher Parlamentsklub"
# BZÖ breakaway faction ("Parlamentsklub des BZÖ") is excluded throughout.

library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(ggplot2)
library(forcats)
library(RColorBrewer)
library(scales)

HERE <- tryCatch(
  dirname(rstudioapi::getSourceEditorContext()$path),
  error = function(e) getwd()
)

df_speeches <- readRDS(file.path(HERE, "df_speech_dataset.rds"))

# ── Prepare base data ─────────────────────────────────────────────────────────
df_fpoe <- df_speeches |>
  filter(!is.na(parl_group), duration_sec > 10) |>
  mutate(
    is_fpoe = str_detect(parl_group, "Freiheitlich") & !str_detect(parl_group, "BZÖ"),
    year    = as.integer(format(date, "%Y"))
  )

# Government period spans (approximate calendar years)
govt_spans <- tibble(
  xmin  = c(2002.0, 2017.5),
  xmax  = c(2005.3, 2019.5),
  label = c("FPÖ in government\n(ÖVP–FPÖ I)", "FPÖ in government\n(ÖVP–FPÖ II)")
)

# ── Plot 1: Speech volume by year ─────────────────────────────────────────────
p1 <- df_fpoe |>
  filter(is_fpoe, !is.na(year)) |>
  count(year) |>
  ggplot(aes(x = year, y = n)) +
  annotate("rect",
    xmin = govt_spans$xmin, xmax = govt_spans$xmax,
    ymin = -Inf, ymax = Inf,
    fill = "#2171b5", alpha = 0.10
  ) +
  annotate("text",
    x = (govt_spans$xmin + govt_spans$xmax) / 2,
    y = c(2700, 2700),
    label = govt_spans$label,
    size = 3, colour = "#2171b5", lineheight = 0.9
  ) +
  geom_vline(xintercept = 2019.35, linetype = "dashed",
             colour = "#cb181d", linewidth = 0.6) +
  annotate("text", x = 2019.55, y = 2600, label = "Ibiza\nscandal",
           hjust = 0, size = 3, colour = "#cb181d", lineheight = 0.9) +
  geom_col(fill = "#2171b5", alpha = 0.7, width = 0.75) +
  scale_x_continuous(breaks = seq(2002, 2024, 2)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(
    x = NULL, y = "Speeches",
    title = "FPÖ speaks less when in government — opposition is its natural habitat",
    subtitle = "Annual number of FPÖ plenary speeches, National Council, 2002–2024"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title.position = "plot", panel.grid.major.x = element_blank())

print(p1)

# ── Plot 2: Topic composition by legislative period ───────────────────────────
df_fpoe_topics <- df_fpoe |>
  filter(is_fpoe, !map_lgl(topics, is.null), !is.na(period_int)) |>
  unnest(topics) |>
  count(period_int, topics)

top_topics_fpoe <- df_fpoe_topics |>
  summarise(total = sum(n), .by = topics) |>
  slice_max(total, n = 7, with_ties = FALSE) |>
  pull(topics)

df_fpoe_pct <- df_fpoe_topics |>
  mutate(topic_label = if_else(topics %in% top_topics_fpoe, topics, "Other")) |>
  summarise(n = sum(n), .by = c(period_int, topic_label)) |>
  group_by(period_int) |>
  mutate(pct = n / sum(n)) |>
  ungroup()

topic_order_fpoe <- df_fpoe_pct |>
  filter(topic_label != "Other") |>
  summarise(total = sum(n), .by = topic_label) |>
  arrange(desc(total)) |>
  pull(topic_label)

df_fpoe_pct <- df_fpoe_pct |>
  mutate(topic_label = factor(topic_label, levels = c(topic_order_fpoe, "Other")))

n_named_f <- length(topic_order_fpoe)
fpoe_colors <- setNames(
  c(colorRampPalette(brewer.pal(8, "Set2"))(n_named_f), "#cccccc"),
  c(topic_order_fpoe, "Other")
)

period_x_labels <- c(
  "20" = "GP XX\n2002–03", "21" = "GP XXI\n2003–06",
  "22" = "GP XXII\n2006–08", "23" = "GP XXIII\n2008–13",
  "24" = "GP XXIV\n2013–17", "25" = "GP XXV\n2017–19",
  "26" = "GP XXVI\n2019–24", "27" = "GP XXVII\n2024–"
)

p2 <- ggplot(df_fpoe_pct,
             aes(x = factor(period_int), y = pct, fill = topic_label)) +
  geom_col(position = position_stack(reverse = TRUE), width = 0.8) +
  scale_x_discrete(labels = period_x_labels) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.02))) +
  scale_fill_manual(values = fpoe_colors, name = NULL) +
  labs(
    x = NULL, y = "Share of matched speeches",
    title = "How FPÖ's topical focus shifted across legislative periods",
    subtitle = "Share of FPÖ plenary speeches per topic group; top 7 topics shown"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title.position = "plot",
    legend.position     = "bottom",
    legend.key.size     = unit(0.45, "cm"),
    panel.grid.major.x  = element_blank()
  ) +
  guides(fill = guide_legend(nrow = 2))

print(p2)

# ── Plot 3: FPÖ topic over-indexing ──────────────────────────────────────────
fpoe_share_overall <- df_fpoe |>
  filter(!map_lgl(topics, is.null)) |>
  unnest(topics) |>
  summarise(
    n_fpoe  = sum(is_fpoe, na.rm = TRUE),
    n_total = n(),
    .by = topics
  ) |>
  mutate(
    fpoe_share   = n_fpoe / n_total,
    overall_fpoe = sum(n_fpoe) / sum(n_total),
    over_index   = fpoe_share / overall_fpoe
  ) |>
  filter(n_total >= 100)

top_n_show <- 18
df_oi <- bind_rows(
  slice_max(fpoe_share_overall, over_index, n = top_n_show %/% 2),
  slice_min(fpoe_share_overall, over_index, n = top_n_show %/% 2)
) |>
  distinct(topics, .keep_all = TRUE) |>
  mutate(dir = if_else(over_index >= 1, "over", "under"))

p3 <- ggplot(df_oi, aes(x = over_index - 1, y = reorder(topics, over_index),
                        fill = dir)) +
  geom_col(width = 0.7, alpha = 0.85) +
  geom_vline(xintercept = 0, linewidth = 0.4) +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_manual(values = c(over = "#cb181d", under = "#2171b5"), guide = "none") +
  labs(
    x = "Over-/under-representation vs. overall FPÖ speech share",
    y = NULL,
    title = "FPÖ dominates security, migration and EU topics; avoids social policy",
    subtitle = "Topics where FPÖ's share of all speeches deviates most from its overall share"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title.position = "plot", panel.grid.major.y = element_blank())

print(p3)

# ── Plot 4: Government vs. opposition topic profile ───────────────────────────
df_role <- df_fpoe |>
  filter(is_fpoe, !map_lgl(topics, is.null), !is.na(year)) |>
  mutate(role = case_when(
    year >= 2002 & year <= 2005 ~ "Government (2002–05)",
    year >= 2017 & year <= 2019 ~ "Government (2017–19)",
    TRUE                        ~ "Opposition"
  )) |>
  unnest(topics) |>
  count(role, topics) |>
  group_by(role) |>
  mutate(pct = n / sum(n)) |>
  ungroup()

top_topics_role <- df_role |>
  summarise(total = sum(n), .by = topics) |>
  slice_max(total, n = 10, with_ties = FALSE) |>
  pull(topics)

df_role_top <- df_role |>
  filter(topics %in% top_topics_role) |>
  pivot_wider(id_cols = topics, names_from = role, values_from = pct, values_fill = 0) |>
  mutate(oppo_minus_govt = Opposition - rowMeans(pick(starts_with("Government")))) |>
  arrange(desc(abs(oppo_minus_govt)))

df_role_plot <- df_role |>
  filter(topics %in% top_topics_role) |>
  mutate(
    role_group = if_else(str_starts(role, "Government"), "Government", "Opposition"),
    topics     = factor(topics, levels = df_role_top$topics)
  ) |>
  summarise(pct = mean(pct), .by = c(topics, role_group))

p4 <- ggplot(df_role_plot, aes(x = pct, y = topics, fill = role_group)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6, alpha = 0.85) +
  scale_x_continuous(labels = percent_format(accuracy = 0.1)) +
  scale_fill_manual(
    values = c("Government" = "#2171b5", "Opposition" = "#cb181d"),
    name = NULL
  ) +
  labs(
    x = "Share of FPÖ speeches on this topic",
    y = NULL,
    title = "In opposition, FPÖ talks more about migration and security",
    subtitle = "Top-10 topics by speech count; government vs. opposition periods averaged"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title.position = "plot",
    panel.grid.major.y  = element_blank(),
    legend.position     = "top"
  )

print(p4)

cat("\nDone. Four plots printed.\n")
