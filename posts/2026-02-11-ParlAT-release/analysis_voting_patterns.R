# analysis_voting_patterns.R
#
# Roll-call voting analysis for the Austrian National Council, period XXVIII.
# Item types covered:
#   RV   (97)  — Government bills; plenary 3rd-reading vote
#   ANTR (1225)— Parliamentary motions; nearly always a single plenary vote
#   E    (85)  — Resolutions; single plenary vote
#
# Voting data lives in get_item_details()$stage_name as a two-line string:
#   Line 1: session + stage description (angenommen / abgelehnt / Beschlossen)
#   Line 2: "Dafür: ÖVP, SPÖ, dagegen: FPÖ, GRÜNE, NEOS"
#              OR "Einstimmig"
#              OR "wechselnde Mehrheiten ..."
#
# Period XXVIII government: ÖVP + SPÖ (coalition since Jan 2025)
# Main opposition:          FPÖ (election winner 2024), GRÜNE, NEOS
#
# Run time (first run): ~15 min for full API fetch.
# Subsequent runs: instant (file-based cache).

library(ParlAT)
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

GP <- 28

# ── 1. Fetch item URLs ────────────────────────────────────────────────────────
cache_urls <- file.path(HERE, sprintf("_cache_vote_urls_gp%d.rds", GP))
if (file.exists(cache_urls)) {
  df_items <- readRDS(cache_urls)
  message("Loaded cached item URLs: ", nrow(df_items))
} else {
  message("Fetching item URLs...")
  df_items <- map_dfr(c("RV", "ANTR", "E"), function(t) {
    items <- tryCatch(
      get_items(legis_period = GP, item = t),
      error = function(e) { message("  failed: ", t); NULL }
    )
    if (is.null(items)) return(NULL)
    items |> mutate(item_type = t) |> select(item_type, item_url)
  })
  saveRDS(df_items, cache_urls)
  message("Saved ", nrow(df_items), " item URLs")
}

# ── 2. Fetch item details (cached) ────────────────────────────────────────────
cache_details <- file.path(HERE, sprintf("_cache_vote_details_gp%d.rds", GP))
if (file.exists(cache_details)) {
  df_details <- readRDS(cache_details)
  message("Loaded cached details: ", nrow(df_details), " stage rows, ",
          n_distinct(df_details$item_url), " items")
} else {
  message("Fetching item details (~15 min first run)...")
  raw <- map(df_items$item_url, safely(get_item_details), .progress = TRUE)
  df_details <- raw |> map("result") |> compact() |> list_rbind()
  saveRDS(df_details, cache_details)
  message("Saved ", nrow(df_details), " stage rows")
}

# ── 3. Parse voting strings ───────────────────────────────────────────────────
PARTIES <- c("ÖVP", "SPÖ", "FPÖ", "GRÜNE", "NEOS", "BZÖ", "JETZT", "Stronach", "LIF")

parse_vote_line <- function(stage_name) {
  # Returns list(result, type, dafuer_raw, dagegen_raw) or NULL
  if (!str_detect(stage_name, "\n")) return(NULL)

  parts      <- str_split_fixed(stage_name, "\n", 2)
  header     <- parts[1]
  vote_line  <- str_trim(parts[2])

  result <- case_when(
    str_detect(header, "angenommen|Beschlossen") ~ "passed",
    str_detect(header, "abgelehnt")              ~ "rejected",
    TRUE                                         ~ NA_character_
  )
  if (is.na(result)) return(NULL)

  if (str_detect(vote_line, "^Einstimmig")) {
    return(list(result = result, type = "unanimous",
                dafuer_raw = "all", dagegen_raw = "-"))
  }
  if (str_detect(vote_line, "wechselnde Mehrheiten|Getrennte Abstimmung")) {
    return(list(result = result, type = "mixed",
                dafuer_raw = NA_character_, dagegen_raw = NA_character_))
  }
  if (!str_detect(vote_line, "Dafür:")) return(NULL)

  dafuer_raw  <- str_extract(vote_line, "(?<=Dafür: ).+?(?=, dagegen:)")
  dagegen_raw <- str_extract(vote_line, "(?<=dagegen: ).+$")

  list(result = result, type = "divided",
       dafuer_raw = dafuer_raw, dagegen_raw = dagegen_raw)
}

is_third_reading <- function(stage_name) {
  str_detect(stage_name,
    "dritter Lesung|Beschlossen|Unselbständiger Entschließungsantrag|Entschließungsantrag")
}

# Extract one definitive vote per item: prefer 3rd reading, else first vote row
vote_rows <- df_details |>
  filter(str_detect(stage_name, "\n"),
         str_detect(stage_name, "Dafür:|Einstimmig|angenommen|abgelehnt")) |>
  filter(str_detect(stage_name, "Nationalrat|Nationalrates")) |>
  mutate(is_3rd = is_third_reading(stage_name)) |>
  arrange(item_url, desc(is_3rd), stage_date) |>
  distinct(item_url, .keep_all = TRUE)

message("Vote rows found: ", nrow(vote_rows))

# Parse each vote
df_votes_raw <- vote_rows |>
  mutate(parsed = map(stage_name, parse_vote_line)) |>
  filter(!map_lgl(parsed, is.null)) |>
  mutate(
    vote_result  = map_chr(parsed, "result"),
    vote_type    = map_chr(parsed, "type"),
    dafuer_raw   = map_chr(parsed, "dafuer_raw"),
    dagegen_raw  = map_chr(parsed, "dagegen_raw")
  ) |>
  select(item_url, title, type, topics, stage_date,
         vote_result, vote_type, dafuer_raw, dagegen_raw)

message("Parsed votes: ", nrow(df_votes_raw),
        " | unanimous: ", sum(df_votes_raw$vote_type == "unanimous"),
        " | divided: ",   sum(df_votes_raw$vote_type == "divided"),
        " | mixed: ",     sum(df_votes_raw$vote_type == "mixed"))

# ── 4. Build long vote matrix ─────────────────────────────────────────────────
split_parties <- function(raw) {
  if (is.na(raw) || raw == "-") return(character(0))
  str_split(raw, ",\\s*")[[1]] |> str_trim()
}

PARTIES_6 <- c("ÖVP", "SPÖ", "FPÖ", "GRÜNE", "NEOS")

df_votes_long <- df_votes_raw |>
  filter(vote_type %in% c("unanimous", "divided")) |>
  mutate(
    dafuer_list  = map(dafuer_raw,  split_parties),
    dagegen_list = map(dagegen_raw, split_parties)
  ) |>
  select(item_url, title, type, topics, stage_date,
         vote_result, vote_type, dafuer_list, dagegen_list) |>
  crossing(party = PARTIES_6) |>
  mutate(
    vote = case_when(
      vote_type == "unanimous"              ~ "for",
      map_lgl(seq_along(party), \(i) party[i] %in% dafuer_list[[i]])  ~ "for",
      map_lgl(seq_along(party), \(i) party[i] %in% dagegen_list[[i]]) ~ "against",
      TRUE ~ NA_character_
    )
  )

# Flatten the map_lgl calls (simpler approach)
df_votes_long <- df_votes_raw |>
  filter(vote_type %in% c("unanimous", "divided")) |>
  select(item_url, title, type, topics, stage_date,
         vote_result, vote_type, dafuer_raw, dagegen_raw) |>
  rowwise() |>
  mutate(
    dafuer_list  = list(split_parties(dafuer_raw)),
    dagegen_list = list(split_parties(dagegen_raw))
  ) |>
  ungroup() |>
  tidyr::expand_grid(party = PARTIES_6) |>   # one row per item × party
  rowwise() |>
  mutate(
    vote = case_when(
      vote_type == "unanimous"        ~ "for",
      party %in% dafuer_list          ~ "for",
      party %in% dagegen_list         ~ "against",
      TRUE                            ~ NA_character_
    )
  ) |>
  ungroup()

cat("\nVote distribution:\n")
print(count(df_votes_long, party, vote) |> pivot_wider(names_from=vote, values_from=n))

# ── 5. Plot 1: Party agreement matrix ────────────────────────────────────────
# For each party pair: % of divided votes where they voted the same way
df_divided <- df_votes_long |>
  filter(vote_type == "divided", !is.na(vote))

agreement_matrix <- df_divided |>
  select(item_url, party, vote) |>
  inner_join(
    df_divided |> select(item_url, party2 = party, vote2 = vote),
    by = "item_url",
    relationship = "many-to-many"
  ) |>
  filter(party != party2) |>
  group_by(party, party2) |>
  summarise(
    n_votes   = n(),
    n_agree   = sum(vote == vote2),
    pct_agree = n_agree / n_votes,
    .groups   = "drop"
  )

party_order <- c("ÖVP", "SPÖ", "FPÖ", "GRÜNE", "NEOS")

p_agree <- ggplot(
  agreement_matrix |>
    mutate(
      party  = factor(party,  levels = party_order),
      party2 = factor(party2, levels = rev(party_order))
    ),
  aes(x = party, y = party2, fill = pct_agree)
) +
  geom_tile(colour = "white", linewidth = 0.5) +
  geom_text(aes(label = scales::percent(pct_agree, accuracy = 1)),
            size = 3.5, colour = "white", fontface = "bold") +
  scale_fill_distiller(palette = "RdYlGn", direction = 1,
                       limits = c(0, 1),
                       labels = percent_format(accuracy = 1),
                       name = "Agreement") +
  labs(
    x = NULL, y = NULL,
    title = "How often do Austrian parties vote the same way?",
    subtitle = sprintf("Share of contested votes with identical vote choice, NR period XXVIII (n=%d divided votes)",
                       n_distinct(df_divided$item_url))
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title.position = "plot",
    axis.text  = element_text(size = 11),
    legend.position = "right",
    panel.grid = element_blank()
  )

print(p_agree)

# ── 6. Plot 2: Coalition vs opposition cohesion ───────────────────────────────
# ÖVP + SPÖ = government; FPÖ, GRÜNE, NEOS = opposition
df_govt_oppo <- df_divided |>
  mutate(
    bloc = case_when(
      party %in% c("ÖVP", "SPÖ") ~ "Government (ÖVP+SPÖ)",
      TRUE                        ~ "Opposition"
    )
  ) |>
  group_by(item_url, vote_result, bloc) |>
  summarise(
    pct_for = mean(vote == "for"),
    .groups = "drop"
  ) |>
  group_by(item_url, vote_result) |>
  pivot_wider(names_from = bloc, values_from = pct_for)

# ── 7. Plot 3: FPÖ isolation — how often does FPÖ vote alone vs with others ───
fpoe_votes <- df_divided |>
  filter(party == "FPÖ") |>
  select(item_url, fpoe_vote = vote)

other_votes <- df_divided |>
  filter(party != "FPÖ") |>
  group_by(item_url, vote) |>
  summarise(n_parties = n(), .groups = "drop")

fpoe_alignment <- fpoe_votes |>
  left_join(
    df_divided |>
      filter(party != "FPÖ") |>
      select(item_url, party, vote),
    by = "item_url"
  ) |>
  group_by(item_url, fpoe_vote) |>
  summarise(
    n_same     = sum(vote == fpoe_vote, na.rm = TRUE),
    n_opposite = sum(vote != fpoe_vote, na.rm = TRUE),
    .groups    = "drop"
  ) |>
  mutate(
    alignment = case_when(
      n_same == 4 ~ "All others agree",
      n_same >= 2 ~ "Majority agrees",
      n_same == 1 ~ "One party agrees",
      n_same == 0 ~ "FPÖ alone"
    )
  )

p_fpoe <- fpoe_alignment |>
  count(alignment) |>
  mutate(
    alignment = factor(alignment,
      levels = c("All others agree", "Majority agrees", "One party agrees", "FPÖ alone")),
    pct = n / sum(n)
  ) |>
  ggplot(aes(x = pct, y = alignment, fill = alignment)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = paste0(scales::percent(pct, accuracy = 1), " (", n, ")")),
            hjust = -0.1, size = 3.5) +
  scale_x_continuous(labels = percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.15))) +
  scale_fill_manual(values = c(
    "All others agree" = "#2171b5",
    "Majority agrees"  = "#6baed6",
    "One party agrees" = "#fdae6b",
    "FPÖ alone"        = "#e6550d"
  )) +
  labs(
    x = "Share of contested votes", y = NULL,
    title = "How isolated is FPÖ in its voting positions?",
    subtitle = "Contested votes in NR period XXVIII, by how many other parties voted with FPÖ"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title.position = "plot", panel.grid.major.y = element_blank())

print(p_fpoe)

# ── 8. Plot 4: Topic contestedness ───────────────────────────────────────────
if ("topics" %in% names(df_votes_raw) && any(!map_lgl(df_votes_raw$topics, is.null))) {
  df_topic_contest <- df_votes_raw |>
    filter(!map_lgl(topics, is.null), vote_type != "mixed") |>
    unnest(topics) |>
    group_by(topics) |>
    summarise(
      n_total    = n(),
      n_divided  = sum(vote_type == "divided"),
      n_rejected = sum(vote_result == "rejected"),
      pct_contested = n_divided / n_total,
      .groups = "drop"
    ) |>
    filter(n_total >= 5) |>
    arrange(desc(pct_contested))

  p_topic <- df_topic_contest |>
    slice_max(pct_contested, n = 15) |>
    mutate(topics = fct_reorder(topics, pct_contested)) |>
    ggplot(aes(x = pct_contested, y = topics)) +
    geom_col(fill = "#cb181d", alpha = 0.8, width = 0.7) +
    scale_x_continuous(labels = percent_format(accuracy = 1),
                       expand = expansion(mult = c(0, 0.05))) +
    labs(
      x = "Share of votes that were contested (not unanimous)",
      y = NULL,
      title = "Which topics are most contested in parliament?",
      subtitle = "Top 15 topics by share of divided votes, NR period XXVIII (min. 5 items)"
    ) +
    theme_minimal(base_size = 12) +
    theme(plot.title.position = "plot", panel.grid.major.y = element_blank())

  print(p_topic)
}

cat("\nDone.\n")
