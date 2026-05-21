# build_speech_dataset.R
#
# Builds a speech-level dataset for all National Council plenary speeches,
# periods XX-XXVII. Variables: speaker, gender, party, date, speech duration,
# debate classification, and item-level topics (eurovoc, keywords).
#
# Run from the post directory:
#   setwd("posts/2026-02-11-ParlAT-release")
#   source("build_speech_dataset.R")
#
# Expensive steps are checkpointed as intermediate RDS files so the script
# is safely resumable after interruption.

library(ParlAT)
library(tidyverse)

PERIODS <- seq(20, 27)
HERE <- tryCatch(
  dirname(rstudioapi::getSourceEditorContext()$path),
  error = function(e) getwd()
)

cache <- function(path, expr) {
  full <- file.path(HERE, path)
  if (file.exists(full)) {
    message(sprintf("  loading cached: %s", path))
    readRDS(full)
  } else {
    result <- expr
    saveRDS(result, full)
    message(sprintf("  saved: %s (%d rows)", path, nrow(result)))
    result
  }
}

# ── 1. Meeting metadata ───────────────────────────────────────────────────────
message("Step 1: plenary meeting list")
df_meetings_all <- cache("_cache_meetings_all.rds", {
  get_plenary_meetings(
    institution            = "NR",
    legis_period           = PERIODS,
    meeting_and_activities = "meetings"
  )
})
message(sprintf("  %d meetings", nrow(df_meetings_all)))

# ── 2. Speech-level data with duration ───────────────────────────────────────
message("Step 2: speech details for all meetings (slow – ~30-60 min first run)")
df_speeches_raw <- cache("_cache_speeches_raw.rds", {
  safe_results <- df_meetings_all$meeting_url |>
    map(safely(\(u) get_plenary_meeting_details(url = u, details_on = "speakers")))

  failed <- df_meetings_all$meeting_url[
    map_lgl(safe_results, \(x) !is.null(x$error))
  ]
  if (length(failed) > 0)
    message(sprintf("  %d meeting(s) failed:\n%s",
                    length(failed), paste(failed, collapse = "\n")))

  safe_results |> map("result") |> compact() |> list_rbind()
})
message(sprintf("  %d speeches", nrow(df_speeches_raw)))

# ── 3. MP metadata: gender + party per period ─────────────────────────────────
message("Step 3: MP metadata")
df_mps_all <- cache("_cache_mps_all.rds", {
  map(PERIODS, \(p) get_mps(institution = "NR", legis_period = p)) |>
    list_rbind()
})

# ── 4. Decisions per meeting (speech → item bridge) ──────────────────────────
# Each agenda item (TOP) has a resolution_url = item_url in df_all_items.
# Speeches have debate_id; decisions have resolution_top ("TOP N").
# Joining debate_id == as.integer(str_extract(resolution_top, "\\d+")) covers
# ~68% of debates (regular agenda items). Special debates (KD, AS, DA) with
# high debate_ids don't appear on the formal agenda and have no item URL.
message("Step 4: decisions per meeting (item bridge)")
df_decisions_raw <- cache("_cache_decisions_raw.rds", {
  safe_results <- df_meetings_all$meeting_url |>
    map(safely(\(u) get_plenary_meeting_details(url = u, details_on = "decisions")))

  failed <- df_meetings_all$meeting_url[
    map_lgl(safe_results, \(x) !is.null(x$error))
  ]
  if (length(failed) > 0)
    message(sprintf("  %d meeting(s) failed:\n%s",
                    length(failed), paste(failed, collapse = "\n")))

  safe_results |> map("result") |> compact() |> list_rbind()
})
message(sprintf("  %d decision rows", nrow(df_decisions_raw)))

# ── 5. Load items ─────────────────────────────────────────────────────────────
message("Step 5: loading df_all_items")
df_all_items <- readRDS(file.path(HERE, "df_all_items.rds"))

# ── 6. Normalise legis_period to integer in all frames ───────────────────────
# ParlAT uses Roman numerals ("XXVII") in some functions and integers in others.
to_period_int <- function(x) {
  suppressWarnings(as.integer(x)) |>
    (\(v) if_else(is.na(v), as.integer(as.roman(x)), v))()
}

df_meetings_norm <- df_meetings_all |>
  mutate(period_int = to_period_int(legis_period))

df_speeches_norm <- df_speeches_raw |>
  mutate(period_int = to_period_int(legis_period))

df_mps_norm <- df_mps_all |>
  unnest(mp_details) |>
  mutate(period_int = to_period_int(legis_period)) |>
  select(pad_intern, name, gender, parl_group, period_int) |>
  distinct(pad_intern, period_int, .keep_all = TRUE)

# Decisions lookup: meeting_url + top_nr → item_url
df_decisions_lookup <- df_decisions_raw |>
  mutate(top_nr = as.integer(str_extract(resolution_top, "\\d+"))) |>
  select(meeting_url, top_nr, item_url = resolution_url) |>
  filter(!is.na(top_nr))

# Items lookup: item_url → topics, eurovoc, keywords
df_items_lookup <- df_all_items |>
  select(item_url, topics, eurovoc, keywords) |>
  distinct(item_url, .keep_all = TRUE)

# ── 7. Assemble ───────────────────────────────────────────────────────────────
message("Step 6: assembling final dataset")

# Some TOPs contain multiple items voted on together (many-to-many between
# debates and decisions). We join with relationship = "many-to-many", then
# collapse multi-item rows back to one row per speech by taking the union of
# topics/eurovoc/keywords across all matched items.
df_speech_dataset <- df_speeches_norm |>
  # Date from meeting metadata
  left_join(
    df_meetings_norm |> select(meeting_url, date),
    by = "meeting_url"
  ) |>
  # Gender + party from MP metadata
  left_join(df_mps_norm, by = c("pad_intern", "period_int")) |>
  # Bridge speech → item via decisions (debate_id = TOP number)
  left_join(df_decisions_lookup,
            by = c("meeting_url", "debate_id" = "top_nr"),
            relationship = "many-to-many") |>
  # Topics from item metadata (100% hit rate once item_url is matched)
  left_join(df_items_lookup, by = "item_url") |>
  # Parse duration "MM:SS" → integer seconds
  mutate(
    duration_sec = {
      parts <- str_split_fixed(duration, ":", 2)
      suppressWarnings(as.integer(parts[, 1]) * 60L + as.integer(parts[, 2]))
    }
  ) |>
  # Collapse multi-item rows to one row per speech, unioning topics.
  # speech_nr resets per debate, so the unique key is (meeting_url, debate_id, speech_nr).
  group_by(meeting_url, debate_id, speech_nr) |>
  summarise(
    across(c(date, legis_period, period_int, speaker_name, name, gender,
             parl_group, debate_type, debate_typetext, debate_text,
             pad_intern, duration, duration_sec, speech_limit, wm_type, start_time),
           first),
    item_url = list(discard(unique(item_url), is.na)),
    topics   = list(discard(unique(unlist(topics)),  is.na)),
    eurovoc  = list(discard(unique(unlist(eurovoc)),  is.na)),
    keywords = list(discard(unique(unlist(keywords)), is.na)),
    .groups  = "drop"
  ) |>
  # Convert empty lists back to NULL for consistency
  mutate(
    item_url = map(item_url, \(x) if (length(x) == 0) NULL else x),
    topics   = map(topics,   \(x) if (length(x) == 0) NULL else x),
    eurovoc  = map(eurovoc,  \(x) if (length(x) == 0) NULL else x),
    keywords = map(keywords, \(x) if (length(x) == 0) NULL else x)
  ) |>
  select(
    date,
    legis_period, period_int,
    pad_intern, speaker_name, name, gender, parl_group,
    debate_type, debate_typetext, debate_text,
    item_url, topics, eurovoc, keywords,
    duration, duration_sec,
    speech_nr, speech_limit, wm_type, start_time,
    meeting_url
  )

# ── 8. Diagnostics ────────────────────────────────────────────────────────────
n_total     <- nrow(df_speech_dataset)
n_no_gender <- sum(is.na(df_speech_dataset$gender))
n_no_topic  <- sum(map_lgl(df_speech_dataset$topics, is.null))
n_no_dur    <- sum(is.na(df_speech_dataset$duration_sec))

message(sprintf(
  "Dataset: %d speeches | missing gender: %d (%.1f%%) | missing topics: %d (%.1f%%) | missing duration: %d (%.1f%%)",
  n_total,
  n_no_gender, 100 * n_no_gender / n_total,
  n_no_topic,  100 * n_no_topic  / n_total,
  n_no_dur,    100 * n_no_dur    / n_total
))

# ── 9. Save ───────────────────────────────────────────────────────────────────
out_path <- file.path(HERE, "df_speech_dataset.rds")
saveRDS(df_speech_dataset, out_path)
message(sprintf("Saved: %s (%.1f MB)", out_path, file.size(out_path) / 1e6))
