# check_voting_coverage.R
#
# Cross-check: do RV + ANTR + E cover the full universe of plenary votes
# in the Austrian National Council?
#
# Strategy:
#   1. Load BNR items (Beschlüsse des Nationalrats) for period 28
#      — BNR = every item the NR formally resolved on (i.e. voted on)
#   2. Extract the source item type from BNR references
#   3. Compare against our current vote dataset (RV + ANTR + E)
#   4. Identify missing item types

library(ParlAT)
library(dplyr)
library(purrr)
library(stringr)
library(tidyr)

HERE <- tryCatch(
  dirname(rstudioapi::getSourceEditorContext()$path),
  error = function(e) getwd()
)

GP <- 28

# ── 1. Load our existing vote dataset ────────────────────────────────────────
df_votes_raw <- readRDS(file.path(HERE, "_cache_vote_rows_gp28.rds"))
df_items     <- readRDS(file.path(HERE, "_cache_vote_urls_gp28.rds"))

cat("Our vote dataset:\n")
cat("  item URLs fetched (RV+ANTR+E):", nrow(df_items), "\n")
cat("  vote rows parsed:             ", nrow(df_votes_raw), "\n\n")

# ── 2. Fetch BNR items for period 28 ─────────────────────────────────────────
cache_bnr_urls <- file.path(HERE, "_cache_bnr_urls_gp28.rds")
if (file.exists(cache_bnr_urls)) {
  df_bnr <- readRDS(cache_bnr_urls)
  message("Loaded cached BNR URLs: ", nrow(df_bnr))
} else {
  message("Fetching BNR item list...")
  df_bnr <- get_items(legis_period = GP, item = "BNR")
  saveRDS(df_bnr, cache_bnr_urls)
  message("Saved ", nrow(df_bnr), " BNR items")
}
cat("BNR items (Beschlüsse des Nationalrats) in period 28:", nrow(df_bnr), "\n\n")

# ── 3. Fetch BNR details to extract references ───────────────────────────────
cache_bnr_details <- file.path(HERE, "_cache_bnr_details_gp28.rds")
if (file.exists(cache_bnr_details)) {
  df_bnr_details <- readRDS(cache_bnr_details)
  message("Loaded cached BNR details: ", nrow(df_bnr_details), " rows")
} else {
  message("Fetching BNR details (~few minutes)...")
  raw <- map(df_bnr$item_url, safely(get_item_details), .progress = TRUE)
  df_bnr_details <- raw |> map("result") |> compact() |> list_rbind()
  saveRDS(df_bnr_details, cache_bnr_details)
  message("Saved ", nrow(df_bnr_details), " BNR detail rows")
}

cat("BNR detail columns:", paste(names(df_bnr_details), collapse = ", "), "\n\n")

# ── 4. Extract source item type from BNR references ──────────────────────────
# References look like: "/gegenstand/XXVIII/RV/123" or "/gegenstand/XXVIII/ANTR/456"
# The item type is the third segment after /XXVIII/

if ("references" %in% names(df_bnr_details)) {
  ref_col <- df_bnr_details |>
    filter(!map_lgl(references, is.null)) |>
    select(item_url, references) |>
    unnest(references)

  cat("Sample BNR references:\n")
  print(head(ref_col, 10))
  cat("\n")

  # Extract type from URL pattern /gegenstand/XXVIII/<TYPE>/<id>
  ref_types <- ref_col |>
    mutate(ref_type = str_extract(references, "(?<=/XXVIII/)[A-Z0-9]+")) |>
    filter(!is.na(ref_type))

  cat("Item types referenced from BNR records:\n")
  print(count(ref_types, ref_type, sort = TRUE))
  cat("\n")

  # Which types are NOT in our dataset?
  our_types <- c("RV", "ANTR", "E")
  missing_types <- ref_types |>
    filter(!ref_type %in% our_types) |>
    count(ref_type, sort = TRUE)

  if (nrow(missing_types) > 0) {
    cat("Item types in BNR references but NOT in our dataset:\n")
    print(missing_types)
  } else {
    cat("All BNR-referenced item types are in our dataset.\n")
  }
} else {
  cat("No 'references' column found. Available columns:\n")
  print(names(df_bnr_details))

  # Inspect a sample of BNR detail rows to understand structure
  cat("\nSample BNR detail rows (first 3 items):\n")
  bnr_sample <- df_bnr_details |>
    distinct(item_url, .keep_all = TRUE) |>
    head(3)
  print(bnr_sample |> select(where(~ !is.list(.))), n = 3)
}

# ── 5. Cross-check: BNR count vs our voted items ─────────────────────────────
cat("\n── Coverage reconciliation ──────────────────────────────────────────────\n")
cat("BNR items (formal resolutions):  ", nrow(df_bnr), "\n")
cat("Our voted items (RV+ANTR+E):     ", nrow(df_votes_raw), "\n")
cat("\nNote: BNR only counts PASSED items; our dataset includes rejected votes.\n")

rejected_in_ours <- sum(df_votes_raw$vote_result == "rejected", na.rm = TRUE)
passed_in_ours   <- sum(df_votes_raw$vote_result == "passed", na.rm = TRUE)
cat("  Our passed items:  ", passed_in_ours, "\n")
cat("  Our rejected items:", rejected_in_ours, "\n")
cat("  Difference (passed in ours - BNR count): ", passed_in_ours - nrow(df_bnr), "\n")

# ── 6. All item types available in period 28 ─────────────────────────────────
cat("\n── All item types in df_all_items (if available) ────────────────────────\n")
if (file.exists(file.path(HERE, "df_all_items.rds"))) {
  df_all <- readRDS(file.path(HERE, "df_all_items.rds"))
  if ("item_type" %in% names(df_all)) {
    cat("All item types in df_all_items:\n")
    gp28_items <- df_all |>
      filter(legis_period == GP | legis_period == "XXVIII") |>
      count(item_type, sort = TRUE)
    print(gp28_items, n = 30)
  } else {
    cat("Columns in df_all_items:", paste(names(df_all), collapse = ", "), "\n")
    cat("Unique legis_period values:", paste(unique(df_all$legis_period), collapse = ", "), "\n")
  }
} else {
  cat("df_all_items.rds not found.\n")
}

cat("\nDone.\n")
