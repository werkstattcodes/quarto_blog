# Refresh the cached API fetches. NOT part of the targets pipeline.
#
# Purpose : re-download the raw ParlAT data the pipeline reads.
# Inputs  : the parlament.gv.at API, via the ParlAT package.
# Outputs : posts/2026-06-08-ParlAT-votingPatterns-2/_data/{items_20_28,
#           item_details_raw,aub_details_raw}.rds
# Runtime : long. get_item_details() is one HTTP GET per item and this fetches
#           every NR item plus every committee report for GP 20-28. Expect tens
#           of thousands of requests even with 4 workers.
#
# Why it is separate: the pipeline treats these three files as `format = "file"`
# targets, so rewriting them here is exactly what invalidates the downstream
# targets on the next tar_make(). Keeping the fetch out of the DAG means an
# ordinary tar_make() never hits the network.
#
# Usage: run the sections you actually need — they are independent.

library(ParlAT)
library(tidyverse)
library(furrr)

source(file.path("R", "constants.R"))

# Wrap in safely() so a single failed request returns an error slot instead of
# halting the whole loop; failures are inspected afterwards rather than aborting.
safe_get_item_details <- safely(get_item_details)

fetch_details <- function(urls) {
  plan(multisession, workers = 4)
  on.exit(plan(sequential), add = TRUE)

  urls %>%
    future_map(
      \(x) {
        safe_result <- safe_get_item_details(item_url = x, stages = TRUE)
        safe_result$item_url <- x # attach URL so failures can be identified later
        safe_result
      },
      .progress = TRUE,
      .options = furrr_options(packages = c("ParlAT", "purrr", "tibble"))
    )
}

# Refuse to overwrite a good cache with an all-failed fetch.
write_if_usable <- function(results, path, what) {
  success_n <- sum(map_lgl(results, \(x) is.null(x$error)))
  if (success_n == 0) {
    stop(
      "All ", length(results), " ", what, " requests failed; ",
      "the existing cache was NOT overwritten. First error: ",
      conditionMessage(results[[1]]$error),
      call. = FALSE
    )
  }
  message(success_n, "/", length(results), " ", what, " requests succeeded.")
  readr::write_rds(results, path)
}

# ── 1. All NR items, GP 20-28 ────────────────────────────────────────────────
refresh_items <- function() {
  items_20_28 <- seq(20, 28) %>%
    map(\(x) get_items(legis_period = x, institution = "NR")) %>%
    list_rbind() # stack per-period tibbles into one dataframe

  message("Fetched ", nrow(items_20_28), " items.")
  readr::write_rds(items_20_28, data_path("items_20_28.rds"))
}

# ── 2. Item details with stages, for vote-bearing item types ─────────────────
refresh_item_details <- function() {
  items_20_28 <- readr::read_rds(data_path("items_20_28.rds"))

  item_urls <- items_20_28 %>%
    filter(str_detect(type_doc, regex(item_type_regex()))) %>%
    pull(item_url)

  fetch_details(item_urls) %>%
    write_if_usable(data_path("item_details_raw.rds"), "item-detail")
}

# ── 3. Committee reports (Ausschussbericht) ──────────────────────────────────
# For most bills the committee vote lives on the report, not the bill's own page.
refresh_aub_details <- function() {
  aub_urls <- readr::read_rds(data_path("items_20_28.rds")) %>%
    filter(type_doc == "AUB") %>%
    pull(item_url) %>%
    unique()

  fetch_details(aub_urls) %>%
    write_if_usable(data_path("aub_details_raw.rds"), "committee-report")
}

# refresh_items()
# refresh_item_details()
# refresh_aub_details()
