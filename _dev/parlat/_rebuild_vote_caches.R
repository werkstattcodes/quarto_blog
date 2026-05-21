library(dplyr); library(purrr); library(stringr); library(tidyr)

df_votes_raw <- readRDS("_cache_vote_rows_gp28.rds")

split_parties <- function(raw) {
  if (is.na(raw) || raw == "-") return(character(0))
  trimws(strsplit(raw, ", ")[[1]])
}

PARTIES_6 <- c("ÖVP", "SPÖ", "FPÖ", "GRÜNE", "NEOS")

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
  tidyr::expand_grid(party = PARTIES_6) |>
  rowwise() |>
  mutate(
    vote = case_when(
      vote_type == "unanimous"   ~ "for",
      party %in% dafuer_list     ~ "for",
      party %in% dagegen_list    ~ "against",
      TRUE                       ~ NA_character_
    )
  ) |>
  ungroup()

saveRDS(df_votes_long, "_cache_votes_long_gp28.rds")
cat("Saved votes long:", nrow(df_votes_long), "rows,", n_distinct(df_votes_long$item_url), "items\n")
cat("\nVote distribution:\n")
print(count(df_votes_long, party, vote) |> pivot_wider(names_from=vote, values_from=n))
cat("\nVote type breakdown:\n")
print(count(df_votes_raw, vote_type, vote_result))
