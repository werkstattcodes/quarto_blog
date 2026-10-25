# Pipeline for "How do Parties vote in the Austrian National Council? Part II".
#
# Run with the working directory set to this folder (see run.R):
#   setwd("_dev/parlat-voting"); targets::tar_make()
#
# The three raw API caches under posts/<slug>/_data/ are tracked as file targets
# and read inside the commands that need them, so ~218 MB of raw fetches never
# land in _targets/objects/. Refreshing them is a separate, opt-in step —
# see refresh_data.R.

library(targets)
library(tarchetypes)

tar_source("R")

tar_option_set(
  packages = c(
    "dplyr",
    "tidyr",
    "purrr",
    "stringr",
    "tibble",
    "readr",
    "lubridate",
    "ggplot2",
    "ggtext",
    "ggalluvial",
    "scales",
    "naniar"
  ),
  format = "qs" # qs2-backed; faster and smaller than the default rds
)

# The four flip-tile plots differ only in which two stage columns define the flip,
# the fill colour, and the wording. One builder plus this table replaces ~550
# lines of near-identical chunk code.
flip_specs <- tibble::tibble(
  suffix = c("no_to_yes", "yes_to_no", "comm_no_to_yes", "comm_yes_to_no"),
  from_col = c("no_second", "yes_second", "no_committee", "yes_committee"),
  to_col = c("yes_third", "no_third", "yes_second", "no_second"),
  fill_hex = c(PAL$green, PAL$red, PAL$green, PAL$red),
  title = c(
    "No-to-yes vote flips between zweite and dritte Lesung",
    "Yes-to-no vote flips between zweite and dritte Lesung",
    "No-to-yes vote flips between Ausschuss and zweite Lesung",
    "Yes-to-no vote flips between Ausschuss and zweite Lesung"
  ),
  stage_from = c(
    "second reading (zweite Lesung)",
    "second reading (zweite Lesung)",
    "committee (Ausschuss)",
    "committee (Ausschuss)"
  ),
  stage_to = c(
    "third reading (dritte Lesung)",
    "third reading (dritte Lesung)",
    "second reading (zweite Lesung)",
    "second reading (zweite Lesung)"
  ),
  from_word = c("against", "for", "against", "for"),
  to_word = c("for", "against", "for", "against"),
  shift_phrase = c(
    "a shift from opposition to support",
    "a withdrawal of previously expressed support",
    "a shift from opposition to support",
    "a withdrawal of previously expressed support"
  ),
  contributes = c(
    "Only items with a recorded second and third reading vote contribute.",
    "Only items with a recorded second and third reading vote contribute.",
    "Only items with a recorded committee and second-reading vote contribute.",
    "Only items with a recorded committee and second-reading vote contribute."
  )
)

flip_plots <- tar_map(
  values = flip_specs,
  names = "suffix",
  tar_target(flip_counts, build_flip_counts(votes_wide, from_col, to_col)),
  tar_target(
    flip_grid,
    build_flip_grid(
      flip_counts,
      all_parties,
      gov_parties_lookup,
      party_period_presence,
      party_order
    )
  ),
  tar_target(
    p_flip,
    build_flip_tile_plot(
      flip_grid,
      fill_hex,
      title,
      flip_subtitle(stage_from, stage_to, from_word, to_word, shift_phrase, contributes),
      vote_graph_caption
    )
  )
)

list(
  # ---- raw inputs (tracked by content hash, never copied into the store) ------
  tar_target(path_items_raw, data_path("items_20_28.rds"), format = "file"),
  tar_target(path_item_details_raw, data_path("item_details_raw.rds"), format = "file"),
  tar_target(path_aub_details_raw, data_path("aub_details_raw.rds"), format = "file"),

  # ---- analysis-ready frames -------------------------------------------------
  tar_target(items_meta, read_items_meta(path_items_raw)),
  tar_target(items_in_scope_types, build_items_in_scope_types(items_meta)),
  tar_target(failed_items, read_failed_items(path_item_details_raw)),
  tar_target(item_stages, read_item_stages(path_item_details_raw)),
  tar_target(data_cutoff, max(item_stages$stage_date)),
  tar_target(aub_details, read_aub_details(path_aub_details_raw)),
  tar_target(committee_rows, build_committee_rows(aub_details, item_stages)),

  # committee-phase context (what the bill's own page records)
  tar_target(committee_stages, build_committee_stages(item_stages)),
  tar_target(
    committee_category_summary,
    build_committee_category_summary(committee_stages)
  ),
  tar_target(committee_vote_rows, build_committee_vote_rows(committee_stages)),

  # scope filter, duplicate handling, vote parsing
  tar_target(stages_scope, build_stages_scope(item_stages, committee_rows)),
  tar_target(dup_rows, build_dup_rows(stages_scope)),
  tar_target(non_pairs_details, build_non_pairs_details(stages_scope, dup_rows)),
  tar_target(votes_regular, build_votes_regular(stages_scope, dup_rows)),
  tar_target(votes_parsed, build_votes_parsed(votes_regular)),
  tar_target(votes_long, build_votes_long(votes_parsed)),
  tar_target(votes_wide, build_votes_wide(votes_long)),

  # ---- shared constants ------------------------------------------------------
  # Extracted out of the first flip-plot chunk, where they used to be defined as a
  # side effect while eight other chunks depended on them.
  tar_target(gov_parties_lookup, build_gov_parties_lookup()),
  tar_target(party_period_presence, build_party_period_presence(votes_wide)),
  tar_target(all_parties, build_all_parties(party_period_presence, gov_parties_lookup)),
  tar_target(party_order, build_party_order(party_period_presence, all_parties)),

  # captions
  tar_target(analysed_type_labels, build_analysed_type_labels(votes_wide, items_meta)),
  tar_target(
    vote_graph_caption,
    build_vote_graph_caption(analysed_type_labels, data_cutoff)
  ),
  tar_target(vote_graph_caption_short, build_vote_graph_caption_short(data_cutoff)),

  # ---- flip tile plots (static branching) ------------------------------------
  flip_plots,

  # ---- transitions and the four "where do flips happen" angles ---------------
  tar_target(stage_positions, build_stage_positions(votes_wide, gov_parties_lookup)),
  tar_target(transitions, build_transitions(stage_positions)),
  tar_target(p_vis_missingness, build_missingness_plot(votes_wide)),
  tar_target(p_where_bars, build_where_bars(transitions, vote_graph_caption_short)),
  tar_target(
    p_where_alluvial,
    build_where_alluvial(stage_positions, vote_graph_caption_short)
  ),
  tar_target(
    where_tile_grid,
    build_where_tile_grid(
      transitions,
      all_parties,
      gov_parties_lookup,
      party_period_presence,
      party_order
    )
  ),
  tar_target(p_where_heatmap, build_where_heatmap(where_tile_grid, vote_graph_caption_short)),
  tar_target(
    where_tile_rate_grid,
    build_where_tile_rate_grid(where_tile_grid, transitions)
  ),
  tar_target(
    p_where_heatmap_percent,
    build_where_heatmap_percent(where_tile_rate_grid, vote_graph_caption_short)
  ),
  tar_target(
    p_flip_stage_period,
    build_flip_stage_period(transitions, gov_parties_lookup, vote_graph_caption_short)
  ),

  # ---- items decided at all three stages -------------------------------------
  tar_target(three_stage_wide, build_three_stage_wide(votes_long, gov_parties_lookup)),
  tar_target(three_stage_item_n, build_three_stage_item_n(three_stage_wide)),
  tar_target(three_stage_changes, build_three_stage_changes(three_stage_wide)),
  tar_target(latest_c2_item, build_latest_c2_item(three_stage_wide)),
  tar_target(
    latest_c2_parties,
    build_latest_c2_parties(three_stage_wide, latest_c2_item)
  )
)
