# Stage positions, the transition table, and the three-stage subset.

# Position held by each party at each stage (NA = no recorded vote there), plus
# the government/opposition role for that period.
build_stage_positions <- function(votes_wide, gov_parties_lookup) {
  votes_wide %>%
    dplyr::mutate(
      pos_committee = dplyr::case_when(
        yes_committee == 1 ~ "Dafür",
        no_committee == 1 ~ "Dagegen",
        .default = NA_character_
      ),
      pos_second = dplyr::case_when(
        yes_second == 1 ~ "Dafür",
        no_second == 1 ~ "Dagegen",
        .default = NA_character_
      ),
      pos_third = dplyr::case_when(
        yes_third == 1 ~ "Dafür",
        no_third == 1 ~ "Dagegen",
        .default = NA_character_
      )
    ) %>%
    dplyr::left_join(
      gov_parties_lookup %>% dplyr::select(legis_period, party, is_gov),
      by = c("legis_period", "party")
    ) %>%
    dplyr::mutate(
      role = factor(
        dplyr::if_else(tidyr::replace_na(is_gov, FALSE), "Government", "Opposition"),
        levels = c("Government", "Opposition")
      )
    )
}

# One row per party × item × transition, keeping the position held before and
# after. from_pos is what lets flip rates be conditioned on the pairs actually at
# risk of flipping.
build_transitions <- function(stage_positions) {
  dplyr::bind_rows(
    stage_positions %>%
      dplyr::transmute(
        item_url,
        legis_period,
        party,
        role,
        transition = "Committee → 2nd reading",
        from_pos = pos_committee,
        to_pos = pos_second
      ),
    stage_positions %>%
      dplyr::transmute(
        item_url,
        legis_period,
        party,
        role,
        transition = "2nd → 3rd reading",
        from_pos = pos_second,
        to_pos = pos_third
      )
  ) %>%
    dplyr::filter(!is.na(from_pos), !is.na(to_pos)) %>% # observed at both ends of the step
    dplyr::mutate(
      change = dplyr::case_when(
        from_pos == to_pos ~ "no change",
        from_pos == "Dagegen" & to_pos == "Dafür" ~ "no → yes",
        from_pos == "Dafür" & to_pos == "Dagegen" ~ "yes → no"
      ),
      transition = factor(transition, levels = TRANSITION_LEVELS)
    )
}

# ---- three-stage subset ------------------------------------------------------

# Items carrying a recorded regular vote at committee, second AND third reading,
# so a party's position can be traced across the full path.
build_three_stage_wide <- function(votes_long, gov_parties_lookup) {
  three_stage_long <- votes_long %>%
    dplyr::filter(reading %in% c("committee", "second", "third")) %>%
    dplyr::mutate(
      pos = dplyr::case_when(
        yes == 1 ~ "Dafür",
        no == 1 ~ "Dagegen",
        .default = NA_character_
      )
    )

  three_stage_items <- three_stage_long %>%
    dplyr::distinct(item_url, reading) %>%
    dplyr::count(item_url, name = "n_stages") %>%
    dplyr::filter(n_stages == 3)

  three_stage_long %>%
    dplyr::semi_join(three_stage_items, by = "item_url") %>%
    dplyr::select(legis_period, item_url, title, party, reading, pos, stage_date) %>%
    tidyr::pivot_wider(names_from = reading, values_from = c(pos, stage_date)) %>%
    dplyr::left_join(
      gov_parties_lookup %>% dplyr::select(legis_period, party, is_gov),
      by = c("legis_period", "party")
    ) %>%
    dplyr::mutate(
      role = dplyr::if_else(tidyr::replace_na(is_gov, FALSE), "Government", "Opposition"),
      change_committee_2nd = pos_committee != pos_second, # NA if a stage is missing
      change_2nd_3rd = pos_second != pos_third
    )
}

# Count of items observed at all three stages — reported in the post's prose.
build_three_stage_item_n <- function(three_stage_wide) {
  dplyr::n_distinct(three_stage_wide$item_url)
}

build_three_stage_changes <- function(three_stage_wide) {
  three_stage_wide %>%
    dplyr::filter(change_committee_2nd | change_2nd_3rd) %>%
    dplyr::mutate(
      where = dplyr::case_when(
        tidyr::replace_na(change_committee_2nd, FALSE) &
          tidyr::replace_na(change_2nd_3rd, FALSE) ~ "committee → 2nd & 2nd → 3rd",
        tidyr::replace_na(change_committee_2nd, FALSE) ~ "committee → 2nd",
        tidyr::replace_na(change_2nd_3rd, FALSE) ~ "2nd → 3rd"
      ),
      item_ref = short_item_ref(item_url)
    ) %>%
    dplyr::arrange(dplyr::desc(stage_date_second)) %>%
    dplyr::select(
      legis_period,
      item_ref,
      title,
      party,
      role,
      pos_committee,
      pos_second,
      pos_third,
      where,
      stage_date_second,
      item_url
    )
}

# Most recent committee → 2nd change, used to anchor the highlight callout.
build_latest_c2_item <- function(three_stage_wide) {
  three_stage_wide %>%
    dplyr::filter(tidyr::replace_na(change_committee_2nd, FALSE)) %>%
    dplyr::arrange(dplyr::desc(stage_date_second)) %>%
    dplyr::slice_head(n = 1)
}

build_latest_c2_parties <- function(three_stage_wide, latest_c2_item) {
  three_stage_wide %>%
    dplyr::semi_join(latest_c2_item, by = c("item_url")) %>%
    dplyr::filter(tidyr::replace_na(change_committee_2nd, FALSE)) %>%
    dplyr::distinct(party) %>%
    dplyr::pull(party) %>%
    paste(collapse = ", ")
}
