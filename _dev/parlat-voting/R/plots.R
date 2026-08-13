# Plot builders.
#
# The shared theme replaces the theme_minimal(...) + theme(...) block that was
# repeated verbatim in nine chunks, and the four flip-tile plots are now one
# parameterised builder driven by a spec table in _targets.R.

# ---- shared theme ------------------------------------------------------------

theme_post <- function(base_size = 11) {
  ggplot2::theme_minimal(base_size = base_size, base_family = FONT) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        face = "bold",
        size = 14,
        hjust = 0,
        margin = ggplot2::margin(b = 4)
      ),
      plot.subtitle = ggtext::element_textbox_simple(
        size = 10,
        family = FONT,
        colour = "grey30",
        lineheight = 1.3,
        margin = ggplot2::margin(b = 10)
      ),
      plot.caption = ggtext::element_textbox_simple(
        size = 8,
        family = FONT,
        colour = "grey45",
        hjust = 0,
        halign = 0,
        margin = ggplot2::margin(t = 10)
      ),
      plot.title.position = "plot",
      plot.caption.position = "plot"
    )
}

# Tile-plot flavour: no grid, small axis text, no legend.
theme_tile <- function(axis_text_size = 9) {
  theme_post() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(size = axis_text_size),
      legend.position = "none"
    )
}

# ---- flip tile plots (party x period) ----------------------------------------

# Count flips per party and period. NA arises when a party voted at only one of
# the two stages; treat that as no flip.
build_flip_counts <- function(votes_wide, from_col, to_col) {
  votes_wide %>%
    dplyr::mutate(
      is_flip = tidyr::replace_na(.data[[from_col]] == 1 & .data[[to_col]] == 1, FALSE)
    ) %>%
    # wt= turns count() into a weighted sum, effectively summing TRUE values
    dplyr::count(legis_period, party, wt = as.integer(is_flip), name = "flip_n")
}

# Full cross-product so every party × period cell exists, carrying the flags that
# distinguish "not in parliament" (white) from "zero flips" (grey).
build_flip_grid <- function(flip_counts,
                            all_parties,
                            gov_parties_lookup,
                            party_period_presence,
                            party_order) {
  tidyr::expand_grid(party = all_parties, legis_period = ALL_PERIODS) %>%
    dplyr::left_join(flip_counts, by = c("party", "legis_period")) %>%
    tidyr::replace_na(list(flip_n = 0)) %>%
    dplyr::left_join(gov_parties_lookup, by = c("party", "legis_period")) %>%
    tidyr::replace_na(list(is_gov = FALSE)) %>%
    dplyr::left_join(party_period_presence, by = c("party", "legis_period")) %>%
    tidyr::replace_na(list(present = FALSE)) %>%
    dplyr::mutate(
      party = factor(party, levels = rev(party_order)),
      legis_period = factor(legis_period, levels = ALL_PERIODS),
      label = dplyr::if_else(flip_n > 0, as.character(flip_n), ""),
      # NA fill_val -> white tile (not in parliament); 0 -> light grey (no flips)
      fill_val = dplyr::if_else(present, as.numeric(flip_n), NA_real_)
    )
}

flip_subtitle <- function(stage_from, stage_to, from_word, to_word, shift_phrase, contributes) {
  paste0(
    "Each cell shows how many items a party voted *", from_word, "* in the ", stage_from,
    " but *", to_word, "* in the ", stage_to, " — ", shift_phrase, ". ",
    "Colour intensity reflects count; white = party not represented in that period; ",
    "grey = no flips recorded; thick black frame = governing party.<br>",
    "<span style='font-size:9pt;color:grey40;'>",
    "**Scope & limits:** Legislative periods XX–XXVIII (NR). ",
    "Only items with a standard Dafür/Dagegen vote report are included; ",
    "votes recorded as *wechselnde Mehrheit*, *namentliche Abstimmung*, *getrennte Abstimmung*, ",
    "or other non-standard formats are excluded. ",
    contributes,
    "</span>"
  )
}

build_flip_tile_plot <- function(flip_grid, fill_hex, title, subtitle, caption) {
  # clamp lower bound to 1 so a single-flip period still gets a visible gradient
  n_max <- max(1, max(flip_grid$flip_n, na.rm = TRUE))

  ggplot2::ggplot(flip_grid, ggplot2::aes(x = legis_period, y = party, fill = fill_val)) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.6) +
    ggplot2::geom_tile(
      data = dplyr::filter(flip_grid, is_gov),
      mapping = ggplot2::aes(x = legis_period, y = party),
      fill = NA,
      colour = "black",
      linewidth = 0.4,
      inherit.aes = FALSE
    ) +
    ggplot2::geom_text(
      data = dplyr::filter(flip_grid, flip_n > 0),
      mapping = ggplot2::aes(x = legis_period, y = party, label = label),
      colour = "white",
      size = 3.2,
      fontface = "bold",
      family = FONT,
      inherit.aes = FALSE
    ) +
    ggplot2::scale_fill_gradient(
      low = PAL$tile_low,
      high = fill_hex,
      limits = c(0, n_max),
      na.value = "white", # white = party not present in that period
      guide = "none"
    ) +
    ggplot2::scale_x_discrete(position = "top") +
    ggplot2::labs(
      x = NULL,
      y = NULL,
      title = title,
      subtitle = subtitle,
      caption = caption
    ) +
    theme_tile()
}

# ---- missingness -------------------------------------------------------------

build_missingness_plot <- function(votes_wide) {
  naniar::vis_miss(votes_wide, facet = legis_period)
}

# ---- angle 1: per-period bars ------------------------------------------------

build_where_bars <- function(transitions, caption_short) {
  # at-risk denominator: only pairs that COULD flip in that direction, i.e. those
  # holding the prior position (no -> yes is only possible for prior "Dagegen")
  at_risk <- transitions %>%
    dplyr::mutate(
      direction = dplyr::if_else(from_pos == "Dagegen", "no → yes", "yes → no")
    ) %>%
    dplyr::count(legis_period, transition, role, direction, name = "n_at_risk")

  flip_dirs <- transitions %>%
    dplyr::filter(change %in% c("no → yes", "yes → no")) %>%
    dplyr::count(legis_period, transition, role, change, name = "n_flip") %>%
    dplyr::left_join(
      at_risk,
      by = c("legis_period", "transition", "role", "change" = "direction")
    ) %>%
    dplyr::mutate(
      rate = n_flip / n_at_risk,
      legis_period = factor(legis_period, levels = ALL_PERIODS)
    )

  ggplot2::ggplot(flip_dirs, ggplot2::aes(x = legis_period, y = rate, fill = transition)) +
    ggplot2::geom_col(
      position = ggplot2::position_dodge2(width = 0.9, preserve = "single"),
      width = 0.7
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = n_flip),
      position = ggplot2::position_dodge2(width = 0.9, preserve = "single"),
      vjust = -0.3,
      size = 2.5,
      family = FONT,
      colour = "grey35"
    ) +
    # drop = FALSE keeps the near-empty Government column visible: its emptiness is the point
    ggplot2::facet_grid(change ~ role, scales = "free_y", drop = FALSE) +
    ggplot2::scale_fill_manual(
      values = c(
        "Committee → 2nd reading" = PAL$blue,
        "2nd → 3rd reading" = PAL$orange
      ),
      name = NULL
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::percent_format(accuracy = 0.1),
      expand = ggplot2::expansion(mult = c(0, 0.12))
    ) +
    ggplot2::labs(
      x = NULL,
      y = "Conditional flip rate (of pairs at risk)",
      title = "Where do parties change their position?",
      subtitle = paste0(
        "Flip **rate** at each transition — committee → 2nd reading (blue) vs 2nd → 3rd reading (orange) — ",
        "by direction and party role. Bar labels show the absolute number of flips. ",
        "A high committee → 2nd bar means positions settled already in committee; ",
        "a high 2nd → 3rd bar means they shifted on the floor.<br>",
        "<span style='font-size:9pt;color:grey40;'>Rate is **conditional on the pairs that could flip**: ",
        "no → yes ÷ pairs voting *no* at the earlier stage; yes → no ÷ pairs voting *yes*. ",
        "NR, regular votes only, GP XX–XXVIII.</span>"
      ),
      caption = caption_short
    ) +
    theme_post() +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(size = 9),
      legend.position = "top",
      strip.text = ggplot2::element_text(face = "bold", size = 10)
    )
}

# ---- angle 2: alluvial -------------------------------------------------------

build_where_alluvial <- function(stage_positions, caption_short) {
  # only pairs observed at all three stages can flow across the full diagram
  alluvial_data <- stage_positions %>%
    dplyr::filter(!is.na(pos_committee), !is.na(pos_second), !is.na(pos_third)) %>%
    dplyr::count(role, pos_committee, pos_second, pos_third, name = "n")

  ggplot2::ggplot(
    alluvial_data,
    ggplot2::aes(axis1 = pos_committee, axis2 = pos_second, axis3 = pos_third, y = n)
  ) +
    ggalluvial::geom_alluvium(ggplot2::aes(fill = pos_committee), alpha = 0.7, width = 0.18) +
    ggalluvial::geom_stratum(width = 0.18, fill = "grey96", colour = "grey55") +
    ggplot2::geom_text(
      stat = "stratum",
      ggplot2::aes(label = ggplot2::after_stat(stratum)),
      size = 3,
      family = FONT
    ) +
    ggplot2::scale_x_discrete(
      limits = c("Committee", "2nd reading", "3rd reading"),
      expand = c(0.08, 0.08)
    ) +
    ggplot2::scale_fill_manual(
      values = c("Dafür" = PAL$green, "Dagegen" = PAL$red),
      name = "Position in committee"
    ) +
    ggplot2::facet_wrap(~role) +
    ggplot2::labs(
      x = NULL,
      y = "Party–item pairs",
      title = "How positions flow from committee to the third reading",
      subtitle = paste0(
        "Each band is a set of party–item pairs sharing the same committee / 2nd / 3rd reading positions; ",
        "a band crossing from one side to the other between two axes is a position change. ",
        "Colour = the party's position already in committee.<br>",
        "<span style='font-size:9pt;color:grey40;'>Only pairs with a recorded regular vote at all three stages ",
        "(NR, GP XX–XXVIII).</span>"
      ),
      caption = caption_short
    ) +
    theme_post() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(size = 9),
      axis.text.x = ggplot2::element_text(size = 10, face = "bold"),
      legend.position = "top",
      strip.text = ggplot2::element_text(face = "bold", size = 11)
    )
}

# ---- angle 3: split-tile heatmap (party x period x transition x direction) ----

build_where_tile_grid <- function(transitions,
                                  all_parties,
                                  gov_parties_lookup,
                                  party_period_presence,
                                  party_order) {
  flip_tiles <- transitions %>%
    dplyr::filter(change %in% c("no → yes", "yes → no")) %>%
    dplyr::count(legis_period, party, transition, change, name = "n_flip")

  tidyr::expand_grid(
    party = all_parties,
    legis_period = ALL_PERIODS,
    transition = TRANSITION_LEVELS,
    change = c("no → yes", "yes → no")
  ) %>%
    dplyr::left_join(
      flip_tiles %>% dplyr::mutate(transition = as.character(transition)),
      by = c("party", "legis_period", "transition", "change")
    ) %>%
    tidyr::replace_na(list(n_flip = 0)) %>%
    dplyr::left_join(gov_parties_lookup, by = c("party", "legis_period")) %>%
    tidyr::replace_na(list(is_gov = FALSE)) %>%
    dplyr::left_join(party_period_presence, by = c("party", "legis_period")) %>%
    tidyr::replace_na(list(present = FALSE)) %>%
    dplyr::mutate(
      party = factor(party, levels = rev(party_order)),
      legis_period = factor(legis_period, levels = ALL_PERIODS),
      transition = factor(transition, levels = TRANSITION_LEVELS),
      label = dplyr::if_else(n_flip > 0, as.character(n_flip), ""),
      fill_val = dplyr::if_else(present, as.numeric(n_flip), NA_real_),
      # negate yes-to-no counts so a single diverging scale can colour the two
      # direction facets differently (red = yes -> no, green = no -> yes)
      fill_val_signed = dplyr::if_else(change == "yes → no", -fill_val, fill_val)
    )
}

build_where_heatmap <- function(where_tile_grid, caption_short) {
  n_max <- max(1, max(where_tile_grid$n_flip, na.rm = TRUE))

  ggplot2::ggplot(
    where_tile_grid,
    ggplot2::aes(x = legis_period, y = party, fill = fill_val_signed)
  ) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.5) +
    ggplot2::geom_tile(
      data = dplyr::filter(where_tile_grid, is_gov),
      mapping = ggplot2::aes(x = legis_period, y = party),
      fill = NA,
      colour = "black",
      linewidth = 0.4,
      inherit.aes = FALSE
    ) +
    ggplot2::geom_text(
      data = dplyr::filter(where_tile_grid, n_flip > 0),
      mapping = ggplot2::aes(x = legis_period, y = party, label = label),
      colour = "white",
      size = 2.8,
      fontface = "bold",
      family = FONT,
      inherit.aes = FALSE
    ) +
    ggplot2::facet_grid(change ~ transition) +
    ggplot2::scale_fill_gradient2(
      low = PAL$red_dark,
      mid = PAL$tile_mid,
      high = PAL$green_dark,
      midpoint = 0,
      limits = c(-n_max, n_max),
      na.value = "white",
      guide = "none"
    ) +
    ggplot2::scale_x_discrete(position = "top") +
    ggplot2::labs(
      x = NULL,
      y = NULL,
      title = "Flip counts by party, period, transition and direction",
      subtitle = paste0(
        "Columns: where the flip occurs (committee → 2nd vs 2nd → 3rd). ",
        "Rows: direction (red = yes → no, green = no → yes). ",
        "Colour intensity = number of flips; white = party not represented; ",
        "thick black frame = governing party.<br>",
        "<span style='font-size:9pt;color:grey40;'>NR, regular votes only, GP XX–XXVIII.</span>"
      ),
      caption = caption_short
    ) +
    theme_tile(axis_text_size = 8) +
    ggplot2::theme(strip.text = ggplot2::element_text(face = "bold", size = 10))
}

# Bill-level share version: for each party and direction, the number of bills with
# a flip over all comparable bills at that transition and period.
build_where_tile_rate_grid <- function(where_tile_grid, transitions) {
  # count each comparable bill once per legislative period and transition
  transition_bill_totals <- transitions %>%
    dplyr::distinct(item_url, legis_period, transition) %>%
    dplyr::count(legis_period, transition, name = "n_bills")

  # every displayed transition-period must have a positive bill denominator
  stopifnot(
    nrow(transition_bill_totals) == length(ALL_PERIODS) * 2L,
    all(transition_bill_totals$n_bills > 0)
  )

  flip_bill_counts <- transitions %>%
    dplyr::filter(change %in% c("no → yes", "yes → no")) %>%
    dplyr::group_by(legis_period, party, transition, change) %>%
    dplyr::summarise(n_flip_bills = dplyr::n_distinct(item_url), .groups = "drop")

  rate_grid <- where_tile_grid %>%
    dplyr::select(-n_flip, -label, -fill_val, -fill_val_signed) %>%
    dplyr::left_join(
      flip_bill_counts,
      by = c("legis_period", "party", "transition", "change")
    ) %>%
    tidyr::replace_na(list(n_flip_bills = 0L)) %>%
    dplyr::left_join(transition_bill_totals, by = c("legis_period", "transition")) %>%
    dplyr::mutate(
      # re-apply the factor levels: joining a factor key against the character
      # columns in flip_bill_counts coerces party/legis_period back to character,
      # which silently loses the row ordering the tile plot depends on
      party = factor(party, levels = levels(where_tile_grid$party)),
      legis_period = factor(legis_period, levels = ALL_PERIODS),
      transition = factor(transition, levels = TRANSITION_LEVELS),
      rate = n_flip_bills / n_bills,
      label = dplyr::if_else(
        present & n_flip_bills > 0,
        scales::percent(rate, accuracy = 0.1),
        ""
      ),
      fill_rate = dplyr::if_else(present, rate, NA_real_),
      fill_rate_signed = dplyr::if_else(change == "yes → no", -fill_rate, fill_rate)
    )

  # denominators must be shared across parties/directions and bound every numerator
  denominator_check <- rate_grid %>%
    dplyr::group_by(legis_period, transition) %>%
    dplyr::summarise(n_denominators = dplyr::n_distinct(n_bills), .groups = "drop")

  stopifnot(
    all(denominator_check$n_denominators == 1L),
    all(rate_grid$n_flip_bills <= rate_grid$n_bills, na.rm = TRUE)
  )

  rate_max <- max(rate_grid$rate, na.rm = TRUE)

  rate_grid %>%
    dplyr::mutate(label_colour = dplyr::if_else(rate > rate_max / 2, "white", "grey15"))
}

build_where_heatmap_percent <- function(where_tile_rate_grid, caption_short) {
  rate_max <- max(where_tile_rate_grid$rate, na.rm = TRUE)

  ggplot2::ggplot(
    where_tile_rate_grid,
    ggplot2::aes(x = legis_period, y = party, fill = fill_rate_signed)
  ) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.5) +
    ggplot2::geom_tile(
      data = dplyr::filter(where_tile_rate_grid, is_gov),
      mapping = ggplot2::aes(x = legis_period, y = party),
      fill = NA,
      colour = "black",
      linewidth = 0.4,
      inherit.aes = FALSE
    ) +
    ggplot2::geom_text(
      data = dplyr::filter(where_tile_rate_grid, n_flip_bills > 0, present),
      mapping = ggplot2::aes(
        x = legis_period,
        y = party,
        label = label,
        colour = label_colour
      ),
      size = 2.8,
      fontface = "bold",
      family = FONT,
      inherit.aes = FALSE
    ) +
    ggplot2::facet_grid(change ~ transition) +
    ggplot2::scale_colour_identity() +
    ggplot2::scale_fill_gradient2(
      low = PAL$red_dark,
      mid = PAL$tile_mid,
      high = PAL$green_dark,
      midpoint = 0,
      limits = c(-rate_max, rate_max),
      labels = scales::percent_format(accuracy = 0.1),
      na.value = "white",
      guide = "none"
    ) +
    ggplot2::scale_x_discrete(position = "top") +
    ggplot2::labs(
      x = NULL,
      y = NULL,
      title = "Share of bills with a party vote flip",
      subtitle = paste0(
        "Each tile shows the share of comparable bills on which a party changes position ",
        "(red = yes → no, green = no → yes). ",
        "The denominator is all unique bills with usable votes at both stages of that transition and period, ",
        "shared across parties and directions; percentages do not sum to 100%. ",
        "White = party not represented; grey = no flips; thick black frame = governing party.<br>",
        "<span style='font-size:9pt;color:grey40;'>NR, regular votes only, GP XX–XXVIII.</span>"
      ),
      caption = caption_short
    ) +
    theme_tile(axis_text_size = 8) +
    ggplot2::theme(strip.text = ggplot2::element_text(face = "bold", size = 10))
}

# ---- angle 4: stage x period ------------------------------------------------

build_flip_stage_period <- function(transitions, gov_parties_lookup, caption_short) {
  flip_stage_period <- transitions %>%
    dplyr::group_by(legis_period, transition) %>%
    dplyr::summarise(
      eligible = dplyr::n(),
      n_flip = sum(change %in% c("no → yes", "yes → no")),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      rate = n_flip / eligible,
      legis_period = factor(legis_period, levels = rev(ALL_PERIODS)),
      # white text on dark tiles, dark text on light tiles
      lab_colour = dplyr::if_else(rate > max(rate) / 2, "white", "grey15")
    )

  # right-hand axis listing each period's coalition
  gov_axis_labels <- gov_parties_lookup %>%
    dplyr::group_by(legis_period) %>%
    dplyr::summarise(gov_parties = paste(party, collapse = ", "), .groups = "drop") %>%
    tibble::deframe()

  ggplot2::ggplot(
    flip_stage_period,
    ggplot2::aes(x = transition, y = legis_period, fill = rate)
  ) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.9) +
    ggplot2::geom_text(
      ggplot2::aes(
        label = scales::percent(rate, accuracy = 0.1),
        colour = lab_colour
      ),
      family = FONT,
      size = 3.3
    ) +
    ggplot2::scale_colour_identity() +
    ggplot2::scale_fill_gradient(
      low = PAL$tile_low_2,
      high = PAL$purple,
      labels = scales::percent_format(accuracy = 1),
      name = "Flip rate"
    ) +
    ggplot2::scale_x_discrete(position = "top") +
    ggplot2::scale_y_discrete(
      breaks = levels(flip_stage_period$legis_period),
      sec.axis = ggplot2::dup_axis(
        labels = function(x) rev(unname(gov_axis_labels[x])),
        name = NULL
      )
    ) +
    ggplot2::labs(
      x = NULL,
      y = NULL,
      title = "Relative number of flips by stage and legislative period",
      subtitle = paste0(
        "Tile colour = share of comparable party–item pairs that change position at each transition ",
        "(both directions combined, all parties). The committee → 2nd reading step carries almost all ",
        "of the movement.<br>",
        "<span style='font-size:9pt;color:grey40;'>NR, regular votes only.</span>"
      ),
      caption = caption_short
    ) +
    theme_post() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(size = 10),
      axis.text.x = ggplot2::element_text(face = "bold"),
      axis.text.y.right = ggplot2::element_text(size = 9, colour = "grey30"),
      axis.title.y.right = ggplot2::element_blank(),
      legend.position = "right",
      plot.margin = ggplot2::margin(t = 8, r = 12, b = 8, l = 8)
    )
}
