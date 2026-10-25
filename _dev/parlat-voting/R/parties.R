# Party name normalisation, government lookup and display ordering.
#
# The ordering helpers used to live inside the `plot-flip-no-to-yes` chunk, which
# made a plot the de-facto definition site for five constants that eight other
# chunks depended on. They are plain functions here and separate targets in the
# pipeline.

# Party labels as they appear in the stage text, including historical aliases
# (F, NEOS-LIF, F-BZÖ and the single-letter S/V/N/G forms). Used as an allowlist
# so vote counts, footnotes and other non-party tokens cannot leak through.
valid_parties <- function() {
  c(
    "SPÖ",
    "ÖVP",
    "F",
    "FPÖ",
    "GRÜNE",
    "L",
    "BZÖ",
    "STRONACH",
    "NEOS",
    "NEOS-LIF",
    "F-BZÖ",
    "JETZT",
    "PILZ",
    "NEOS/NEOS-LIF",
    "JETZT/PILZ",
    "S",
    "V",
    "N",
    "G"
  )
}

# Maps every historical/transitional label onto one canonical name per party.
# Without the single-letter cases the tile plots grow phantom rows: "G" would sit
# next to "GRÜNE" as if they were different parties.
normalise_party <- function(party) {
  dplyr::case_when(
    party %in% c("NEOS-LIF", "NEOS/NEOS-LIF") ~ "NEOS",
    party %in% c("JETZT", "PILZ", "JETZT/PILZ") ~ "JETZT/PILZ",
    party == "F" ~ "FPÖ",
    party == "F-BZÖ" ~ "BZÖ",
    party == "S" ~ "SPÖ",
    party == "V" ~ "ÖVP",
    party == "N" ~ "NEOS",
    party == "G" ~ "GRÜNE",
    TRUE ~ party
  )
}

# Hand-coded coalition membership per legislative period (GP XX–XXVIII).
build_gov_parties_lookup <- function() {
  tibble::tribble(
    ~legis_period , ~party ,
    "XX"          , "SPÖ"  , "XX"     , "ÖVP"   ,
    "XXI"         , "ÖVP"  , "XXI"    , "F"     ,
    "XXII"        , "ÖVP"  , "XXII"   , "F"     , "XXII"   , "F-BZÖ" ,
    "XXIII"       , "SPÖ"  , "XXIII"  , "ÖVP"   ,
    "XXIV"        , "SPÖ"  , "XXIV"   , "ÖVP"   ,
    "XXV"         , "SPÖ"  , "XXV"    , "ÖVP"   ,
    "XXVI"        , "ÖVP"  , "XXVI"   , "FPÖ"   ,
    "XXVII"       , "ÖVP"  , "XXVII"  , "GRÜNE" ,
    "XXVIII"      , "ÖVP"  , "XXVIII" , "SPÖ"   , "XXVIII" , "NEOS"
  ) %>%
    dplyr::mutate(party = normalise_party(party)) %>% # align aliases before joining
    dplyr::mutate(is_gov = TRUE)
}

# Which party × period combinations actually appear in the data. Downstream this
# distinguishes "not in parliament" (white tile) from "zero flips" (grey tile).
build_party_period_presence <- function(votes_wide) {
  votes_wide %>%
    dplyr::distinct(legis_period, party) %>%
    dplyr::mutate(present = TRUE)
}

build_all_parties <- function(party_period_presence, gov_parties_lookup) {
  union(party_period_presence$party, gov_parties_lookup$party)
}

# Fixed top order, then the remaining parties by how many periods they sit in.
build_party_order <- function(party_period_presence, all_parties) {
  trailing <- party_period_presence %>%
    dplyr::distinct(party, legis_period) %>%
    dplyr::count(party, name = "periods_present") %>%
    dplyr::right_join(tibble::tibble(party = all_parties), by = "party") %>%
    dplyr::mutate(periods_present = tidyr::replace_na(periods_present, 0L)) %>%
    dplyr::filter(!party %in% PARTY_ORDER_PRIORITY) %>%
    dplyr::arrange(dplyr::desc(periods_present), party) %>%
    dplyr::pull(party)

  unique(c(
    PARTY_ORDER_PRIORITY,
    trailing,
    setdiff(PARTY_ORDER_PRIORITY, all_parties)
  ))
}
