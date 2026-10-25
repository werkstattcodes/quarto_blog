# Parsing party positions out of the stage text, and reshaping to long/wide.

# Split a raw comma-separated party string into a clean character vector.
clean_parties <- function(x) {
  if (is.na(x) || stringr::str_trim(x) == "-") {
    return(character(0))
  }

  x %>%
    stringr::str_split(",\\s*") %>%
    unlist() %>%
    stringr::str_trim() %>%
    stringr::str_remove("\\)+$") %>% # strip trailing parens, e.g. "SPÖ (Mehrheit)" -> "SPÖ"
    stringr::str_trim() %>%
    # keep only all-caps tokens (party abbreviations); drops vote counts, notes, etc.
    purrr::keep(\(p) {
      stringr::str_detect(p, stringr::regex("^[[:upper:]ÄÖÜ]+(?:[-/][[:upper:]ÄÖÜ]+)*$"))
    }) %>%
    purrr::keep(\(p) p %in% valid_parties()) # final guard against unknown tokens
}

# Parse one stage description into the parties for and against.
# Text follows (mostly) "Dafür: party A, party B, dagegen: party C".
fn_get_votes <- function(vt) {
  # some stage texts use ". Dagegen:" instead of ", dagegen:" — normalise so the
  # split below works in both cases
  vt_norm <- stringr::str_replace(
    vt,
    stringr::regex("\\.\\s*Dagegen:", ignore_case = TRUE),
    ", dagegen:"
  )

  # n = 2 prevents splitting inside the party lists
  parts <- stringr::str_split(
    vt_norm,
    stringr::regex(",\\s*dagegen:", ignore_case = TRUE),
    n = 2
  )[[1]]

  if (length(parts) != 2) {
    stop("Regular vote text does not contain both Dafür and Dagegen parts.", call. = FALSE)
  }

  if (!stringr::str_detect(parts[1], stringr::regex("Dafür:", ignore_case = TRUE))) {
    stop("Regular vote text does not contain a Dafür part.", call. = FALSE)
  }

  # strip the "Dafür:" label and everything before it (dotall handles multi-line stage names)
  yes_raw <- parts[1] %>%
    stringr::str_remove(stringr::regex("^.*Dafür:\\s*", dotall = TRUE, ignore_case = TRUE))

  list(
    vote_yes = clean_parties(yes_raw),
    vote_no = clean_parties(parts[2])
  )
}

# `votes_regular` gains a list column of parsed positions. Kept separate from the
# long form because the original post mutated the same object across two chunks.
build_votes_parsed <- function(votes_regular) {
  votes_regular %>%
    dplyr::mutate(votes_li = purrr::map(stage_name, fn_get_votes))
}

# One row per item × reading × party, with yes/no indicator columns.
build_votes_long <- function(votes_parsed) {
  votes_parsed %>%
    dplyr::mutate(
      votes_party = purrr::map(votes_li, \(v) {
        yes_parties <- normalise_party(v$vote_yes)
        no_parties <- normalise_party(v$vote_no)

        # union ensures each party appears once per stage even if listed on both sides
        tibble::tibble(
          party = union(yes_parties, no_parties),
          yes = as.integer(party %in% yes_parties),
          no = as.integer(party %in% no_parties)
        )
      })
    ) %>%
    tidyr::unnest(votes_party) %>%
    dplyr::select(
      item_url,
      legis_period,
      type_doc,
      title,
      item_number,
      status_number,
      status_description,
      stage_date,
      stage_name,
      reading,
      vote_report_type,
      party,
      yes,
      no
    ) %>%
    dplyr::group_by(dplyr::across(-c(yes, no))) %>%
    # collapse duplicate rows (deduplication edge cases) by taking the max vote value
    dplyr::summarise(
      yes = max(yes, na.rm = TRUE),
      no = max(no, na.rm = TRUE),
      .groups = "drop"
    )
}

# One row per item × party, with a yes_/no_ column per stage so flips between
# stages can be read off directly. NA means no recorded vote at that stage.
build_votes_wide <- function(votes_long) {
  votes_long %>%
    # drop "second & third" combined rows; keep committee + clean individual readings
    dplyr::filter(reading %in% c("committee", "second", "third")) %>%
    dplyr::filter(vote_report_type == "regular") %>%
    dplyr::select(
      item_url,
      legis_period,
      type_doc,
      title,
      item_number,
      status_number,
      status_description,
      party,
      reading,
      vote_report_type,
      yes,
      no
    ) %>%
    tidyr::pivot_wider(
      id_cols = c(
        item_url,
        legis_period,
        type_doc,
        title,
        item_number,
        status_number,
        status_description,
        party
      ),
      names_from = reading,
      values_from = c(vote_report_type, yes, no),
      names_glue = "{.value}_{reading}",
      values_fill = list(
        vote_report_type = NA_character_,
        yes = NA_integer_,
        no = NA_integer_
      )
    )
}

# ---- captions ----------------------------------------------------------------

# List only the item types that actually survive into the analysis: the type
# filter upstream is broader, but several variants never carry a reading vote.
build_analysed_type_labels <- function(votes_wide, items_meta) {
  votes_wide %>%
    dplyr::distinct(type_doc) %>%
    dplyr::left_join(
      dplyr::distinct(items_meta, type_doc, type_doc_long),
      by = "type_doc"
    ) %>%
    dplyr::arrange(type_doc) %>%
    dplyr::mutate(label = paste0(type_doc_long, " (", type_doc, ")")) %>%
    dplyr::pull(label) %>%
    paste(collapse = ", ")
}

build_vote_graph_caption <- function(analysed_type_labels, data_cutoff) {
  paste0(
    "Item types included ('Verhandlungsgegenstände'): ",
    analysed_type_labels,
    "<br>Data: www&#46;parlament.gv.at via ParlAT package, as of ",
    data_cutoff,
    "<br>Graphic: Roland Schmidt, https&#58;//werk.statt.codes | Bluesky: @zoowalk.bsky.social"
  )
}

# The full item-list caption is too tall for the denser multi-facet plots.
build_vote_graph_caption_short <- function(data_cutoff) {
  paste0(
    "Data: www&#46;parlament.gv.at via ParlAT package, as of ",
    data_cutoff,
    " · Graphic: Roland Schmidt, https&#58;//werk.statt.codes · NR, regular votes only (GP XX–XXVIII)"
  )
}
