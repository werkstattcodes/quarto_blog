# From the cached API fetches to the scoped, deduplicated set of vote stages.
#
# Each reader takes a path and reads the .rds inside the target command, so the
# ~218 MB of raw fetches never enter _targets/objects/ — only the much smaller
# derived frames are stored.

# ---- raw readers -------------------------------------------------------------

# items_20_28.rds is ~108 MB but only four columns are ever needed downstream
# (the item-type scope diagnostic and the caption's type labels), so it is slimmed
# on the way in.
read_items_meta <- function(path) {
  readr::read_rds(path) %>%
    dplyr::distinct(item_url, legis_period, type_doc, type_doc_long)
}

# Items whose type can carry a reading vote. Note the unanchored RV/GABR patterns
# also match variants (RVEU, RVS, GABR13, …); those pass the type filter but
# rarely carry a standard reading vote and drop out downstream. The graph caption
# is therefore built later, from the types that actually survive.
build_items_in_scope_types <- function(items_meta) {
  items_meta %>%
    dplyr::filter(stringr::str_detect(type_doc, stringr::regex(item_type_regex())))
}

# Items where the API call failed. Diagnostic only — nothing downstream depends
# on it, but it is what tells you a cache refresh went wrong.
read_failed_items <- function(path) {
  readr::read_rds(path) %>%
    purrr::keep(\(x) !is.null(x$error)) %>%
    purrr::map(\(x) {
      tibble::tibble(item_url = x$item_url, error = conditionMessage(x$error))
    }) %>%
    purrr::list_rbind()
}

# One row per stage per item. The API's `votes` list-column is deliberately NOT
# carried through: the original post selected it here and never read it again,
# and it inflates the stored object considerably.
read_item_stages <- function(path) {
  readr::read_rds(path) %>%
    purrr::keep(\(x) is.null(x$error)) %>%
    purrr::map("result") %>%
    purrr::list_rbind() %>%
    dplyr::select(
      item_url,
      legis_period,
      type_doc,
      title,
      item_number,
      status_number,
      status_description,
      stages
    ) %>%
    dplyr::filter(purrr::map_lgl(stages, \(x) !is.null(x))) %>% # drop items with no stage data
    tidyr::unnest(stages) %>%
    dplyr::mutate(stage_date = lubridate::dmy(stage_date))
}

# Committee reports (Ausschussbericht). Older caches may predate the normalised
# item_url column, so the requested URL is back-filled when absent.
read_aub_details <- function(path) {
  raw <- readr::read_rds(path)

  success_n <- sum(purrr::map_lgl(raw, \(x) is.null(x$error)))
  if (success_n == 0) {
    stop(
      "The AUB cache contains ", length(raw),
      " failed requests and no usable committee-report data. ",
      "Re-run refresh_data.R to rebuild it. First cached error: ",
      conditionMessage(raw[[1]]$error),
      call. = FALSE
    )
  }

  raw %>%
    purrr::keep(\(x) is.null(x$error)) %>%
    purrr::map(\(x) {
      result <- x$result
      if (!"item_url" %in% names(result)) {
        result$item_url <- x$item_url
      }
      result
    }) %>%
    purrr::list_rbind()
}

# ---- committee votes ---------------------------------------------------------

# For most bills the committee vote is not on the bill's own page at all: it lives
# on the committee report, which carries the formal adoption division plus a
# reference back to the parent bill. This rebuilds one committee row per bill.
build_committee_rows <- function(aub_details, item_stages) {
  # link each report to its parent bill via the reference flagged "HG" (Hauptgegenstand)
  aub_parent <- aub_details %>%
    dplyr::transmute(
      aub_url = item_url,
      parent_url = purrr::map_chr(references, \(r) {
        if (is.null(r) || !is.data.frame(r) || nrow(r) == 0) {
          return(NA_character_)
        }
        hg <- r[!is.na(r$art) & r$art == "HG", , drop = FALSE]
        if (nrow(hg) == 0) {
          return(NA_character_)
        }
        paste0("https://www.parlament.gv.at", hg$url[1])
      })
    )

  aub_committee_stages <- aub_details %>%
    dplyr::select(aub_url = item_url, stages) %>%
    dplyr::filter(purrr::map_lgl(stages, \(x) !is.null(x))) %>%
    tidyr::unnest(stages) %>%
    dplyr::filter(stringr::str_detect(
      stage_name,
      stringr::regex("Antrag auf Annahme des", ignore_case = TRUE)
    )) %>%
    dplyr::mutate(stage_date = lubridate::dmy(stage_date)) %>%
    dplyr::left_join(aub_parent, by = "aub_url") %>%
    dplyr::filter(!is.na(parent_url))

  # parent bill's item-level fields, so committee rows line up with reading rows
  parent_meta <- item_stages %>%
    dplyr::distinct(
      item_url,
      legis_period,
      type_doc,
      title,
      item_number,
      status_number,
      status_description
    )

  aub_committee_stages %>%
    dplyr::inner_join(parent_meta, by = c("parent_url" = "item_url")) %>%
    dplyr::transmute(
      item_url = parent_url,
      legis_period,
      type_doc,
      title,
      item_number,
      status_number,
      status_description,
      stage_date,
      stage_name,
      phase = "Ausschussbericht",
      reading = "committee"
    ) %>%
    # keep the latest adoption vote if a report records several
    dplyr::slice_max(stage_date, n = 1, by = item_url, with_ties = FALSE)
}

# ---- what the bill's own page records about its committee phase --------------

build_committee_stages <- function(item_stages) {
  item_stages %>%
    dplyr::filter(phase == "Ausschussberatungen NR") %>%
    dplyr::mutate(
      is_vote = stringr::str_detect(
        stage_name,
        stringr::regex(VOTE_KEYWORDS, ignore_case = TRUE)
      ),
      category = dplyr::case_when(
        stringr::str_detect(
          stage_name,
          stringr::regex("Antrag auf Annahme des", ignore_case = TRUE)
        ) ~ "Adoption vote on the bill",
        stringr::str_detect(
          stage_name,
          stringr::regex("Einholung einer Stellungnahme", ignore_case = TRUE)
        ) ~ "Opinion request (Stellungnahme)",
        stringr::str_detect(
          stage_name,
          stringr::regex("Fristsetzung", ignore_case = TRUE)
        ) ~ "Deadline motion (Fristsetzung)",
        is_vote ~ "Other vote",
        .default = "Non-vote / procedural"
      ),
      # only the adoption vote is carried into the analysis by the scope filter
      in_scope = category == "Adoption vote on the bill"
    )
}

build_committee_category_summary <- function(committee_stages) {
  committee_stages %>%
    dplyr::summarise(rows = dplyr::n(), .by = c(category, in_scope)) %>%
    dplyr::mutate(
      share = rows / sum(rows),
      in_analysis_scope = dplyr::if_else(in_scope, "Yes", "No")
    ) %>%
    dplyr::arrange(dplyr::desc(rows)) %>%
    dplyr::select(category, rows, share, in_analysis_scope)
}

build_committee_vote_rows <- function(committee_stages) {
  committee_stages %>%
    dplyr::filter(is_vote) %>%
    dplyr::mutate(
      item_ref = short_item_ref(item_url),
      in_analysis_scope = dplyr::if_else(in_scope, "Yes", "No")
    ) %>%
    dplyr::select(
      legis_period,
      item_ref,
      stage_date,
      stage_name,
      category,
      in_analysis_scope,
      item_url
    ) %>%
    dplyr::arrange(dplyr::desc(stage_date))
}

# Strip base URL and query string to a compact "XXVIII/I/403" style reference.
short_item_ref <- function(item_url) {
  item_url %>%
    stringr::str_remove("^https://www\\.parlament\\.gv\\.at/gegenstand/") %>%
    stringr::str_remove("\\?.*$")
}

# ---- scope filter ------------------------------------------------------------

# Second/third-reading rows from the bills' own NR stages, with the committee
# votes rebuilt from the reports bound on, narrowed to rows that record a vote.
#
# Named `stages_scope` rather than `items_scope`: in the original post that name
# was assigned twice from two unrelated lineages, and the second silently shadowed
# the first.
build_stages_scope <- function(item_stages, committee_rows) {
  item_stages %>%
    dplyr::filter(stringr::str_detect(phase, stringr::regex("NR"))) %>%
    # r? matches both "zweite"/"zweiter" and "dritte"/"dritter"
    dplyr::filter(stringr::str_detect(stage_name, "(dritter?|zweiter?) Lesung")) %>%
    dplyr::mutate(
      reading = dplyr::case_when(
        # some items combine both readings into a single stage entry
        stringr::str_detect(stage_name, stringr::regex("zweite")) &
          stringr::str_detect(stage_name, stringr::regex("dritte")) ~ "second & third",
        stringr::str_detect(stage_name, stringr::regex("zweite")) ~ "second",
        stringr::str_detect(stage_name, stringr::regex("dritte")) ~ "third",
        .default = NA_character_
      )
    ) %>%
    dplyr::bind_rows(committee_rows) %>%
    dplyr::filter(stringr::str_detect(
      stage_name,
      stringr::regex(VOTE_KEYWORDS, ignore_case = TRUE)
    )) %>%
    dplyr::mutate(
      # how the vote result is reported. Order matters: "regular" must come first
      # because some stage names contain both "dafür" and other keywords.
      vote_report_type = dplyr::case_when(
        # "regular" requires the explicit Dafür:/Dagegen: separator the parser keys
        # on; parenthetical forms like "mehrstimmig (dafür S,V dagegen F)" fall through
        stringr::str_detect(stage_name, stringr::regex("dafür", ignore_case = TRUE)) &
          stringr::str_detect(stage_name, stringr::regex("dagegen", ignore_case = TRUE)) &
          stringr::str_detect(
            stage_name,
            stringr::regex(",\\s*dagegen:|\\.\\s*Dagegen:", ignore_case = TRUE)
          ) ~ "regular",
        stringr::str_detect(
          stage_name,
          stringr::regex("wechselnde Mehrheit", ignore_case = TRUE)
        ) ~ "wechselnde Mehrheit",
        stringr::str_detect(
          stage_name,
          stringr::regex("namentliche Abstimmung", ignore_case = TRUE)
        ) ~ "namentliche Abstimmung",
        stringr::str_detect(
          stage_name,
          stringr::regex("getrennte Abstimmung", ignore_case = TRUE)
        ) ~ "getrennte Abstimmung",
        stringr::str_detect(
          stage_name,
          stringr::regex("abgelehnt", ignore_case = TRUE)
        ) ~ "abgelehnt",
        stringr::str_detect(
          stage_name,
          stringr::regex("mehrstimmig", ignore_case = TRUE)
        ) ~ "mehrstimmig",
        .default = NA
      )
    )
}

# Each item should have at most one row per stage. More than that means a source
# duplicate, or an item approved/rejected twice within one stage.
build_dup_rows <- function(stages_scope) {
  stages_scope %>%
    dplyr::count(legis_period, item_url, reading, name = "n_rows") %>%
    dplyr::filter(n_rows > 1)
}

build_non_pairs_details <- function(stages_scope, dup_rows) {
  stages_scope %>%
    dplyr::semi_join(dup_rows, by = c("legis_period", "item_url")) %>%
    dplyr::distinct() %>%
    dplyr::select(legis_period, item_url, stage_date, stage_name, vote_report_type) %>%
    dplyr::mutate(n_obs = dplyr::n(), .by = c("legis_period", "item_url")) %>%
    dplyr::arrange(dplyr::desc(n_obs), legis_period, item_url, stage_date) %>%
    dplyr::mutate(item_ref = short_item_ref(item_url)) %>%
    dplyr::select(
      legis_period,
      item_ref,
      stage_date,
      stage_name,
      vote_report_type,
      n_obs,
      item_url
    )
}

# Drop items with duplicate rows within a stage, keep only "regular" vote reports,
# then re-check: the type filter can itself leave residual duplicates.
build_votes_regular <- function(stages_scope, dup_rows) {
  regular <- stages_scope %>%
    dplyr::anti_join(dup_rows, by = c("legis_period", "item_url")) %>%
    dplyr::filter(vote_report_type == "regular")

  dup_rows2 <- regular %>%
    dplyr::count(legis_period, item_url, reading, name = "n_rows") %>%
    dplyr::filter(n_rows > 1)

  regular %>%
    dplyr::anti_join(dup_rows2, by = c("legis_period", "item_url"))
}
