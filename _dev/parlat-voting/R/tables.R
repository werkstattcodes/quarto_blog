# Reactable builders.
#
# These are functions rather than targets: htmlwidgets are cheap to rebuild and
# awkward to serialise, so the pipeline stores the table *data* and the qmd calls
# these to render. Each cell renderer closes over the frame passed in, which is
# why the data argument is named consistently.

table_theme <- function() {
  reactablefmtr::fivethirtyeight(font_size = 12)
}

table_source <- function() {
  reactablefmtr::html(paste0(
    "<span style='font-size:8pt;color:grey30;font-family:Segoe UI !important;line-height:0.5'>",
    "Source: www.parlament.gv.at. Analysis: Roland Schmidt - @zoowalk.bsky.social - ",
    "https://werk.statt.codes</span>"
  ))
}

# Consistent title, optional subtitle and source footer for every table.
style_reactable_table <- function(table, title, subtitle = NULL) {
  styled_table <- table %>%
    reactablefmtr::add_title(
      title = reactablefmtr::html(paste0(
        "<span style='font-size:12pt;'>", title, "</span>"
      ))
    )

  if (!is.null(subtitle)) {
    styled_table <- styled_table %>%
      reactablefmtr::add_subtitle(
        subtitle = reactablefmtr::html(paste0(
          "<span style='font-size:10pt;line-height:0.5;'>", subtitle, "</span>"
        )),
        font_color = "grey30",
        font_weight = "normal",
        font_style = "italic"
      )
  }

  styled_table %>%
    reactablefmtr::add_source(source = table_source())
}

# Turn a short item reference into a link back to parlament.gv.at. `urls` is the
# item_url column of the same frame, aligned by row index.
link_cell <- function(urls) {
  function(value, index) {
    htmltools::tags$a(href = urls[index], target = "_blank", value)
  }
}

# ---- individual tables -------------------------------------------------------

build_committee_category_table <- function(committee_category_summary, data_cutoff) {
  reactable::reactable(
    committee_category_summary,
    columns = list(
      category = reactable::colDef(name = "Committee stage type", minWidth = 230),
      rows = reactable::colDef(
        name = "Stages",
        align = "right",
        minWidth = 90,
        format = reactable::colFormat(separators = TRUE)
      ),
      share = reactable::colDef(
        name = "Share of committee stages",
        minWidth = 230,
        align = "left",
        # data bars make the tiny vote share read at a glance against the procedural bulk
        cell = reactablefmtr::data_bars(
          committee_category_summary,
          max_value = 1,
          number_fmt = scales::label_percent(accuracy = 0.1),
          fill_color = PAL$green,
          background = "#f0f0f0",
          text_position = "outside-end"
        )
      ),
      in_analysis_scope = reactable::colDef(
        name = "Adoption vote?",
        align = "center",
        minWidth = 130
      )
    ),
    fullWidth = TRUE,
    compact = TRUE,
    highlight = TRUE,
    outlined = TRUE,
    theme = table_theme()
  ) %>%
    style_reactable_table(
      "WHAT THE BILL'S OWN PAGE RECORDS ABOUT ITS COMMITTEE PHASE",
      paste0(
        "All committee stages recorded on the bills' own pages, by type — the adoption vote is ",
        "the rarest of them. Data as of ", data_cutoff, "."
      )
    )
}

build_committee_vote_rows_table <- function(committee_vote_rows, data_cutoff) {
  reactable::reactable(
    committee_vote_rows,
    columns = list(
      legis_period = reactable::colDef(
        name = "Legis. period",
        align = "right",
        minWidth = 90,
        filterable = TRUE
      ),
      item_ref = reactable::colDef(
        name = "Item",
        minWidth = 120,
        filterable = TRUE,
        cell = link_cell(committee_vote_rows$item_url)
      ),
      stage_date = reactable::colDef(
        name = "Stage date",
        align = "left",
        minWidth = 110,
        filterable = TRUE
      ),
      stage_name = reactable::colDef(
        name = "Stage",
        minWidth = 460,
        filterable = TRUE,
        style = list(whiteSpace = "normal", lineHeight = "1.35")
      ),
      category = reactable::colDef(name = "Type", minWidth = 180, filterable = TRUE),
      in_analysis_scope = reactable::colDef(
        name = "Adoption vote?",
        align = "center",
        minWidth = 100,
        filterable = TRUE
      ),
      item_url = reactable::colDef(show = FALSE) # kept for the renderer, hidden from display
    ),
    fullWidth = TRUE,
    compact = TRUE,
    highlight = FALSE,
    outlined = TRUE,
    searchable = TRUE,
    defaultPageSize = 10,
    theme = table_theme()
  ) %>%
    style_reactable_table(
      "COMMITTEE STAGES THAT RECORD A VOTE",
      paste0(
        "All 'Ausschussberatungen NR' stages containing a vote keyword. 'In scope?' marks the ",
        "bill-adoption votes used in the analysis. Data as of ", data_cutoff, "."
      )
    )
}

build_non_pairs_table <- function(non_pairs_details, data_cutoff) {
  reactable::reactable(
    non_pairs_details,
    columns = list(
      legis_period = reactable::colDef(
        name = "Legis. period",
        align = "right",
        minWidth = 90,
        filterable = TRUE
      ),
      item_ref = reactable::colDef(
        name = "Item",
        minWidth = 125,
        filterable = TRUE,
        cell = link_cell(non_pairs_details$item_url)
      ),
      stage_date = reactable::colDef(
        name = "Stage date",
        align = "left",
        minWidth = 110,
        filterable = TRUE
      ),
      stage_name = reactable::colDef(
        name = "Stage",
        minWidth = 540,
        filterable = TRUE,
        style = list(whiteSpace = "normal", lineHeight = "1.35")
      ),
      vote_report_type = reactable::colDef(
        name = "Vote report type",
        minWidth = 155,
        filterable = TRUE
      ),
      n_obs = reactable::colDef(
        name = "Rows per item",
        align = "right",
        minWidth = 130,
        format = reactable::colFormat(digits = 0)
      ),
      item_url = reactable::colDef(show = FALSE)
    ),
    fullWidth = TRUE,
    compact = TRUE,
    highlight = FALSE,
    outlined = TRUE,
    searchable = TRUE,
    defaultPageSize = 10,
    defaultSorted = "n_obs",
    defaultSortOrder = "desc",
    theme = table_theme()
  ) %>%
    style_reactable_table(
      "ITEMS WITH DUPLICATE VOTE ROWS IN A STAGE",
      paste0(
        "Items with more than one vote row for the same stage (committee, second or third ",
        "reading). Data as of ", data_cutoff, "."
      )
    )
}

# Shared colour styling for the three position columns.
pos_style <- function(value) {
  if (is.na(value)) {
    return(list(color = "#bbb"))
  }
  list(
    color = if (value == "Dafür") PAL$pos_yes else PAL$pos_no,
    fontWeight = "600"
  )
}

build_three_stage_changes_table <- function(three_stage_changes, data_cutoff) {
  reactable::reactable(
    three_stage_changes,
    columns = list(
      legis_period = reactable::colDef(
        name = "GP",
        align = "right",
        minWidth = 60,
        filterable = TRUE
      ),
      item_ref = reactable::colDef(
        name = "Item",
        minWidth = 110,
        filterable = TRUE,
        cell = link_cell(three_stage_changes$item_url)
      ),
      title = reactable::colDef(
        name = "Title",
        minWidth = 240,
        filterable = TRUE,
        style = list(whiteSpace = "normal", lineHeight = "1.3")
      ),
      party = reactable::colDef(name = "Party", minWidth = 80, filterable = TRUE),
      role = reactable::colDef(name = "Role", minWidth = 110, filterable = TRUE),
      pos_committee = reactable::colDef(name = "Committee", minWidth = 95, style = pos_style),
      pos_second = reactable::colDef(name = "2nd reading", minWidth = 95, style = pos_style),
      pos_third = reactable::colDef(name = "3rd reading", minWidth = 95, style = pos_style),
      where = reactable::colDef(name = "Change at", minWidth = 150, filterable = TRUE),
      stage_date_second = reactable::colDef(
        name = "2nd reading date",
        minWidth = 130,
        filterable = TRUE
      ),
      item_url = reactable::colDef(show = FALSE)
    ),
    fullWidth = TRUE,
    compact = TRUE,
    highlight = TRUE,
    outlined = TRUE,
    searchable = TRUE,
    defaultPageSize = 12,
    theme = table_theme()
  ) %>%
    style_reactable_table(
      "POSITION CHANGES AMONG ITEMS DECIDED AT ALL THREE STAGES",
      paste0(
        "Every party–item whose position differs between two consecutive stages, most recent ",
        "first. Data as of ", data_cutoff, "."
      )
    )
}
