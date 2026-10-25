# Paths, palette and shared string constants.
#
# Everything here is deliberately free of pipeline state so the module can be
# sourced on its own when poking at the data interactively.

# ---- paths -------------------------------------------------------------------

POST_SLUG <- "2026-06-08-ParlAT-votingPatterns-2"

# Walk up from `start` until the blog root is found. Anchoring on the .Rproj file
# rather than here::here() because `here`'s root criteria do not include
# _quarto.yml, and this folder carries a nested one.
blog_root <- function(start = getwd()) {
  path <- normalizePath(start, winslash = "/", mustWork = TRUE)
  repeat {
    if (file.exists(file.path(path, "quarto_blog.Rproj"))) {
      return(path)
    }
    parent <- dirname(path)
    if (identical(parent, path)) {
      stop(
        "Could not locate the blog root (quarto_blog.Rproj) above: ",
        start,
        call. = FALSE
      )
    }
    path <- parent
  }
}

# The three cached fetches stay where the post already keeps them; the pipeline
# reads them in place rather than duplicating ~218 MB into _targets/objects/.
data_dir <- function() {
  file.path(blog_root(), "posts", POST_SLUG, "_data")
}

data_path <- function(file) {
  file.path(data_dir(), file)
}

# ---- plot styling ------------------------------------------------------------

FONT <- "Noto Sans"

# Named replacements for the hex literals that were scattered across nine chunks.
PAL <- list(
  green      = "#1a9641", # no -> yes (a move toward support)
  red        = "#d73027", # yes -> no (a withdrawal of support)
  green_dark = "#1a9850", # diverging high end, split heatmaps
  red_dark   = "#b2182b", # diverging low end, split heatmaps
  purple     = "#3b0f70", # stage x period flip rate
  blue       = "#4575b4", # committee -> 2nd reading
  orange     = "#fdae61", # 2nd -> 3rd reading
  pos_yes    = "#1a7a3c", # "Dafür" in tables
  pos_no     = "#c0392b", # "Dagegen" in tables
  tile_low   = "grey93",
  tile_mid   = "grey92",
  tile_low_2 = "grey94"
)

# ---- domain constants --------------------------------------------------------

# Item types that can carry a reading vote. A=Anträge, RV=Regierungsvorlagen,
# BUA/GABR/BRA=budget-related, EBR=EU items. Anchored patterns (^A$, ^EBR$)
# prevent partial matches against longer codes.
ITEM_TYPE_PATTERNS <- c("^A$", "RV", "BUA", "GABR", "GABR13", "BRA", "^EBR$")

item_type_regex <- function() {
  paste0(ITEM_TYPE_PATTERNS, collapse = "|")
}

# Single definition of the vote-keyword set. Previously this existed both as a
# constant in `committee-context-summary` and as a duplicated literal regex in
# `scope-items`, which meant the two could silently drift apart.
VOTE_KEYWORDS <- paste(
  "dafür",
  "wechselnde Mehrheit",
  "getrennte Abstimmung",
  "namentliche Abstimmung",
  "abgelehnt",
  "mehrstimmig",
  sep = "|"
)

ALL_PERIODS <- c(
  "XX",
  "XXI",
  "XXII",
  "XXIII",
  "XXIV",
  "XXV",
  "XXVI",
  "XXVII",
  "XXVIII"
)

TRANSITION_LEVELS <- c("Committee → 2nd reading", "2nd → 3rd reading")

# Parties pinned to the top of every tile plot; the rest follow by how many
# periods they appear in.
PARTY_ORDER_PRIORITY <- c("FPÖ", "ÖVP", "SPÖ", "GRÜNE", "NEOS")
