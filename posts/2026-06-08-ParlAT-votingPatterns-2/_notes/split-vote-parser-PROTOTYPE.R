suppressMessages({library(dplyr);library(tidyr);library(purrr);library(stringr)})

# ---- letter -> party, for the case-encoded form (SVfLg) -------------------
LETTER_PARTY <- c(S = "SPÖ", V = "ÖVP", F = "FPÖ", G = "GRÜNE", L = "L",
                  B = "BZÖ", N = "NEOS", T = "STRONACH", J = "JETZT/PILZ")

# ---- split one vote text into its PART-level supporter sets ---------------
# Returns a list of character vectors, one per part of the bill, or NULL if the
# text names no parties at all.
parse_part_sets <- function(txt) {
  # 1. case-encoded blocks: SVfLg bzw. SVFLg   (upper = for, lower = against)
  blocks <- str_match_all(txt, "\\b([SVFGLNBTJ][SVFGLNBTJsvfglnbtj]{2,})\\b")[[1]]
  if (nrow(blocks) > 0) {
    return(map(blocks[, 2], \(b) {
      ch <- str_split_1(b, "")
      unname(LETTER_PARTY[toupper(ch[ch == toupper(ch)])])
    }))
  }
  # 2. "dafür: S, V bzw. S, V, F"  /  "dafür. S, V, tlw. G"
  m <- str_match(txt, regex("dafür[:.]\\s*(.+)$", ignore_case = TRUE))[, 2]
  if (is.na(m)) return(NULL)
  m <- str_remove(m, "\\)+\\s*$")          # nested parens: strip ALL trailing ")"
  # cut tails that are not party lists at all (vote counts, croquis references)
  m <- str_remove(m, regex("\\)?\\s*(Stimmenausz|JA-Stimmen|NEIN-Stimmen|siehe\\s+Croquis).*$",
                           ignore_case = TRUE, dotall = TRUE))
  parts <- str_split_1(m, regex("\\s+bzw\\.?\\s+|,?\\s+teils\\s+", ignore_case = TRUE))
  sets <- map(parts, \(p) {
    toks <- str_split_1(p, ",\\s*") |> str_trim() |> str_remove("\\s*\\(.*$")
    toks <- toks[toks != ""]
    unname(map_chr(toks, \(t) {
      # "F ohne Abg. X" = party voted for except named MPs -> keep the party, the
      # within-party split is picked up by classify_position()
      t2 <- str_remove(t, regex("\\s+(ohne|und)\\s+Abg\\..*$", ignore_case = TRUE))
      t2 <- str_remove(t2, regex("^(tlw\\.?|teilw\\.?|teilweise)\\s*", ignore_case = TRUE)) |> str_trim()
      if (nchar(t2) == 1 && t2 %in% names(LETTER_PARTY)) LETTER_PARTY[[t2]] else t2
    }))
  })
  # "teils einstimmig" = some provisions carried unanimously -> an implicit part in
  # which EVERY seated party voted for. Without this, a party absent from the "dafür"
  # list reads as plain Against when it actually supported the unanimous provisions.
  if (str_detect(txt, regex("teils\\s+einstimmig|einstimmig[,)]", ignore_case = TRUE))) {
    sets <- c(sets, list("__ALL__"))
  }
  sets
}

# ---- classify one party's position given the part sets -------------------
classify_position <- function(party, part_sets, txt) {
  in_part <- map_lgl(part_sets, \(s) identical(s, "__ALL__") || party %in% s)
  # "tlw. X" marks MPs of X dissenting -> split WITHIN the party
  letter <- names(LETTER_PARTY)[match(party, LETTER_PARTY)]
  partial <- "(tlw\\.?|teilw\\.?|teilweise)\\s*"
  if (str_detect(txt, regex(paste0(partial, party, "\\b"), ignore_case = TRUE)) ||
      (!is.na(letter) && str_detect(txt, regex(paste0(partial, letter, "\\b")))) ||
      str_detect(txt, regex(paste0("\\b", party, "\\s+ohne\\s+Abg\\."), ignore_case = TRUE)) ||
      (!is.na(letter) && str_detect(txt, paste0("\\b", letter, "\\s+ohne\\s+Abg\\.")))) {
    return("Split-within-party")
  }
  if (all(in_part))  return("For")
  if (!any(in_part)) return("Against")
  "Split-across-bill"                       # in some parts, not others
}

# ---------------- prove it on the real notations --------------------------
cases <- c(
  "wechselnde Mehrheiten (SVfLg bzw. SVFLg)",
  "wechselnde Mehrheiten (teils SVFLG, teils SVflg)",
  "Getrennte Abstimmung (mit wechselnden Mehrheiten (dafür: S, V, F bzw. S, V))",
  "Getrennte Abstimmung (teils einstimmig, teils mehrstimmig (dafür: S, V))",
  "mehrstimmig (dafür: S, V, tlw. L)",
  "mehrstimmig (dafür. S, V, tlw. G)",
  "Getrennte Abstimmung (teils einstimmig, teils mehrstimmig)"
)
for (txt in cases) {
  ps <- parse_part_sets(txt)
  cat("\n", str_trunc(txt, 76), "\n", sep = "")
  if (is.null(ps)) { cat("   -> no party names, unusable\n"); next }
  cat("   parts: ", paste(map_chr(ps, \(s) paste(s, collapse = "+")), collapse = "  |  "), "\n", sep = "")
  seated <- c("SPÖ","ÖVP","FPÖ","GRÜNE","L")
  cat("   ", paste(map_chr(seated, \(p) paste0(p, "=", classify_position(p, ps, txt))),
                   collapse = "  "), "\n", sep = "")
}
