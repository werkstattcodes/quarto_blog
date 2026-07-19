# Laws passed by the Nationalrat since 1996, by origin (RV vs other).
# Produces the .rds files in this folder; see README.md.
# Full re-scrape takes ~2h (one get_item_details() call per item).

library(ParlAT)
library(dplyr)
library(purrr)
library(tidyr)

data_dir <- "posts/2026-07-19-gov-proposals/_data"
periods <- 20:28  # XX (1996) - XXVIII

# 1. Items that can result in a federal law ------------------------------
grab <- function(itm) {
  map(periods, function(p) {
    tryCatch(get_items(item = itm, institution = "NR", legis_period = p, echo = FALSE),
             error = function(e) NULL)
  }) |> list_rbind()
}
all_items <- list(rv = grab("RV"), antr = grab("ANTR"),
                  gabr = grab("GABR"), volkbg = grab("VOLKBG"))
saveRDS(all_items, file.path(data_dir, "all_items.rds"))

# 2. Stage 5 = "settled", NOT "passed": includes rejected and miterledigte
#    items. Filter to law-capable doc types; outcome is resolved in step 3/4.
passed <- bind_rows(
  all_items$rv   |> filter(stage == "5") |> mutate(origin = "RV"),
  all_items$antr |> filter(type_doc == "A",   stage == "5") |> mutate(origin = "A"),
  all_items$antr |> filter(type_doc == "BUA", stage == "5") |> mutate(origin = "BUA"),
  all_items$gabr |> filter(stage == "5") |> mutate(origin = "GABR")
)
saveRDS(passed, file.path(data_dir, "passed_laws.rds"))

# 3. Detail pages (slow; resumable via cache) ----------------------------
out_file <- file.path(data_dir, "all_details.rds")
done <- if (file.exists(out_file)) readRDS(out_file) else list()
todo <- setdiff(passed$item_url, names(done))
for (i in seq_along(todo)) {
  u <- todo[i]
  done[[u]] <- tryCatch(get_item_details(u, stages = TRUE, votes = FALSE),
                        error = function(e) "ERROR")
  if (i %% 25 == 0) saveRDS(done, out_file)
}
saveRDS(done, out_file)

# 4. Outcome classification ----------------------------------------------
`%||%` <- function(a, b) if (is.null(a)) b else a
info <- imap(done, function(d, u) {
  if (is.character(d)) return(tibble(item_url = u, status = NA_character_,
                                     third_reading = as.Date(NA),
                                     miterledigt = NA, rejected = NA))
  sd <- gsub("\n", " ", d$status_description %||% "")
  st <- d$stages[[1]]
  third_date <- as.Date(NA); miterl <- FALSE
  if (!is.null(st) && nrow(st)) {
    nm <- as.character(st$stage_name)
    dt <- as.Date(as.character(st$stage_date), format = "%d.%m.%Y")
    i3 <- grepl("dritter Lesung", nm, ignore.case = TRUE) &
          grepl("angenommen", nm, ignore.case = TRUE)
    if (any(i3, na.rm = TRUE)) third_date <- max(dt[which(i3)], na.rm = TRUE)
    miterl <- any(grepl("Miterledigung|miterledigt", nm, ignore.case = TRUE), na.rm = TRUE)
  }
  if (grepl("Miterledigung|miterledigt", sd, ignore.case = TRUE)) miterl <- TRUE
  tibble(item_url = u, status = substr(sd, 1, 70), third_reading = third_date,
         miterledigt = miterl && is.na(third_date),
         rejected = grepl("abgelehnt", sd, ignore.case = TRUE))
}) |> list_rbind()

all_classified <- passed |> left_join(info, by = "item_url") |>
  mutate(outcome = case_when(!is.na(third_reading) ~ "law",
                             miterledigt ~ "miterledigt",
                             rejected ~ "rejected",
                             TRUE ~ "unclear"))
saveRDS(all_classified, file.path(data_dir, "all_classified.rds"))

# 5. RV share per Tagung year (Sept 15 boundary) --------------------------
laws <- all_classified |> filter(outcome == "law") |>
  mutate(m = as.integer(format(third_reading, "%m")),
         d = as.integer(format(third_reading, "%d")),
         y = as.integer(format(third_reading, "%Y")),
         ys = ifelse(m > 9 | (m == 9 & d >= 15), y, y - 1),
         tagung = sprintf("%d/%02d", ys, (ys + 1) %% 100))
tab <- laws |> count(tagung, origin) |>
  pivot_wider(names_from = origin, values_from = n, values_fill = 0) |>
  mutate(total = RV + A + BUA + GABR, pct_rv = 100 * RV / total)
print(as.data.frame(tab), row.names = FALSE)
