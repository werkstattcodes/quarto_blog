# Data: laws passed by the Nationalrat since 1996, by origin

Scraped 2026-07-18/19 via ParlAT from www.parlament.gv.at (periods XX–XXVIII).

- `all_items.rds` — list of raw `get_items()` results: `rv` (Regierungsvorlagen), `antr` (Anträge, all type_doc), `gabr`, `volkbg`; institution NR, periods 20–28.
- `passed_laws.rds` — items with `stage == "5"` filtered to law-capable types (RV, A, BUA, GABR) with an `origin` column. Caution: stage 5 means "settled", not "passed" — includes rejected and miterledigte items.
- `all_details.rds` — named list (by item_url) of `get_item_details(url, stages = TRUE)` for all 4,823 stage-5 items. ~2h of API calls; do not re-scrape.
- `all_classified.rds` — `passed_laws` joined with the outcome classification: `outcome` ("law" = passed third reading, "miterledigt", "rejected", "unclear"), `third_reading` (passage date), `status`. Only `outcome == "law"` (4,120 items) are actual laws.

Yearly aggregation (Aug–Jul on `third_reading`) reproduces the official RV-share statistics within 0–2pp.
