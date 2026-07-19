suppressPackageStartupMessages({library(ParlAT); library(dplyr); library(purrr)})
data_dir <- "posts/2026-07-19-gov-proposals/_data"
laws <- readRDS(file.path(data_dir, "all_classified.rds")) |> filter(outcome == "law")
out_file <- file.path(data_dir, "vote_details.rds")
done <- if (file.exists(out_file)) readRDS(out_file) else list()
todo <- setdiff(laws$item_url, names(done))
cat("laws:", nrow(laws), "| cached:", length(done), "| todo:", length(todo), "\n")
for (i in seq_along(todo)) {
  u <- todo[i]
  d <- tryCatch(get_item_details(u, stages = FALSE, votes = TRUE),
                error = function(e) { message("ERR ", u, ": ", conditionMessage(e)); "ERROR" })
  done[[u]] <- d
  if (i %% 25 == 0) { saveRDS(done, out_file); cat(i, "of", length(todo), "\n") }
}
saveRDS(done, out_file)
cat("DONE:", length(done), "| errors:", sum(map_lgl(done, is.character)), "\n")
