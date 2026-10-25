# Convenience entry points for the pipeline.
#
# Run with the working directory set to this folder — _targets.R locates the blog
# root by walking up from getwd(), and the store lives at ./_targets.
#
#   setwd("_dev/parlat-voting")   # from the blog root
#   source("run.R")

library(targets)

stopifnot(
  "run.R must be sourced with the working directory set to _dev/parlat-voting" =
    file.exists("_targets.R")
)

# 1. Validate target definitions before running anything.
#    tar_manifest(fields = c("name", "command"))

# 2. Inspect the dependency graph.
#    tar_visnetwork(targets_only = TRUE)

# 3. Build. No network access needed: every input is an existing .rds tracked as a
#    file target. Refreshing those is a separate step — see refresh_data.R.
#    tar_make()

# 4. Pull results into the session.
#    tar_load(c(votes_wide, transitions, party_order))
#    tar_read(p_flip_no_to_yes)

# 5. What is stale, and what cost what.
#    tar_outdated()
#    tar_meta(fields = c("seconds", "bytes"), complete_only = TRUE)

# 6. Render the document that reads the store.
#    quarto::quarto_render("index.qmd")

# Sanity check used during the port: the party set should hold no single-letter
# codes and no separate JETZT / PILZ entries.
check_parties <- function() {
  sort(unique(tar_read(votes_long)$party))
}

# Spot-check a handful of published figures' numbers against the store.
check_flip_counts <- function() {
  tar_read(flip_grid_no_to_yes)[
    tar_read(flip_grid_no_to_yes)$flip_n > 0,
    c("party", "legis_period", "flip_n")
  ]
}
