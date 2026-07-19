suppressPackageStartupMessages({library(dplyr); library(purrr); library(tidyr)})
data_dir <- "posts/2026-07-19-gov-proposals/_data"
laws <- readRDS(file.path(data_dir, "all_classified.rds")) |> filter(outcome == "law")
vd <- readRDS(file.path(data_dir, "vote_details.rds"))

pos <- imap(vd, function(d, u) {
  if (is.character(d)) return(NULL)
  v <- tryCatch(d$votes[[1]], error = function(e) NULL)
  if (is.null(v)) return(NULL)
  r <- v$result
  if (is.null(r) || !is.data.frame(r) || !nrow(r)) return(NULL)
  tibble(item_url = u, party = as.character(r$text), infavor = as.logical(r$infavor))
}) |> list_rbind()
cat("laws with vote data:", n_distinct(pos$item_url), "of", nrow(laws), "\n")
cat("party labels seen:\n"); print(table(pos$party))

coal <- function(d) {
  if (d <  as.Date("2000-02-04")) return(c("SPÖ","ÖVP"))
  if (d <  as.Date("2005-04-17")) return(c("ÖVP","FPÖ","F"))
  if (d <  as.Date("2007-01-11")) return(c("ÖVP","BZÖ","F-BZÖ"))
  if (d <  as.Date("2017-12-18")) return(c("SPÖ","ÖVP"))
  if (d <  as.Date("2019-05-28")) return(c("ÖVP","FPÖ"))
  if (d <  as.Date("2020-01-07")) return(character(0))     # caretaker
  if (d <  as.Date("2025-03-03")) return(c("ÖVP","GRÜNE"))
  c("ÖVP","SPÖ","NEOS")
}
nonparty <- c("OK","OF")

vb <- pos |> filter(!party %in% nonparty) |>
  group_by(item_url) |>
  summarise(infav = list(party[infavor]), against = list(party[!infavor]), .groups = "drop")

res <- laws |> select(item_url, origin, legis_period, subject, third_reading) |>
  left_join(vb, by = "item_url")
res$vote_class <- vapply(seq_len(nrow(res)), function(i) {
  iv <- res$infav[[i]]; ag <- res$against[[i]]
  if (is.null(iv)) return("no vote data")
  co <- coal(res$third_reading[i])
  if (!length(co)) return("caretaker period")
  if (!length(ag)) return("unanimous")
  opp_in_favor <- setdiff(iv, co)
  coal_missing <- setdiff(co, c(iv, setdiff(co, unique(c(iv, ag)))))  # coalition parties that voted against
  coal_against <- intersect(ag, co)
  if (length(coal_against)) return("other")
  if (!length(opp_in_favor)) return("coalition only")
  "coalition + opposition"
}, character(1))
saveRDS(res, file.path(data_dir, "vote_classified.rds"))

cat("\n--- overall ---\n"); print(table(res$vote_class))
cat("\n--- by period (share, %) ---\n")
t <- table(res$legis_period, res$vote_class)
print(t[c("XX","XXI","XXII","XXIII","XXIV","XXV","XXVI","XXVII","XXVIII"),])
cat("\n--- 'other' samples ---\n")
oth <- which(res$vote_class == "other")[1:min(8, sum(res$vote_class=="other"))]
for (i in oth) cat(res$item_url[i], "| For:", paste(res$infav[[i]], collapse=","),
                   "| Against:", paste(res$against[[i]], collapse=","), "\n")
cat("\n--- no-vote-data by period ---\n")
print(table(res$legis_period[res$vote_class=="no vote data"]))
