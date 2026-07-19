suppressPackageStartupMessages({library(dplyr); library(ggplot2); library(forcats); library(scales)})
res <- readRDS("posts/2026-07-19-gov-proposals/_data/vote_classified.rds")

df <- res |>
  mutate(m = as.integer(format(third_reading, "%m")),
         d = as.integer(format(third_reading, "%d")),
         y = as.integer(format(third_reading, "%Y")),
         ys = ifelse(m > 9 | (m == 9 & d >= 15), y, y - 1),
         tagung = sprintf("%d/%02d", ys, (ys + 1) %% 100),
         rv_group = ifelse(origin == "RV", "Regierungsvorlagen",
                           "Other bills (Initiativantrag, BUA, Bundesrat)"),
         rv_group = factor(rv_group, levels = c("Regierungsvorlagen",
                                                "Other bills (Initiativantrag, BUA, Bundesrat)")),
         class = case_when(
           vote_class == "unanimous" ~ "unanimous",
           vote_class == "coalition + opposition" ~ "coalition + opposition",
           vote_class == "coalition only" ~ "coalition only",
           TRUE ~ "other / no data") |>
           factor(levels = c("coalition only", "coalition + opposition", "unanimous", "other / no data")))

totals <- df |> count(rv_group, tagung, name = "n_laws")

p <- ggplot(df, aes(x = tagung, fill = class)) +
  geom_bar(width = 0.8) +
  geom_text(data = totals, aes(x = tagung, y = n_laws + 4, label = n_laws),
            inherit.aes = FALSE, size = 2.4, color = "grey40") +
  facet_wrap(~rv_group, ncol = 1) +
  scale_y_continuous(expand = expansion(mult = c(0.01, 0.08))) +
  scale_fill_manual(values = c("coalition only" = "#c03728",
                               "coalition + opposition" = "#f5c04a",
                               "unanimous" = "#2c6e63",
                               "other / no data" = "grey80")) +
  labs(title = "How laws passed the Nationalrat, 1996–2026",
       subtitle = "Number of federal laws by voting majority in the third reading, split by bill origin; parliamentary years (mid-Sept to mid-Sept).\n'Other' incl. caretaker period 2019 and missing vote records.",
       x = NULL, y = "laws passed", fill = NULL,
       caption = "Source: parlament.gv.at via {ParlAT} | draft") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7.5),
        legend.position = "top",
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold", hjust = 0, size = 10),
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(size = 9, color = "grey30"),
        plot.caption = element_text(size = 8, color = "grey50"))

ggsave("posts/2026-07-19-gov-proposals/_data/vote-classes-by-origin-abs.png", p,
       width = 11, height = 9, dpi = 150, bg = "white")
cat("saved\n")
