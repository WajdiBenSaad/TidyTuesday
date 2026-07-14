library(ggplot2)
library(dplyr)
library(scales)
library(ggrepel)
library(patchwork)
library(systemfonts)

out_dir <- "2026/2026-06-09"
data_dir <- "2026/data"
data_path <- file.path(data_dir, "game_films_2026-06-09.csv")
data_url <- paste0(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/",
  "main/data/2026/2026-06-09/game_films.csv"
)
font_path <- file.path(out_dir, "resources", "fonts", "PressStart2P-Regular.ttf")
output_path <- file.path(out_dir, "video_game_movies_scores.png")

title_text <- "DO VIDEO-GAME MOVIES\nFINALLY GET GOOD?"
subtitle_text <- paste0(
  "Three decades of Rotten Tomatoes scores suggest the genre may have found an extra life  ",
  "//  73 reviewed releases"
)
footer_text <- paste0(
  "#TidyTuesday 2026 Week 23 | Source: Wikipedia data compiled by TidyTuesday\n",
  "Wajdi Ben Saad | www.WajdiBenSaad.com | Bluesky: wajdi.bsky.social"
)

bg <- "#080A12"
panel_bg <- "#0D1020"
grid_color <- "#26304B"
text_primary <- "#F4F7FF"
text_muted <- "#AAB3CB"
neon_cyan <- "#3CF2FF"
neon_pink <- "#FF2B55"
neon_yellow <- "#FFE66D"

register_font(
  name = "Press Start 2P",
  plain = font_path,
  bold = font_path,
  italic = font_path,
  bolditalic = font_path
)

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(data_path)) {
  download.file(data_url, data_path, mode = "wb", quiet = TRUE)
}

films <- read.csv(data_path, stringsAsFactors = FALSE) %>%
  mutate(
    release_date = as.Date(release_date),
    release_year = as.integer(format(release_date, "%Y")),
    score = as.numeric(rotten_tomatoes),
    decade = floor(release_year / 10) * 10
  ) %>%
  filter(!is.na(release_date), !is.na(score)) %>%
  arrange(release_year, score, title) %>%
  group_by(release_year) %>%
  mutate(
    x_offset = if (n() == 1) 0 else seq(-0.30, 0.30, length.out = n()),
    plot_year = release_year + x_offset
  ) %>%
  ungroup()

trend <- data.frame(release_year = seq(min(films$release_year), max(films$release_year))) %>%
  rowwise() %>%
  mutate(
    films_in_window = sum(abs(films$release_year - release_year) <= 2),
    rolling_median = median(
      films$score[abs(films$release_year - release_year) <= 2],
      na.rm = TRUE
    )
  ) %>%
  ungroup() %>%
  filter(films_in_window >= 3)

decade_summary <- films %>%
  filter(decade %in% c(1990, 2000, 2010, 2020)) %>%
  group_by(decade) %>%
  summarise(
    median_score = median(score),
    n_films = n(),
    .groups = "drop"
  ) %>%
  mutate(
    xmin = pmax(decade, min(films$release_year) - 0.5),
    xmax = pmin(decade + 9.8, max(films$release_year) + 0.5),
    midpoint = (xmin + xmax) / 2,
    level = paste0("LEVEL ", row_number()),
    summary = paste0(decade, "s  //  MEDIAN ", round(median_score))
  )

decade_bands <- decade_summary %>%
  mutate(fill = rep(c("#10162A", "#0B1326"), length.out = n()))

label_titles <- c(
  "Super Mario Bros.",
  "Alone in the Dark",
  "Resident Evil: Damnation",
  "Werewolves Within",
  "Sonic the Hedgehog 3"
)

film_labels <- films %>%
  filter(title %in% label_titles) %>%
  mutate(
    display_title = case_when(
      title == "Resident Evil: Damnation" ~ "RESIDENT EVIL:\nDAMNATION",
      title == "Sonic the Hedgehog 3" ~ "SONIC 3",
      TRUE ~ toupper(title)
    )
  )

scanlines <- data.frame(y = seq(0, 100, by = 2))

hud <- ggplot(decade_summary) +
  geom_rect(
    aes(xmin = xmin + 0.18, xmax = xmax - 0.18, ymin = 0, ymax = 1),
    fill = panel_bg,
    color = grid_color,
    linewidth = 0.6
  ) +
  geom_segment(
    aes(x = xmin + 0.18, xend = xmax - 0.18, y = 0.06, yend = 0.06),
    color = neon_pink,
    linewidth = 1.2
  ) +
  geom_text(
    aes(midpoint, 0.67, label = level),
    family = "Press Start 2P",
    color = neon_cyan,
    size = 2.45
  ) +
  geom_text(
    aes(midpoint, 0.32, label = summary),
    family = "Press Start 2P",
    color = text_primary,
    size = 1.65
  ) +
  scale_x_continuous(limits = c(1992.5, 2026.6), expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = bg, color = NA),
    plot.margin = margin(0, 12, 2, 12)
  )

main_plot <- ggplot() +
  geom_rect(
    data = decade_bands,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = fill),
    inherit.aes = FALSE,
    show.legend = FALSE
  ) +
  scale_fill_identity() +
  geom_hline(
    data = scanlines,
    aes(yintercept = y),
    color = "#FFFFFF",
    linewidth = 0.16,
    alpha = 0.035
  ) +
  geom_vline(
    xintercept = c(2000, 2010, 2020),
    color = neon_cyan,
    linewidth = 0.35,
    alpha = 0.30,
    linetype = "dashed"
  ) +
  geom_step(
    data = trend,
    aes(release_year, rolling_median),
    color = neon_cyan,
    linewidth = 5.5,
    alpha = 0.055,
    direction = "mid"
  ) +
  geom_step(
    data = trend,
    aes(release_year, rolling_median),
    color = neon_cyan,
    linewidth = 2.6,
    alpha = 0.16,
    direction = "mid"
  ) +
  geom_step(
    data = trend,
    aes(release_year, rolling_median),
    color = neon_cyan,
    linewidth = 0.9,
    direction = "mid"
  ) +
  geom_point(
    data = films,
    aes(plot_year, score),
    shape = 15,
    color = "#000000",
    size = 5.1,
    alpha = 0.70
  ) +
  geom_point(
    data = films,
    aes(plot_year, score, color = score),
    shape = 15,
    size = 3.6
  ) +
  geom_label_repel(
    data = film_labels,
    aes(plot_year, score, label = display_title),
    family = "Press Start 2P",
    color = text_primary,
    fill = alpha(bg, 0.92),
    box.padding = 0.55,
    point.padding = 0.45,
    min.segment.length = 0,
    segment.color = neon_yellow,
    segment.size = 0.35,
    label.padding = unit(0.18, "lines"),
    label.r = unit(0, "lines"),
    label.size = 0.25,
    size = 1.65,
    seed = 20260609,
    max.overlaps = Inf,
    show.legend = FALSE
  ) +
  annotate(
    "label",
    x = 2025.9,
    y = tail(trend$rolling_median, 1) + 7,
    label = "5-YEAR\nMEDIAN PATH",
    family = "Press Start 2P",
    color = neon_cyan,
    fill = alpha(bg, 0.90),
    hjust = 1,
    size = 1.7,
    label.padding = unit(0.18, "lines"),
    label.r = unit(0, "lines"),
    linewidth = 0.25
  ) +
  scale_color_gradientn(
    colours = c(neon_pink, "#FF6438", neon_yellow, neon_cyan, "#7CFFB2"),
    values = rescale(c(0, 25, 50, 75, 100)),
    limits = c(0, 100),
    breaks = c(0, 25, 50, 75, 100),
    guide = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      barwidth = unit(11, "lines"),
      barheight = unit(0.55, "lines"),
      ticks.colour = text_primary,
      frame.colour = grid_color
    )
  ) +
  scale_x_continuous(
    limits = c(1992.5, 2026.6),
    breaks = seq(1995, 2025, by = 5),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = c(0, 25, 50, 75, 100),
    labels = c("0  GAME OVER", "25", "50", "75", "100  HIGH SCORE"),
    expand = expansion(mult = c(0.015, 0.025))
  ) +
  labs(
    x = "RELEASE YEAR  >  NEXT LEVEL",
    y = "ROTTEN TOMATOES SCORE",
    color = "CRITIC SCORE"
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_family = "Arial", base_size = 11) +
  theme(
    plot.background = element_rect(fill = bg, color = NA),
    panel.background = element_rect(fill = panel_bg, color = grid_color, linewidth = 0.8),
    panel.grid.major = element_line(color = grid_color, linewidth = 0.35),
    panel.grid.minor = element_blank(),
    axis.text = element_text(color = text_muted, family = "Press Start 2P", size = 6.2),
    axis.text.x = element_text(margin = margin(t = 8)),
    axis.text.y = element_text(margin = margin(r = 8)),
    axis.title = element_text(color = text_primary, family = "Press Start 2P", size = 7.2),
    axis.title.x = element_text(margin = margin(t = 14)),
    axis.title.y = element_text(margin = margin(r = 14)),
    legend.position = "bottom",
    legend.title = element_text(color = neon_cyan, family = "Press Start 2P", size = 6.5),
    legend.text = element_text(color = text_muted, family = "Press Start 2P", size = 5.8),
    legend.margin = margin(t = 5, b = 0),
    plot.margin = margin(4, 16, 2, 16)
  )

final_plot <- hud / main_plot +
  plot_layout(heights = c(0.16, 1), guides = "collect") +
  plot_annotation(
    title = title_text,
    subtitle = subtitle_text,
    caption = footer_text,
    theme = theme(
      plot.background = element_rect(fill = bg, color = neon_pink, linewidth = 1.2),
      plot.title = element_text(
        family = "Press Start 2P",
        color = text_primary,
        size = 20,
        lineheight = 1.25,
        hjust = 0.5,
        margin = margin(t = 24, b = 12)
      ),
      plot.subtitle = element_text(
        family = "Arial",
        color = text_muted,
        size = 10.5,
        hjust = 0.5,
        margin = margin(b = 18)
      ),
      plot.caption = element_text(
        family = "Arial",
        color = text_muted,
        size = 7.5,
        lineheight = 1.3,
        hjust = 0.5,
        margin = margin(t = 12, b = 18)
      ),
      plot.margin = margin(10, 10, 10, 10)
    )
  ) &
  theme(legend.position = "bottom")

ggsave(
  output_path,
  final_plot,
  width = 10,
  height = 7.2,
  dpi = 300,
  device = ragg::agg_png,
  bg = bg
)

message("Saved: ", output_path)

source(file.path(out_dir, "make_interactive_page.R"), local = new.env())
