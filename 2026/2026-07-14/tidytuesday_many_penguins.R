library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)

out_dir <- "2026/2026-07-14"
data_dir <- "2026/data"
data_path <- file.path(data_dir, "many_penguins_2026-07-14.csv")
data_url <- paste0(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/",
  "main/data/2026/2026-07-14/many_penguins.csv"
)
output_path <- file.path(out_dir, "penguin_anatomy_atlas.png")

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(data_path)) {
  download.file(data_url, data_path, mode = "wb", quiet = TRUE)
}

penguins <- read.csv(data_path, stringsAsFactors = FALSE)

footer_text <- paste0(
  "#TidyTuesday 2026 Week 28 | Source: AVONET via TidyTuesday\n",
  "Wajdi Ben Saad | www.WajdiBenSaad.com | Bluesky: wajdi.bsky.social"
)

ice_paper <- "#E8F1EF"
panel_ice <- "#F8FBFA"
navy <- "#12313D"
sea_blue <- "#2E6F83"
grid_blue <- "#B7CDD0"
muted_blue <- "#56747D"
safety_orange <- "#E76F51"

genus_colors <- c(
  Aptenodytes = "#2389A0",
  Eudyptes = "#E76F51",
  Eudyptula = "#4F8A78",
  Megadyptes = "#C99A2E",
  Pygoscelis = "#6C6FA9",
  Spheniscus = "#C85C70"
)

trait_meta <- tibble::tribble(
  ~trait, ~facet_label,
  "beak.length_culmen", "BEAK // CULMEN LENGTH (MM)",
  "beak.length_nares", "BEAK // NARES LENGTH (MM)",
  "beak.width", "BEAK // WIDTH (MM)",
  "beak.depth", "BEAK // DEPTH (MM)",
  "wing.length", "WING // LENGTH (MM)",
  "kipps.distance", "WING // KIPP'S DISTANCE (MM)",
  "secondary1", "WING // FIRST SECONDARY (MM)",
  "hand.wing.index", "WING // HAND-WING INDEX",
  "tarsus.length", "LEG // TARSUS LENGTH (MM)",
  "tail.length", "TAIL // LENGTH (MM)"
)

genus_codes <- c(
  Aptenodytes = "APT",
  Eudyptes = "EPT",
  Eudyptula = "EDL",
  Megadyptes = "MGD",
  Pygoscelis = "PYG",
  Spheniscus = "SPH"
)

anatomy_data <- penguins %>%
  pivot_longer(
    cols = all_of(trait_meta$trait),
    names_to = "trait",
    values_to = "measurement"
  ) %>%
  left_join(trait_meta, by = "trait") %>%
  filter(!is.na(measurement)) %>%
  mutate(
    genus_code = factor(unname(genus_codes[genus]), levels = unname(genus_codes)),
    facet_label = factor(facet_label, levels = trait_meta$facet_label)
  )

anatomy_summary <- anatomy_data %>%
  group_by(facet_label, genus, genus_code) %>%
  summarise(
    q25 = quantile(measurement, 0.25),
    median = median(measurement),
    q75 = quantile(measurement, 0.75),
    .groups = "drop"
  )

anatomy_plot <- ggplot(anatomy_data, aes(genus_code, measurement, color = genus)) +
  geom_point(
    position = position_jitter(width = 0.16, height = 0, seed = 20260714),
    size = 1.2,
    alpha = 0.42
  ) +
  geom_linerange(
    data = anatomy_summary,
    aes(x = genus_code, ymin = q25, ymax = q75, color = genus),
    inherit.aes = FALSE,
    linewidth = 1.35
  ) +
  geom_point(
    data = anatomy_summary,
    aes(y = median),
    shape = 21,
    fill = panel_ice,
    stroke = 1.15,
    size = 3.2
  ) +
  facet_wrap(~ facet_label, scales = "free_y", ncol = 5) +
  scale_color_manual(values = genus_colors) +
  guides(
    color = guide_legend(
      title = "GENUS",
      nrow = 1,
      override.aes = list(size = 3.2, alpha = 1)
    )
  ) +
  labs(
    x = "GENUS CODE // ANTARCTIC MORPHOMETRIC LOG",
    y = "RECORDED MEASUREMENT",
    caption = footer_text
  ) +
  theme_minimal(base_family = "Arial", base_size = 10) +
  theme(
    plot.background = element_rect(fill = ice_paper, color = navy, linewidth = 1),
    panel.background = element_rect(fill = panel_ice, color = grid_blue, linewidth = 0.5),
    panel.grid.major.x = element_line(color = grid_blue, linewidth = 0.28, linetype = "dotted"),
    panel.grid.major.y = element_line(color = grid_blue, linewidth = 0.42),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(0.85, "lines"),
    strip.background = element_rect(fill = navy, color = navy),
    strip.text = element_text(
      color = panel_ice,
      size = 8.5,
      face = "bold",
      margin = margin(7, 4, 7, 4)
    ),
    axis.title = element_text(color = navy, size = 8.8, face = "bold"),
    axis.title.x = element_text(margin = margin(t = 14, b = 16)),
    axis.text.x = element_text(color = navy, size = 7.1, face = "bold"),
    axis.text.y = element_text(color = muted_blue, size = 6.8),
    axis.ticks = element_line(color = sea_blue, linewidth = 0.35),
    legend.position = "top",
    legend.justification = "center",
    legend.background = element_rect(fill = ice_paper, color = NA),
    legend.title = element_text(color = navy, size = 8, face = "bold"),
    legend.text = element_text(color = muted_blue, size = 8),
    legend.key = element_rect(fill = ice_paper, color = NA),
    plot.caption = element_text(
      color = muted_blue,
      size = 7.5,
      lineheight = 1.35,
      hjust = 0.5,
      margin = margin(t = 18)
    ),
    plot.margin = margin(2, 18, 14, 18)
  )

# A small front-facing penguin pictogram built from standard ggplot marks.
penguin_centers <- data.frame(x = c(0.095, 0.905), y = 0.50)

header_plot <- ggplot() +
  geom_segment(
    data = penguin_centers,
    aes(x = x - 0.035, xend = x - 0.075, y = y + 0.02, yend = y - 0.18),
    color = navy,
    linewidth = 7,
    lineend = "round"
  ) +
  geom_segment(
    data = penguin_centers,
    aes(x = x + 0.035, xend = x + 0.075, y = y + 0.02, yend = y - 0.18),
    color = navy,
    linewidth = 7,
    lineend = "round"
  ) +
  geom_point(
    data = penguin_centers,
    aes(x, y),
    shape = 21,
    size = 29,
    stroke = 0,
    fill = navy,
    color = navy
  ) +
  geom_point(
    data = penguin_centers,
    aes(x, y = y - 0.025),
    shape = 21,
    size = 19,
    stroke = 0,
    fill = panel_ice,
    color = panel_ice
  ) +
  geom_point(
    data = penguin_centers,
    aes(x, y = y + 0.13),
    shape = 21,
    size = 18,
    stroke = 0,
    fill = navy,
    color = navy
  ) +
  geom_point(
    data = penguin_centers,
    aes(x, y = y + 0.12),
    shape = 24,
    size = 5.2,
    stroke = 0,
    fill = safety_orange,
    color = safety_orange
  ) +
  geom_segment(
    data = penguin_centers,
    aes(x = x - 0.035, xend = x - 0.005, y = y - 0.18, yend = y - 0.18),
    color = safety_orange,
    linewidth = 4,
    lineend = "round"
  ) +
  geom_segment(
    data = penguin_centers,
    aes(x = x + 0.005, xend = x + 0.035, y = y - 0.18, yend = y - 0.18),
    color = safety_orange,
    linewidth = 4,
    lineend = "round"
  ) +
  annotate(
    "text",
    x = 0.5,
    y = 0.64,
    label = "THE ANATOMY OF A PENGUIN",
    family = "Arial",
    color = navy,
    fontface = "bold",
    size = 10.5
  ) +
  annotate(
    "text",
    x = 0.5,
    y = 0.37,
    label = "Raw specimens with genus medians and interquartile ranges",
    family = "Arial",
    color = muted_blue,
    size = 4.1
  ) +
  annotate(
    "segment",
    x = 0.28,
    xend = 0.72,
    y = 0.20,
    yend = 0.20,
    color = sea_blue,
    linewidth = 0.8
  ) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), clip = "off", expand = FALSE) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = ice_paper, color = NA),
    plot.margin = margin(8, 18, 0, 18)
  )

final_plot <- header_plot / anatomy_plot +
  plot_layout(heights = c(0.19, 1))

ggsave(
  output_path,
  final_plot,
  width = 14,
  height = 9.5,
  dpi = 300,
  device = ragg::agg_png,
  bg = ice_paper
)

message("Saved: ", output_path)
