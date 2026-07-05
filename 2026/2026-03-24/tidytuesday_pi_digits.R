library(ggplot2)
library(dplyr)
library(scales)

out_dir <- "2026/2026-03-24"
data_path <- "2026/data/pi_digits_2026-03-24.csv"

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

pi_digits <- read.csv(data_path, stringsAsFactors = FALSE) %>%
  mutate(
    digit = factor(digit, levels = 0:9),
    digit_value = as.integer(as.character(digit))
  )

footer_text <- "#TidyTuesday 2026 Week 12 | Source: One Million Digits of Pi via TidyTuesday\nWajdi Ben Saad | www.WajdiBenSaad.com | Bluesky: wajdi.bsky.social"

ink <- "#120B22"
paper <- "#FAF4E7"
muted <- "#C7BED7"
warm_digit_palette <- c(
  "0" = "#5C1A1B",
  "1" = "#7F1D1D",
  "2" = "#A8321D",
  "3" = "#C84C1B",
  "4" = "#E36414",
  "5" = "#F77F00",
  "6" = "#FCA311",
  "7" = "#FFD166",
  "8" = "#FFE8A3",
  "9" = "#FFF3C4"
)

theme_pi <- function(base_size = 12) {
  theme_void(base_size = base_size) +
    theme(
      plot.background = element_rect(fill = ink, color = NA),
      panel.background = element_rect(fill = ink, color = NA),
      plot.title = element_text(color = paper, face = "bold", size = 26, margin = margin(b = 8)),
      plot.subtitle = element_text(color = muted, size = 12, margin = margin(b = 18)),
      plot.caption = element_text(color = "#9C93AE", size = 8.5, hjust = 0),
      legend.position = "bottom",
      legend.title = element_text(color = paper, face = "bold"),
      legend.text = element_text(color = muted),
      legend.background = element_rect(fill = ink, color = NA),
      legend.key = element_rect(fill = ink, color = NA),
      plot.margin = margin(24, 28, 22, 28)
    )
}

save_plot <- function(filename, plot, width = 10, height = 10) {
  ggsave(
    file.path(out_dir, filename),
    plot,
    width = width,
    height = height,
    dpi = 320,
    bg = ink
  )
}

# 1. Pi written as a spiral, with prime digits set larger.
digit_galaxy_data <- pi_digits %>%
  filter(digit_position <= 3141) %>%
  mutate(
    theta = digit_position * 0.28,
    radius = sqrt(digit_position) * 1.45,
    x = radius * cos(theta),
    y = radius * sin(theta),
    is_prime_digit = digit_value %in% c(2, 3, 5, 7),
    digit_alpha = ifelse(is_prime_digit, 0.98, 0.72),
    digit_size = ifelse(is_prime_digit, 2.75, 1.65),
    angle = (theta * 180 / pi + 90) %% 360,
    angle = ifelse(angle > 90 & angle < 270, angle + 180, angle)
  )

p_spiral <- ggplot(digit_galaxy_data, aes(x, y, label = digit, color = digit)) +
  geom_text(
    data = digit_galaxy_data %>% filter(is_prime_digit),
    aes(size = digit_size, angle = angle),
    alpha = 0.18,
    family = "mono",
    fontface = "bold"
  ) +
  geom_text(
    aes(size = digit_size, alpha = digit_alpha, angle = angle),
    family = "mono",
    fontface = "bold"
  ) +
  scale_color_manual(values = warm_digit_palette) +
  scale_size_identity() +
  scale_alpha_identity() +
  coord_equal() +
  labs(
    title = "Pi Written As A Spiral",
    subtitle = "The first 3,141 digits; prime digits are set larger to expose spiral ridges.",
    caption = footer_text
  ) +
  theme_pi() +
  theme(legend.position = "none")

save_plot("palette_galaxy_digits_candy_glass.png", p_spiral, width = 11, height = 11)

# 2. Random walk by digit direction.
walk_data <- pi_digits %>%
  filter(digit_position <= 1000000) %>%
  mutate(
    angle = 2 * pi * digit_value / 10,
    dx = cos(angle),
    dy = sin(angle),
    x = cumsum(dx),
    y = cumsum(dy)
  )

p_walk <- ggplot(walk_data, aes(x, y)) +
  geom_path(aes(color = digit_position), linewidth = 0.24, alpha = 0.85, lineend = "round") +
  geom_point(data = slice_tail(walk_data, n = 1), color = "#FFF3C4", size = 2.5) +
  scale_color_gradientn(
    colors = c("#FFF3C4", "#FFD166", "#F77F00", "#C84C1B", "#5C1A1B"),
    labels = label_number(scale_cut = cut_short_scale()),
    guide = guide_colorbar(title = "Position", title.position = "top", barwidth = unit(5.5, "cm"))
  ) +
  coord_equal() +
  labs(
    title = "A Random Walk Through Pi",
    subtitle = "Each digit chooses one of ten directions; the path follows the first one million steps.",
    caption = footer_text
  ) +
  theme_pi()

save_plot("10_pi_random_walk.png", p_walk)

message("Wrote selected Pi digit plots to ", out_dir)
