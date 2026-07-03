library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(scales)
library(grid)

out_dir <- "2026/2026-06-30"
data_path <- "2026/data/wreck_inventory_2026-06-30.csv"

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

wrecks_raw <- read.csv(data_path, stringsAsFactors = FALSE)

footer_text <- "#TidyTuesday 2026 Week 26 | Source: National Monuments Service / data.gov.ie\nWajdi Ben Saad | www.WajdiBenSaad.com | Bluesky: wajdi.bsky.social"

navy <- "#071D2B"
deep <- "#0B2E42"
blue <- "#117DA4"
cyan <- "#58C7D8"
foam <- "#E8F7F7"
mist <- "#A7C8CE"
gold <- "#F2B84B"
coral <- "#F06C5B"
paper <- "#F3F0E8"

theme_wreck <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.background = element_rect(fill = navy, color = NA),
      panel.background = element_rect(fill = navy, color = NA),
      panel.grid.major = element_line(color = "#21485A", linewidth = 0.32),
      panel.grid.minor = element_blank(),
      axis.text = element_text(color = "#B9D4D9"),
      axis.title = element_text(color = "#D9ECEE"),
      plot.title = element_text(color = foam, face = "bold", size = 22, margin = margin(b = 8)),
      plot.subtitle = element_text(color = "#B9D4D9", size = 12, margin = margin(b = 15)),
      plot.caption = element_text(color = "#8DB4BD", size = 8.5, hjust = 0),
      legend.position = "top",
      legend.title = element_text(color = foam),
      legend.text = element_text(color = "#B9D4D9"),
      legend.background = element_rect(fill = navy, color = NA),
      legend.key = element_rect(fill = navy, color = NA),
      strip.text = element_text(color = foam, face = "bold"),
      plot.margin = margin(22, 30, 24, 30)
    )
}

theme_right_legend <- function() {
  theme(
    legend.position = "right",
    legend.direction = "vertical",
    legend.box = "vertical",
    legend.key.height = unit(0.42, "cm"),
    legend.key.width = unit(0.55, "cm"),
    legend.spacing.y = unit(0.08, "cm"),
    legend.title = element_text(size = 10, face = "bold", color = foam),
    legend.text = element_text(size = 8.2, color = "#B9D4D9"),
    legend.margin = margin(0, 0, 0, 8)
  )
}

save_plot <- function(filename, plot, width = 11, height = 7.2) {
  ggsave(
    file.path(out_dir, filename),
    plot,
    width = width,
    height = height,
    dpi = 320,
    bg = navy
  )
}

clean_class <- function(x) {
  x <- ifelse(is.na(x) | x == "", "Unknown", x)
  x <- str_to_title(x)
  x <- case_when(
    str_detect(x, regex("steam", ignore_case = TRUE)) ~ "Steam vessels",
    str_detect(x, regex("schooner|brig|barque|sloop|ketch|lugger|smack|yaw|cutter|sailing", ignore_case = TRUE)) ~ "Sailing vessels",
    str_detect(x, regex("submarine", ignore_case = TRUE)) ~ "Submarine",
    str_detect(x, regex("trawler|fishing", ignore_case = TRUE)) ~ "Fishing vessels",
    str_detect(x, regex("boat|yacht|vessel", ignore_case = TRUE)) ~ "Small craft",
    x == "Unknown" ~ "Unknown",
    TRUE ~ "Other"
  )
  x
}

wrecks <- wrecks_raw %>%
  mutate(
    year = suppressWarnings(as.integer(year)),
    date = as.Date(date),
    month = as.integer(format(date, "%m")),
    decade = floor(year / 10) * 10,
    century = paste0(floor((year - 1) / 100) + 1, "th c."),
    classification = ifelse(is.na(classification) | classification == "", "Unknown", classification),
    class_group = clean_class(classification),
    has_coords = !is.na(latitude) & !is.na(longitude),
    name_known = !is.na(wreck_name) & wreck_name != "" & wreck_name != "Unknown",
    place_short = str_squish(str_replace_all(place_of_loss, "\\s+", " "))
  )

wrecks_year <- wrecks %>%
  filter(!is.na(year), year >= 1500, year <= 2026)

wrecks_coord <- wrecks %>%
  filter(has_coords, longitude >= -25, longitude <= -5, latitude >= 46, latitude <= 58)

ireland_outline <- data.frame(
  lon = c(-10.6, -10.2, -9.7, -9.2, -8.7, -8.2, -7.4, -6.7, -6.1, -5.8, -6.1, -6.4, -6.8, -7.5, -8.2, -8.8, -9.5, -10.1, -10.6),
  lat = c(51.4, 52.1, 52.7, 53.2, 53.7, 54.3, 55.1, 55.3, 54.8, 53.9, 53.1, 52.2, 51.7, 51.5, 51.5, 51.3, 51.2, 51.2, 51.4)
)

p1 <- ggplot() +
  geom_path(
    data = ireland_outline,
    aes(lon, lat),
    color = "#4E8793",
    linewidth = 0.8,
    alpha = 0.82
  ) +
  stat_density_2d(
    data = wrecks_coord,
    aes(longitude, latitude, color = after_stat(level)),
    linewidth = 0.28,
    bins = 16,
    alpha = 0.65
  ) +
  geom_point(
    data = wrecks_coord,
    aes(longitude, latitude),
    color = cyan,
    alpha = 0.22,
    size = 0.55
  ) +
  scale_color_gradient(low = "#315A68", high = gold, guide = "none") +
  coord_fixed(xlim = c(-14.5, -5.2), ylim = c(50.5, 56.2)) +
  labs(
    title = "Wrecks In The Irish Sea Fog",
    subtitle = "Recorded wreck locations with density contours around the island of Ireland.",
    x = "Longitude",
    y = "Latitude",
    caption = footer_text
  ) +
  theme_wreck()

save_plot("01_wreck_density_nautical_chart.png", p1)

p2 <- ggplot(wrecks_coord, aes(longitude, latitude)) +
  stat_density_2d_filled(alpha = 0.92, contour_var = "ndensity", bins = 9) +
  geom_path(
    data = ireland_outline,
    aes(lon, lat),
    inherit.aes = FALSE,
    color = "#9DC7CA",
    linewidth = 0.7,
    alpha = 0.72
  ) +
  scale_fill_manual(
    values = colorRampPalette(c("#0A2233", "#0F5A78", "#2FA7BE", "#D9F5F2", "#F2B84B"))(9),
    labels = c("Low", "", "", "", "Medium", "", "", "", "High"),
    guide = guide_legend(title = "Relative\ndensity", ncol = 1, byrow = TRUE)
  ) +
  coord_fixed(xlim = c(-14.5, -5.2), ylim = c(50.5, 56.2)) +
  labs(
    title = "Shoals Of Recorded Wrecks",
    subtitle = "Filled density bands reveal clusters in the coordinate-rich subset of the inventory.",
    x = "Longitude",
    y = "Latitude",
    caption = footer_text
  ) +
  theme_wreck() +
  theme_right_legend()

save_plot("02_filled_density_shoals.png", p2)

month_levels <- month.abb
month_counts <- wrecks %>%
  filter(!is.na(month)) %>%
  count(month, name = "wrecks") %>%
  mutate(month_lab = factor(month.abb[month], levels = month_levels))

p3 <- ggplot(month_counts, aes(month_lab, wrecks, fill = wrecks)) +
  geom_col(width = 0.88, color = navy, linewidth = 0.25) +
  coord_polar(start = -pi / 12) +
  scale_fill_gradient(low = blue, high = gold, guide = "none") +
  labs(
    title = "A Compass Of Wreck Dates",
    subtitle = "Records with parsed dates by month; winter months carry a heavier ring.",
    x = NULL,
    y = NULL,
    caption = footer_text
  ) +
  theme_wreck() +
  theme(
    axis.text.y = element_blank(),
    axis.text.x = element_text(color = foam, face = "bold", size = 11),
    panel.grid.major.x = element_line(color = "#456B78", linewidth = 0.35)
  )

save_plot("03_monthly_compass_rose.png", p3, height = 8)

cause_patterns <- c(
  "Gale / storm" = "gale|storm|tempest|hurricane",
  "Stranded" = "strand|grounded|ran aground",
  "Collision" = "collision|collided|struck by|in collision",
  "Fire / explosion" = "fire|explosion|burnt|burned",
  "War / attack" = "torpedo|mine|submarine|enemy|war|attack",
  "Foundered" = "founder|sank|sunk|capsize"
)

cause_data <- do.call(
  rbind,
  lapply(names(cause_patterns), function(label) {
    data.frame(
      cause = label,
      year = wrecks_year$year,
      matched = str_detect(wrecks_year$description, regex(cause_patterns[[label]], ignore_case = TRUE))
    )
  })
) %>%
  filter(matched) %>%
  mutate(
    period = cut(
      year,
      breaks = c(1500, 1800, 1850, 1900, 1950, 2026),
      labels = c("1500-1800", "1801-1850", "1851-1900", "1901-1950", "1951-2026"),
      include.lowest = TRUE
    )
  ) %>%
  count(period, cause, name = "records")

p4 <- ggplot(cause_data, aes(period, cause, fill = records)) +
  geom_tile(color = navy, linewidth = 0.8) +
  geom_text(aes(label = records), color = foam, fontface = "bold", size = 3.4) +
  scale_fill_gradient(
    low = "#12364A",
    high = coral,
    trans = "sqrt",
    breaks = pretty_breaks(n = 5),
    labels = comma,
    guide = guide_colorbar(
      title = "Records",
      title.position = "top",
      barheight = unit(4.2, "cm"),
      barwidth = unit(0.45, "cm")
    )
  ) +
  labs(
    title = "Words Of Loss",
    subtitle = "Description keywords suggest how wrecks are narrated across broad periods.",
    x = NULL,
    y = NULL,
    caption = footer_text
  ) +
  theme_wreck() +
  theme_right_legend() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

save_plot("04_words_of_loss_heatmap.png", p4)

poster_plot <- function(filename, width = 16, height = 12, dpi = 320) {
  png(
    file.path(out_dir, filename),
    width = width,
    height = height,
    units = "in",
    res = dpi,
    bg = navy
  )

  grid.newpage()
  grid.rect(gp = gpar(fill = navy, col = NA))

  grid.text(
    "Ireland's Wreck Inventory: Shoals, Seasons, and Words of Loss",
    x = unit(0.045, "npc"),
    y = unit(0.965, "npc"),
    hjust = 0,
    gp = gpar(col = foam, fontsize = 28, fontface = "bold")
  )
  grid.text(
    "Four views of the National Monuments Service wreck records: mapped density, seasonal timing, and description keywords.",
    x = unit(0.045, "npc"),
    y = unit(0.925, "npc"),
    hjust = 0,
    gp = gpar(col = "#B9D4D9", fontsize = 13)
  )

  draw_panel <- function(plot, x, y, width, height) {
    pushViewport(viewport(x = x, y = y, width = width, height = height, just = c("left", "bottom")))
    grid.draw(ggplotGrob(plot + labs(caption = NULL) + theme(plot.margin = margin(8, 10, 8, 10))))
    popViewport()
  }

  p3_poster <- p3 + labs(subtitle = "Winter records form the largest ring.")
  p4_poster <- p4 + labs(subtitle = "Keyword patterns in wreck descriptions.")

  draw_panel(p1, 0.035, 0.52, 0.455, 0.365)
  draw_panel(p2, 0.515, 0.52, 0.455, 0.365)
  draw_panel(p3_poster, 0.035, 0.125, 0.455, 0.365)
  draw_panel(p4_poster, 0.515, 0.125, 0.455, 0.365)

  grid.text(
    footer_text,
    x = unit(0.045, "npc"),
    y = unit(0.035, "npc"),
    hjust = 0,
    gp = gpar(col = "#8DB4BD", fontsize = 9)
  )

  dev.off()
}

poster_plot("05_wreck_inventory_poster.png")

message("Wrote final plots and poster to ", out_dir)
