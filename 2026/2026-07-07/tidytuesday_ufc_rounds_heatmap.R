library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(png)
library(grid)
library(systemfonts)

out_dir <- "2026/2026-07-07"
data_dir <- "2026/data"
data_path <- file.path(data_dir, "ufc_fights_2026-07-07.csv")
data_url <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-07-07/ufc_fights.csv"
logo_path <- file.path(out_dir, "resources", "ufc_wordmark.png")
title_font_path <- file.path(out_dir, "resources", "fonts", "Sternbach Italic.otf")

title_text <- "Inside the Final Round"
subtitle_text <- "How UFC fights conclude across five rounds, from knockouts to decisions"
footer_text <- paste0(
  "#TidyTuesday 2026 Week 27 | Source: UFC fight data via TidyTuesday/fightr\n",
  "Wajdi Ben Saad | www.WajdiBenSaad.com | Bluesky: wajdi.bsky.social"
)

background_color <- "#070708"
ufc_red <- "#C41230"
render_width_in <- 9
render_height_in <- 5.3
render_dpi <- 300
output_width_px <- 2260
output_height_px <- as.integer(render_height_in * render_dpi)
border_width_pt <- 2
border_width_px <- round(render_dpi * border_width_pt / 72)
content_width_px <- output_width_px - 4 * border_width_px
content_height_px <- output_height_px - 4 * border_width_px

register_font(
  name = "Sternbach",
  plain = title_font_path,
  italic = title_font_path,
  bold = title_font_path,
  bolditalic = title_font_path
)

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(data_path)) {
  download.file(data_url, data_path, mode = "wb", quiet = TRUE)
}

finish_levels <- c("Decision", "KO/TKO", "Submission", "Other outcomes")

fights <- read.csv(data_path, stringsAsFactors = FALSE) %>%
  mutate(
    finish_group = case_when(
      grepl("Decision", method) ~ "Decision",
      method %in% c("KO/TKO", "TKO - Doctor's Stoppage") ~ "KO/TKO",
      method == "Submission" ~ "Submission",
      TRUE ~ "Other outcomes"
    ),
    finish_group = factor(finish_group, levels = finish_levels),
    round = factor(round, levels = 1:5)
  ) %>%
  filter(!is.na(round), !is.na(finish_group))

heatmap_data <- fights %>%
  count(finish_group, round, name = "fights") %>%
  complete(finish_group, round, fill = list(fights = 0)) %>%
  group_by(finish_group) %>%
  mutate(
    method_total = sum(fights),
    share_within_method = fights / method_total,
    percent_exact = share_within_method * 100,
    percent_floor = floor(percent_exact),
    remainder_rank = row_number(desc(percent_exact - percent_floor)),
    percent_display = percent_floor + as.integer(
      remainder_rank <= as.integer(round(100 - sum(percent_floor)))
    ),
    label = paste0(percent_display, "%"),
    label_color = if_else(percent_display <= 1, "#92989E", "#FFFFFF")
  ) %>%
  ungroup()

grid_scale <- 0.8
grid_center_x <- 3
grid_center_y <- 2.5
scale_grid_x <- function(x) grid_center_x + (x - grid_center_x) * grid_scale
scale_grid_y <- function(y) grid_center_y + (y - grid_center_y) * grid_scale

heatmap_data <- heatmap_data %>%
  mutate(
    x_center = scale_grid_x(as.integer(round)),
    y_center = scale_grid_y(as.integer(factor(finish_group, levels = rev(finish_levels)))),
    cell_id = row_number()
  )

octagon_radius <- 0.53 * grid_scale
inner_octagon_radius <- 0.485 * grid_scale

make_octagon <- function(x, y, radius = 0.5) {
  theta <- pi / 8 + (0:7) * pi / 4
  data.frame(
    x = x + radius * cos(theta),
    y = y + radius * sin(theta)
  )
}

clip_line_to_poly <- function(poly, slope, intercept) {
  pts <- lapply(seq_len(nrow(poly)), function(i) {
    p1 <- poly[i, ]
    p2 <- poly[ifelse(i == nrow(poly), 1, i + 1), ]
    dx <- p2$x - p1$x
    dy <- p2$y - p1$y
    denom <- dy - slope * dx

    if (abs(denom) < 1e-8) {
      return(NULL)
    }

    t <- (slope * p1$x + intercept - p1$y) / denom
    if (t < -1e-8 || t > 1 + 1e-8) {
      return(NULL)
    }

    data.frame(
      x = p1$x + t * dx,
      y = p1$y + t * dy
    )
  })

  pts <- bind_rows(pts)
  if (nrow(pts) < 2) {
    return(NULL)
  }

  pts <- pts %>%
    distinct(round(x, 6), round(y, 6), .keep_all = TRUE)

  if (nrow(pts) < 2) {
    return(NULL)
  }

  pts <- pts[order(pts$x, pts$y), ][c(1, nrow(pts)), ]
  data.frame(x = pts$x[1], y = pts$y[1], xend = pts$x[2], yend = pts$y[2])
}

make_mesh <- function(x, y, radius = 0.5, spacing = 0.17) {
  poly <- make_octagon(x, y, radius)
  bind_rows(lapply(c(-1, 1), function(slope) {
    center_intercept <- y - slope * x
    bind_rows(lapply(seq(-0.48, 0.48, by = spacing), function(offset) {
      clip_line_to_poly(poly, slope, center_intercept + offset)
    }))
  }))
}

heatmap_octagons <- heatmap_data %>%
  group_by(cell_id) %>%
  group_modify(~ {
    bind_cols(
      .x,
      make_octagon(.x$x_center[1], .x$y_center[1], radius = octagon_radius)
    )
  }) %>%
  ungroup()

heatmap_shadow <- heatmap_octagons %>%
  mutate(x = x + 0.035 * grid_scale, y = y - 0.035 * grid_scale)

heatmap_inner_octagons <- heatmap_data %>%
  group_by(cell_id) %>%
  group_modify(~ {
    bind_cols(
      .x,
      make_octagon(.x$x_center[1], .x$y_center[1], radius = inner_octagon_radius)
    )
  }) %>%
  ungroup()

heatmap_mesh <- heatmap_data %>%
  group_by(cell_id) %>%
  group_modify(~ {
    bind_cols(
      .x,
      make_mesh(
        .x$x_center[1], .x$y_center[1],
        radius = inner_octagon_radius,
        spacing = 0.17 * grid_scale
      )
    )
  }) %>%
  ungroup()

finish_totals <- fights %>%
  count(finish_group, name = "total") %>%
  mutate(
    y = scale_grid_y(as.integer(factor(finish_group, levels = rev(finish_levels)))),
    method_label = as.character(finish_group),
    total_label = paste0(comma(total), " fights")
  )

ufc_wordmark <- readPNG(logo_path)

p <- ggplot() +
  geom_polygon(
    data = heatmap_shadow,
    aes(x, y, group = cell_id),
    fill = "#000000",
    color = NA,
    alpha = 0.52
  ) +
  geom_polygon(
    data = heatmap_octagons,
    aes(x, y, group = cell_id, fill = share_within_method),
    color = "#101010",
    linewidth = 1.45
  ) +
  geom_segment(
    data = heatmap_mesh,
    aes(x = x, y = y, xend = xend, yend = yend, group = cell_id),
    color = "#FFFFFF",
    alpha = 0.045,
    linewidth = 0.18
  ) +
  geom_polygon(
    data = heatmap_inner_octagons,
    aes(x, y, group = cell_id),
    fill = NA,
    color = "#B7BABD",
    alpha = 0.26,
    linewidth = 0.32
  ) +
  geom_text(
    data = heatmap_data,
    aes(x = x_center, y = y_center, label = label, color = label_color),
    fontface = "bold",
    size = 4.1
  ) +
  geom_text(
    data = finish_totals,
    aes(x = 0.72, y = y + 0.08, label = method_label),
    color = "#D20A0A",
    family = "Sternbach",
    fontface = "plain",
    hjust = 1,
    size = 3.2
  ) +
  geom_text(
    data = finish_totals,
    aes(x = 0.72, y = y - 0.08, label = total_label),
    color = "#AEB4BA",
    fontface = "plain",
    hjust = 1,
    size = 2.7
  ) +
  geom_text(
    data = data.frame(round = 1:5, x = scale_grid_x(1:5)),
    aes(x = x, y = 0.78, label = round),
    color = "#D20A0A",
    fontface = "bold",
    size = 4.1
  ) +
  annotate(
    "label",
    x = grid_center_x,
    y = 4.80,
    label = title_text,
    family = "Sternbach",
    color = ufc_red,
    fill = "#FFFFFF",
    size = 7.75,
    label.padding = unit(0.18, "lines"),
    label.r = unit(0, "lines"),
    linewidth = 0
  ) +
  annotate(
    "segment",
    x = 1.05,
    xend = 4.95,
    y = 4.63,
    yend = 4.63,
    color = ufc_red,
    linewidth = 1.1
  ) +
  annotate(
    "text",
    x = grid_center_x,
    y = 4.43,
    label = subtitle_text,
    color = "#D8DEE3",
    size = 2.65
  ) +
  annotate(
    "text",
    x = grid_center_x,
    y = 0.50,
    label = "Ending round",
    color = "#FFFFFF",
    family = "Sternbach",
    fontface = "plain",
    size = 3.9
  ) +
  annotate(
    "text",
    x = grid_center_x,
    y = 0.15,
    label = footer_text,
    color = "#A7ADB3",
    size = 2.1,
    lineheight = 1.2
  ) +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  scale_color_identity() +
  scale_fill_gradientn(
    colors = c("#17191C", "#30343A", "#59616A", "#702833", "#A80F1B", "#C41230", "#FF2A32"),
    values = c(0, 0.08, 0.22, 0.32, 0.5, 0.75, 1),
    limits = c(0, 1),
    breaks = c(0, 0.25, 0.5, 0.75, 1),
    labels = percent,
    name = "Percentage\nwithin method"
  ) +
  labs(
    x = NULL,
    y = NULL
  ) +
  coord_equal(xlim = c(-0.40, 7.70), ylim = c(0.75, 4.25), clip = "off") +
  theme_minimal(base_family = "sans", base_size = 14) +
  theme(
    plot.background = element_rect(fill = background_color, color = NA),
    panel.background = element_rect(fill = background_color, color = NA),
    panel.grid = element_blank(),
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    plot.caption = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "inside",
    legend.position.inside = c(0.79, 0.52),
    legend.justification = c(0.5, 0.5),
    legend.title = element_text(color = "#FFFFFF", face = "bold", size = 8.5),
    legend.text = element_text(color = "#D8DEE3", size = 8),
    plot.margin = margin(38, 24, 34, 20)
  )

output_path <- file.path(out_dir, "ufc_rounds_finish_heatmap.png")
render_path <- tempfile(fileext = ".png")

ragg::agg_png(
  render_path,
  width = render_width_in,
  height = render_height_in,
  units = "in",
  res = render_dpi,
  background = background_color
)
print(p)
grid.raster(
  ufc_wordmark,
  x = unit(0.74, "npc"),
  y = unit(0.075, "npc"),
  width = unit(320 / (render_width_in * render_dpi), "npc"),
  height = unit(112 / (render_height_in * render_dpi), "npc"),
  interpolate = TRUE
)
dev.off()

magick::image_read(render_path) %>%
  magick::image_crop(
    geometry = magick::geometry_area(
      width = content_width_px,
      height = content_height_px,
      x_off = 0,
      y_off = 0
    ),
    repage = TRUE
  ) %>%
  magick::image_border(
    color = "#FFFFFF",
    geometry = paste0(border_width_px, "x", border_width_px)
  ) %>%
  magick::image_border(
    color = ufc_red,
    geometry = paste0(border_width_px, "x", border_width_px)
  ) %>%
  magick::image_write(output_path)

unlink(render_path)

message("Wrote ", output_path)
