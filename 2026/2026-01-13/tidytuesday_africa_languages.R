library(tidyverse)
library(ggtext)
library(scales)
library(maps)
library(sp)
library(systemfonts)

out_dir <- "2026/2026-01-13"
data_path <- "2026/data/africa_languages_2026-01-13.csv"
pattern_side_path <- "2026/2026-01-13/resources/vecteezy_pattern_side.png"

map_xmin <- -20
map_xmax <- 55
map_ymin <- -36
map_ymax <- 42

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

languages_raw <- read.csv(data_path, stringsAsFactors = FALSE)

papakilo_font_path <- "/Users/dix/Library/Fonts/PapaKilo - Decorative.ttf"
if (file.exists(papakilo_font_path)) {
  register_font(name = "PapaKilo Decorative", plain = papakilo_font_path)
}

footer_text <- "#TidyTuesday 2026 Week 02 | Source: The Languages of Africa via TidyTuesday\nWajdi Ben Saad | www.WajdiBenSaad.com | Bluesky: wajdi.bsky.social"

paper <- "#FFFFFF"
ink <- "#1D1A16"
muted <- "#6B6258"

paper_variants <- c(
  parchment = "#F6EEDC",
  warm_ivory = "#FAF4E8",
  sand = "#EFE2C8"
)

family_palette <- c(
  "Niger-Congo" = "#257f08",
  "Afroasiatic" = "#F0A202",
  "Nilo-Saharan" = "#9c0400",
  "Khoisan" = "#B86604",
  "Austronesian" = "#DD8200",
  "Indo-European / Creole" = "#A36031",
  "Other" = "#C9B458"
)

country_recode <- c(
  "Congo" = "Democratic Republic of the Congo",
  "Eswatini" = "Swaziland"
)

clean_family <- function(x) {
  case_when(
    x %in% c("Afroasiatic", "Afro-Asiatic", "Arabic-based") ~ "Afroasiatic",
    x %in% c("Niger–Congo", "Mande", "Ubangian", "Kongo-based") ~ "Niger-Congo",
    x %in% c("Kxʼa", "Khoe–Kwadi", "Tuu") ~ "Khoisan",
    x %in% c("English", "French", "Portuguese", "Indo-European") ~ "Indo-European / Creole",
    x %in% c("Nilo-Saharan", "Austronesian") ~ x,
    TRUE ~ "Other"
  )
}

save_plot <- function(filename, plot, width = 11, height = 9, bg = paper) {
  ggsave(
    file.path(out_dir, filename),
    plot,
    width = width,
    height = height,
    dpi = 320,
    bg = bg
  )
}

add_full_canvas_pattern <- function(input_file, output_file) {
  dimensions <- system2(
    "identify",
    c("-format", "%wx%h", input_file),
    stdout = TRUE
  )

  dimensions <- as.integer(strsplit(dimensions, "x")[[1]])
  geometry <- paste0(dimensions[1], "x", dimensions[2])
  side_width <- round(dimensions[1] * 0.065)

  system2(
    "convert",
    c(
      input_file,
      "\\(",
      pattern_side_path,
      "-resize", paste0(side_width, "x", dimensions[2], "!"),
      "\\)",
      "-gravity", "west",
      "-composite",
      "\\(",
      pattern_side_path,
      "-flop",
      "-resize", paste0(side_width, "x", dimensions[2], "!"),
      "\\)",
      "-gravity", "east",
      "-composite",
      output_file
    )
  )
}

composite_on_background <- function(background_file, input_file, output_file) {
  system2(
    "convert",
    c(
      background_file,
      input_file,
      "-compose", "over",
      "-composite",
      output_file
    )
  )
}

languages <- languages_raw %>%
  distinct(language, family, country, .keep_all = TRUE) %>%
  mutate(
    native_speakers = suppressWarnings(as.numeric(native_speakers)),
    family_clean = clean_family(family),
    map_country = recode(country, !!!country_recode, .default = country)
  )

africa_regions <- c(
  "Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", "Burundi",
  "Cameroon", "Cape Verde", "Central African Republic", "Chad", "Comoros",
  "Democratic Republic of the Congo", "Djibouti", "Egypt", "Equatorial Guinea",
  "Eritrea", "Ethiopia", "Gabon", "Gambia", "Ghana", "Guinea",
  "Guinea-Bissau", "Ivory Coast", "Kenya", "Lesotho", "Liberia", "Libya",
  "Madagascar", "Malawi", "Mali", "Mauritania", "Mauritius", "Morocco",
  "Mozambique", "Namibia", "Niger", "Nigeria", "Republic of Congo",
  "Rwanda", "Senegal", "Seychelles", "Sierra Leone", "Somalia",
  "South Africa", "South Sudan", "Sudan", "Swaziland", "Tanzania", "Togo", "Tunisia",
  "Uganda", "Western Sahara", "Zambia", "Zimbabwe"
)

africa_map <- map_data("world") %>%
  filter(region %in% africa_regions)

single_overlay_families <- names(family_palette)

single_overlay_reach <- languages %>%
  count(map_country, family_clean, name = "languages") %>%
  bind_rows(
    languages %>%
      filter(map_country == "Morocco") %>%
      count(family_clean, name = "languages") %>%
      mutate(map_country = "Western Sahara")
  ) %>%
  bind_rows(
    languages %>%
      filter(map_country == "Democratic Republic of the Congo") %>%
      count(family_clean, name = "languages") %>%
      mutate(map_country = "Republic of Congo")
  ) %>%
  bind_rows(
    languages %>%
      filter(map_country == "Guinea") %>%
      count(family_clean, name = "languages") %>%
      mutate(map_country = "Guinea-Bissau")
  ) %>%
  group_by(map_country) %>%
  mutate(country_total = sum(languages), share = languages / country_total) %>%
  ungroup()

set.seed(1301)

african_script_glyphs <- c(
  letters,
  LETTERS,
  "ا", "ب", "ت", "ج", "ح", "د", "ر", "س", "ع", "ف", "ق", "ك", "ل", "م", "ن", "ه", "و", "ي",
  "ሀ", "ለ", "መ", "ሠ", "ረ", "ሰ", "ቀ", "በ", "ተ", "ነ", "አ", "ከ", "ወ", "ዘ", "የ", "ገ", "ጠ", "ፈ",
  "ⴰ", "ⴱ", "ⴳ", "ⴷ", "ⴹ", "ⴼ", "ⴽ", "ⵀ", "ⵃ", "ⵎ", "ⵏ", "ⵔ", "ⵙ", "ⵛ", "ⵜ", "ⵡ", "ⵢ",
  "ߊ", "ߋ", "ߌ", "ߍ", "ߎ", "ߏ", "ߐ", "ߓ", "ߕ", "ߖ", "ߘ", "ߛ", "ߞ", "ߟ", "ߡ", "ߣ", "ߦ",
  "𞤀", "𞤁", "𞤂", "𞤃", "𞤄", "𞤅", "𞤆", "𞤇", "𞤈", "𞤉", "𞤊", "𞤋", "𞤌", "𞤍",
  "ꕉ", "ꔀ", "ꔤ", "ꔧ", "ꔬ", "ꕆ", "ꕎ", "ꕙ", "ꕢ", "ꕩ", "ꕱ", "ꕺ", "ꖇ", "ꖏ", "ꖕ",
  "ꚠ", "ꚡ", "ꚢ", "ꚣ", "ꚤ", "ꚥ", "ꚦ", "ꚧ", "ꚨ", "ꚩ", "ꚪ", "ꚫ",
  "𐒀", "𐒁", "𐒂", "𐒃", "𐒄", "𐒅", "𐒆", "𐒇", "𐒈", "𐒉"
)

background_glyphs <- tibble(
  x = runif(520, map_xmin - 8, map_xmax + 8),
  y = runif(520, map_ymin, map_ymax),
  label = sample(african_script_glyphs, 520, replace = TRUE),
  size = runif(520, 2.0, 7.2),
  angle = runif(520, -45, 45),
  alpha_value = runif(520, 0.055, 0.18),
  color = sample(c("#1B8A5A", "#F0A202", "#2D9CDB", "#7B61FF", "#6B5E7A", "#1D1A16"), 520, replace = TRUE)
)

full_canvas_glyphs <- tibble(
  x = runif(900, 0.03, 0.97),
  y = runif(900, 0.03, 0.97),
  label = sample(african_script_glyphs, 900, replace = TRUE),
  size = runif(900, 2.0, 8.5),
  angle = runif(900, -45, 45),
  alpha_value = runif(900, 0.035, 0.145),
  color = sample(c("#257f08", "#F0A202", "#9c0400", "#DD8200", "#A36031", "#1D1A16"), 900, replace = TRUE)
)

make_letter_background <- function(paper) {
  ggplot(full_canvas_glyphs, aes(x, y, label = label, size = size, angle = angle, alpha = alpha_value, color = color)) +
    geom_text(show.legend = FALSE, family = "sans") +
    scale_color_identity() +
    scale_alpha_identity() +
    scale_size_identity() +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE, clip = "off") +
    theme_void() +
    theme(
      plot.background = element_rect(fill = paper, color = NA),
      panel.background = element_rect(fill = paper, color = NA),
      plot.margin = margin(0, 0, 0, 0)
    )
}

hex_points <- function(cx, cy, radius = 0.5) {
  angle <- pi / 6 + seq(0, 2 * pi, length.out = 7)[-7]
  tibble(
    vertex_id = seq_along(angle),
    px = cx + radius * cos(angle),
    py = cy + radius * sin(angle)
  )
}

hex_radius <- 0.9
hex_dx <- sqrt(3) * hex_radius
hex_dy <- 1.5 * hex_radius

hex_centers <- expand_grid(
  row_id = seq_len(ceiling((map_ymax - map_ymin) / hex_dy) + 2),
  col_id = seq_len(ceiling((map_xmax - map_xmin) / hex_dx) + 2)
) %>%
  mutate(
    center_y = map_ymax - (row_id - 1) * hex_dy,
    center_x = map_xmin + (col_id - 1) * hex_dx + if_else(row_id %% 2 == 0, hex_dx / 2, 0),
    hex_id = row_number()
  ) %>%
  filter(
    between(center_x, map_xmin - hex_radius, map_xmax + hex_radius),
    between(center_y, map_ymin - hex_radius, map_ymax + hex_radius)
  )

assign_hex_country <- function(country_poly, centers) {
  country_name <- unique(country_poly$region)
  country_bbox <- country_poly %>%
    summarise(
      xmin = min(long, na.rm = TRUE) - hex_radius,
      xmax = max(long, na.rm = TRUE) + hex_radius,
      ymin = min(lat, na.rm = TRUE) - hex_radius,
      ymax = max(lat, na.rm = TRUE) + hex_radius,
      .groups = "drop"
    )

  candidates <- centers %>%
    filter(
      between(center_x, country_bbox$xmin, country_bbox$xmax),
      between(center_y, country_bbox$ymin, country_bbox$ymax)
    )

  if (nrow(candidates) == 0) {
    return(tibble())
  }

  inside <- rep(FALSE, nrow(candidates))

  for (poly_group in unique(country_poly$group)) {
    piece <- country_poly %>%
      filter(group == poly_group) %>%
      arrange(order)

    inside <- inside | sp::point.in.polygon(
      candidates$center_x,
      candidates$center_y,
      piece$long,
      piece$lat
    ) > 0
  }

  candidates %>%
    filter(inside) %>%
    mutate(map_country = country_name)
}

country_hexes <- africa_map %>%
  group_by(region) %>%
  group_split() %>%
  map_dfr(assign_hex_country, centers = hex_centers) %>%
  distinct(hex_id, .keep_all = TRUE)

allocate_country_hexes <- function(country_data, family_data) {
  country_name <- unique(country_data$map_country)
  n_hex <- nrow(country_data)
  mix <- family_data %>%
    filter(map_country == country_name, share > 0) %>%
    arrange(factor(family_clean, levels = single_overlay_families))

  if (n_hex == 0 || nrow(mix) == 0) {
    return(tibble())
  }

  raw_counts <- mix$share * n_hex
  counts <- floor(raw_counts)
  remainder <- n_hex - sum(counts)

  if (remainder > 0) {
    add_to <- order(raw_counts - counts, decreasing = TRUE)[seq_len(remainder)]
    counts[add_to] <- counts[add_to] + 1
  }

  family_assignments <- rep(as.character(mix$family_clean), counts)

  country_data %>%
    arrange(desc(center_y), center_x) %>%
    mutate(family_clean = factor(family_assignments, levels = single_overlay_families))
}

tessellated_hexes <- country_hexes %>%
  group_by(map_country) %>%
  group_split() %>%
  map_dfr(allocate_country_hexes, family_data = single_overlay_reach)

tessellated_hex_polygons <- tessellated_hexes %>%
  rowwise() %>%
  mutate(points = list(hex_points(center_x, center_y, hex_radius * 0.96))) %>%
  unnest(points) %>%
  ungroup()

tessellated_country_edges <- tessellated_hex_polygons %>%
  arrange(hex_id, vertex_id) %>%
  group_by(hex_id, map_country) %>%
  mutate(
    xend = lead(px, default = first(px)),
    yend = lead(py, default = first(py))
  ) %>%
  ungroup() %>%
  mutate(
    x1 = pmin(px, xend),
    y1 = if_else(px <= xend, py, yend),
    x2 = pmax(px, xend),
    y2 = if_else(px <= xend, yend, py),
    edge_key = paste(round(x1, 3), round(y1, 3), round(x2, 3), round(y2, 3), sep = "_")
  ) %>%
  group_by(edge_key) %>%
  filter(n_distinct(map_country) > 1) %>%
  slice(1) %>%
  ungroup()

hex_row_edges <- tessellated_hexes %>%
  mutate(edge_band = round(center_y / hex_dy)) %>%
  group_by(edge_band) %>%
  summarise(
    left_edge = min(center_x - hex_dx / 2, na.rm = TRUE),
    right_edge = max(center_x + hex_dx / 2, na.rm = TRUE),
    .groups = "drop"
  )

spread_label_y <- function(y, min_gap = 1.45) {
  if (length(y) <= 1) {
    return(y)
  }

  y_new <- y
  order_y <- order(y_new)

  for (i in order_y[-1]) {
    previous <- order_y[which(order_y == i) - 1]
    if (y_new[i] - y_new[previous] < min_gap) {
      y_new[i] <- y_new[previous] + min_gap
    }
  }

  y_new
}

tessellated_labels <- tessellated_hexes %>%
  group_by(map_country) %>%
  summarise(
    n_hexes = n(),
    center_x = mean(center_x),
    center_y = mean(center_y),
    .groups = "drop"
  ) %>%
  filter(n_hexes >= 10 | map_country == "Tunisia") %>%
  mutate(edge_band = round(center_y / hex_dy)) %>%
  left_join(hex_row_edges, by = "edge_band") %>%
  mutate(
    label_side = case_when(
      map_country %in% c("Morocco", "Algeria", "Tunisia", "Libya") ~ "top",
      map_country == "South Africa" ~ "left",
      map_country == "Mozambique" ~ "right",
      map_country == "Zimbabwe" ~ "right",
      map_country == "Cameroon" ~ "right",
      map_country == "Central African Republic" ~ "left",
      map_country == "Ghana" ~ "bottom",
      TRUE ~ if_else(center_x < (left_edge + right_edge) / 2, "left", "right")
    ),
    label_x = case_when(
      map_country == "Central African Republic" ~ 6.64,
      map_country == "Cameroon" ~ 3.6,
      map_country == "Nigeria" ~ 3.2,
      label_side == "left" ~ left_edge - 2.2,
      map_country == "Mozambique" ~ center_x + 3.6,
      map_country == "Zimbabwe" ~ center_x + 7.6,
      map_country == "Ghana" ~ center_x,
      label_side == "right" ~ right_edge + 2.2,
      TRUE ~ center_x
    ),
    label_y = case_when(
      label_side == "top" ~ max(tessellated_hexes$center_y + hex_radius, na.rm = TRUE) + 1.25,
      map_country == "Mozambique" ~ center_y - 2.2,
      map_country == "Zimbabwe" ~ center_y - 4.2,
      map_country == "Cameroon" ~ center_y - 2.6,
      map_country == "Central African Republic" ~ 0.55,
      map_country == "Nigeria" ~ 3,
      map_country == "Ghana" ~ center_y - 4.3,
      map_country == "South Africa" ~ center_y - 3.0,
      TRUE ~ center_y
    ),
    hjust_value = case_when(
      label_side == "left" ~ 1,
      label_side %in% c("top", "bottom") ~ 0.5,
      TRUE ~ 0
    )
  ) %>%
  group_by(label_side) %>%
  arrange(label_y, .by_group = TRUE) %>%
  mutate(label_y = if_else(label_side %in% c("left", "right"), spread_label_y(label_y), label_y)) %>%
  ungroup()

make_option_17 <- function(paper) {
  ggplot() +
  geom_polygon(
    data = tessellated_hex_polygons,
    aes(px, py, group = hex_id, fill = family_clean),
    color = "#FFFFFF",
    linewidth = 0.12
  ) +
  geom_segment(
    data = tessellated_country_edges,
    aes(x = px, y = py, xend = xend, yend = yend),
    inherit.aes = FALSE,
    color = alpha("#1D1A16", 0.34),
    linewidth = 0.28,
    lineend = "round"
  ) +
  geom_segment(
    data = tessellated_labels,
    aes(x = center_x, y = center_y, xend = label_x, yend = label_y),
    color = alpha(ink, 0.34),
    linewidth = 0.18
  ) +
  geom_text(
    data = tessellated_labels,
    aes(label_x, label_y, label = map_country),
    inherit.aes = FALSE,
    size = 2.25,
    color = ink,
    hjust = tessellated_labels$hjust_value
  ) +
  scale_fill_manual(values = family_palette[single_overlay_families], drop = FALSE) +
  scale_color_identity() +
  scale_alpha_identity() +
  scale_size_identity() +
  coord_quickmap(xlim = c(map_xmin + 1, map_xmax - 1), ylim = c(map_ymin, map_ymax - 3), expand = FALSE, clip = "off") +
  labs(
    title = paste0(
      "<span style='font-family:\"PapaKilo Decorative\"; font-size:75pt; font-weight:400;'>The Languages of Africa</span><br>",
      "<span style='font-size:11.5pt; font-weight:400; color:#6B6258;'>",
      "A map of the spread of language families across the African continent",
      "</span>"
    ),
    fill = "Language Family",
    caption = footer_text
  ) +
  theme_void(base_size = 11) +
  theme(
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.title = element_textbox_simple(
      color = ink,
      face = "bold",
      size = 24,
      halign = 0.5,
      fill = alpha(paper, 0.9),
      box.color = NA,
      linetype = 0,
      linewidth = 0,
      width = unit(1, "npc"),
      lineheight = 1.15,
      padding = margin(10, 14, 10, 14),
      margin = margin(b = 16)
    ),
    plot.caption = element_text(color = muted, size = 8.2, hjust = 0.5, margin = margin(t = 12)),
    legend.position = "bottom",
    legend.justification = "center",
    legend.background = element_rect(fill = alpha(paper, 0.92), color = "#D6E2EA", linewidth = 0.35),
    legend.box.background = element_rect(fill = alpha(paper, 0.92), color = "#D6E2EA", linewidth = 0.35),
    legend.margin = margin(14, 10, 8, 10),
    legend.title = element_text(color = ink, face = "bold", size = 10.5),
    legend.text = element_text(color = muted, size = 9.5),
    legend.key.size = unit(0.42, "cm"),
    plot.margin = margin(6, 8, 8, 8)
  ) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))
}

background_preview <- file.path(out_dir, "africa_languages_map_background.png")
base_preview <- file.path(out_dir, "africa_languages_map_base.png")
composited_preview <- file.path(out_dir, "africa_languages_map_composited.png")
final_preview <- file.path(out_dir, "africa_languages_map.png")
final_paper <- paper_variants[["sand"]]

save_plot(
  basename(background_preview),
  make_letter_background(final_paper),
  width = 14,
  height = 10.5,
  bg = final_paper
)

save_plot(
  basename(base_preview),
  make_option_17(final_paper),
  width = 14,
  height = 10.5,
  bg = "transparent"
)
composite_on_background(background_preview, base_preview, composited_preview)
add_full_canvas_pattern(composited_preview, final_preview)
unlink(c(background_preview, base_preview, composited_preview))

message("Wrote final Africa language chart to ", final_preview)
