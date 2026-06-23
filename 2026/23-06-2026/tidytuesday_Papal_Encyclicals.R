library(ggplot2)
library(dplyr)
library(grid)

out_dir <- "2026/23-06-2026"
data_dir <- "2026/data"

footer_text <- "#TidyTuesday 2026 Week 25 | Source: Vatican.va via TidyTuesday\nWajdi Ben Saad | www.WajdiBenSaad.com | 🦋 wajdi.bsky.social"

enc <- read.csv(file.path(data_dir, "encyclicals.csv"), stringsAsFactors = FALSE)

stop_words <- c(
  "a", "about", "above", "after", "again", "against", "all", "also", "am",
  "an", "and", "any", "are", "as", "at", "be", "because", "been", "before",
  "being", "below", "between", "both", "but", "by", "can", "cannot", "could",
  "did", "do", "does", "doing", "down", "during", "each", "few", "for",
  "from", "further", "had", "has", "have", "having", "he", "her", "here",
  "hers", "herself", "him", "himself", "his", "how", "i", "if", "in",
  "into", "is", "it", "its", "itself", "just", "me", "more", "most", "my",
  "myself", "no", "nor", "not", "now", "of", "off", "on", "once", "only",
  "or", "other", "our", "ours", "ourselves", "out", "over", "own", "same",
  "she", "should", "so", "some", "such", "than", "that", "the", "their",
  "theirs", "them", "themselves", "then", "there", "these", "they", "this",
  "those", "through", "to", "too", "under", "until", "up", "very", "was",
  "we", "were", "what", "when", "where", "which", "while", "who", "whom",
  "why", "will", "with", "you", "your", "yours", "yourself", "yourselves",
  "may", "must", "shall", "would", "one", "two", "upon", "therefore",
  "thus", "even", "every", "many", "much", "well", "yet", "way", "ways",
  "thing", "things", "without", "means", "make", "made", "become",
  "becomes", "rather", "however", "indeed", "among", "first", "second",
  "since", "within", "toward", "another", "like", "let", "said", "good",
  "common", "public", "state", "states", "new", "great", "person",
  "social", "forms", "terms", "time", "today", "use"
)

tokenize <- function(text) {
  text <- tolower(text)
  text <- gsub("artificial intelligence", "artificial_intelligence", text)
  text <- gsub("\\bai\\b|\\ba\\.i\\.\\b", "artificial_intelligence", text)
  text <- gsub("[^a-z_ ]", " ", text)

  words <- unlist(strsplit(text, "\\s+"))
  words <- ifelse(words == "rights", "right", words)
  words <- ifelse(words == "men", "man", words)
  words <- ifelse(words == "humanity", "human", words)
  words <- ifelse(words == "society", "social", words)
  words <- ifelse(words == "people", "person", words)
  words <- words[nchar(words) > 2]
  words <- words[!words %in% stop_words]

  data.frame(word = words, stringsAsFactors = FALSE)
}

tokens <- do.call(
  rbind,
  lapply(seq_len(nrow(enc)), function(i) {
    out <- tokenize(enc$text[i])
    if (nrow(out) == 0) return(NULL)
    out$encyclical <- enc$encyclical[i]
    out
  })
)

word_counts <- tokens %>%
  count(encyclical, word, name = "n")

wide <- reshape(word_counts, idvar = "word", timevar = "encyclical", direction = "wide")
names(wide) <- sub("^n\\.", "", names(wide))
wide[is.na(wide)] <- 0
wide$`Rerum Novarum` <- as.numeric(wide$`Rerum Novarum`)
wide$`Magnifica Humanitas` <- as.numeric(wide$`Magnifica Humanitas`)

shared_words <- wide %>%
  filter(`Rerum Novarum` > 0, `Magnifica Humanitas` > 0) %>%
  arrange(desc(`Magnifica Humanitas`)) %>%
  slice_head(n = 20) %>%
  arrange(word) %>%
  mutate(
    word_display = factor(word, levels = rev(word)),
    direction = case_when(
      `Rerum Novarum` > `Magnifica Humanitas` * 1.25 ~ "More common in 1891",
      `Magnifica Humanitas` > `Rerum Novarum` * 1.25 ~ "More common in 2026",
      TRUE ~ "Similar frequency"
    )
  )

p_dumbbell <- ggplot(shared_words, aes(y = word_display)) +
  geom_segment(
    aes(
      x = `Rerum Novarum`,
      xend = `Magnifica Humanitas`,
      yend = word_display,
      color = direction
    ),
    linewidth = 2.1,
    alpha = 0.7,
    lineend = "round"
  ) +
  geom_point(aes(x = `Rerum Novarum`), size = 4.2, color = "#007C89") +
  geom_point(aes(x = `Magnifica Humanitas`), size = 4.2, color = "#E56A24") +
  geom_label(
    aes(
      x = `Rerum Novarum`,
      label = `Rerum Novarum`,
      hjust = ifelse(`Magnifica Humanitas` < `Rerum Novarum`, -0.75, 1.75)
    ),
    position = position_nudge(y = 0.24),
    size = 3.05,
    fontface = "bold",
    color = "#007C89",
    fill = "#FFF9EF",
    linewidth = 0,
    label.padding = unit(0.06, "lines")
  ) +
  geom_label(
    aes(
      x = `Magnifica Humanitas`,
      label = `Magnifica Humanitas`,
      hjust = ifelse(`Magnifica Humanitas` < `Rerum Novarum`, 1.75, -0.75)
    ),
    position = position_nudge(y = -0.24),
    size = 3.05,
    fontface = "bold",
    color = "#C56A2D",
    fill = "#FFF9EF",
    linewidth = 0,
    label.padding = unit(0.06, "lines")
  ) +
  annotate("text", x = 6, y = 21.15, label = "1891", color = "#007C89", fontface = "bold", size = 4.3) +
  annotate("text", x = 286, y = 21.15, label = "2026", color = "#C56A2D", fontface = "bold", size = 4.3) +
  scale_color_manual(values = c(
    "More common in 1891" = "#007C89",
    "Similar frequency" = "#9A8F83",
    "More common in 2026" = "#E56A24"
  )) +
  scale_x_continuous(expand = expansion(mult = c(0.12, 0.16))) +
  labs(
    title = "From Leo XIII to Leo XIV: Shared Words, Different Weight",
    subtitle = "Shared words in Rerum Novarum (1891) and Magnifica Humanitas (2026).\nRanked by frequency in Leo XIV's letter.",
    x = "Word frequency",
    y = NULL,
    color = NULL,
    caption = footer_text
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.background = element_rect(fill = "#FFF9EF", color = NA),
    panel.background = element_rect(fill = "#FFF9EF", color = NA),
    panel.grid.major.y = element_line(color = "#E9DFD2", linewidth = 0.35),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "#DDD2C5", linewidth = 0.35),
    axis.text.y = element_text(face = "bold", color = "#2E2A27", size = 11),
    axis.text.x = element_text(color = "#746E68"),
    axis.title.x = element_text(color = "#746E68", margin = margin(t = 10)),
    plot.title = element_text(face = "bold", size = 22.5, color = "#1F2A2A"),
    plot.subtitle = element_text(size = 13, color = "#3E4542", margin = margin(b = 18)),
    plot.caption = element_text(size = 9.3, color = "#746E68", hjust = 0),
    legend.position = "none",
    plot.margin = margin(22, 48, 28, 48)
  ) +
  coord_cartesian(clip = "off")

ggsave(
  file.path(out_dir, "26_shared_vocabulary_dumbbell.png"),
  p_dumbbell,
  width = 11,
  height = 8,
  dpi = 320,
  bg = "#FFF9EF"
)

message("Wrote ", file.path(out_dir, "26_shared_vocabulary_dumbbell.png"))
