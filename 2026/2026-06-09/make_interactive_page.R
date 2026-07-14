library(dplyr)
library(jsonlite)
library(base64enc)

out_dir <- "2026/2026-06-09"
data_path <- "2026/data/game_films_2026-06-09.csv"
font_path <- file.path(out_dir, "resources", "fonts", "PressStart2P-Regular.ttf")
output_path <- file.path(out_dir, "video_game_movies_scores_interactive.html")

films <- read.csv(data_path, stringsAsFactors = FALSE) %>%
  mutate(
    release_date = as.Date(release_date),
    year = as.integer(format(release_date, "%Y")),
    score = as.numeric(rotten_tomatoes),
    decade = floor(year / 10) * 10
  ) %>%
  filter(!is.na(release_date), !is.na(score)) %>%
  arrange(year, score, title) %>%
  group_by(year) %>%
  mutate(
    x_offset = if (n() == 1) 0 else seq(-0.30, 0.30, length.out = n()),
    plot_year = year + x_offset
  ) %>%
  ungroup() %>%
  transmute(
    title,
    release_date = format(release_date, "%d %B %Y"),
    year,
    decade,
    plot_year,
    score,
    metacritic,
    cinema_score = ifelse(cinema_score %in% c("", "N/A"), NA, cinema_score),
    director = ifelse(director == "", NA, director),
    publisher = ifelse(original_game_publisher == "", NA, original_game_publisher),
    format = category
  )

trend <- data.frame(year = seq(min(films$year), max(films$year))) %>%
  rowwise() %>%
  mutate(
    films_in_window = sum(abs(films$year - year) <= 2),
    score = median(films$score[abs(films$year - year) <= 2], na.rm = TRUE)
  ) %>%
  ungroup() %>%
  filter(films_in_window >= 3)

decades <- films %>%
  filter(decade %in% c(1990, 2000, 2010, 2020)) %>%
  group_by(decade) %>%
  summarise(
    median_score = median(score),
    films = n(),
    .groups = "drop"
  ) %>%
  mutate(level = paste0("LEVEL ", row_number()))

labels <- data.frame(
  title = c(
    "Super Mario Bros.",
    "Alone in the Dark",
    "Resident Evil: Damnation",
    "Werewolves Within",
    "Sonic the Hedgehog 3"
  ),
  text = c(
    "SUPER MARIO BROS.",
    "ALONE IN THE DARK",
    "RESIDENT EVIL: DAMNATION",
    "WEREWOLVES WITHIN",
    "SONIC 3"
  ),
  dx = c(18, -10, 0, -18, 8),
  dy = c(-15, -20, 30, 6, 30),
  anchor = c("start", "end", "middle", "end", "start")
) %>%
  left_join(films %>% select(title, plot_year, score), by = "title")

font_base64 <- base64encode(font_path)
films_json <- toJSON(films, dataframe = "rows", na = "null", auto_unbox = TRUE)
trend_json <- toJSON(trend, dataframe = "rows", na = "null", auto_unbox = TRUE)
decades_json <- toJSON(decades, dataframe = "rows", na = "null", auto_unbox = TRUE)
labels_json <- toJSON(labels, dataframe = "rows", na = "null", auto_unbox = TRUE)

page <- r"---(<!doctype html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <meta name="theme-color" content="#080A12">
  <meta name="description" content="An interactive exploration of Rotten Tomatoes scores for films based on video games.">
  <title>Do Video-Game Movies Finally Get Good?</title>
  <style>
    @font-face {
      font-family: "Press Start 2P";
      src: url(data:font/ttf;base64,__FONT__) format("truetype");
      font-weight: 400;
      font-style: normal;
      font-display: swap;
    }
    :root {
      --bg: #080A12;
      --panel: #0D1020;
      --grid: #26304B;
      --white: #F4F7FF;
      --muted: #AAB3CB;
      --cyan: #3CF2FF;
      --pink: #FF2B55;
      --yellow: #FFE66D;
    }
    * { box-sizing: border-box; }
    html { background: var(--bg); }
    body {
      margin: 0;
      min-width: 320px;
      background: var(--bg);
      color: var(--white);
      font-family: Arial, Helvetica, sans-serif;
    }
    .page {
      min-height: 100vh;
      border: 3px solid var(--pink);
      background:
        linear-gradient(rgba(255,255,255,.012) 1px, transparent 1px),
        var(--bg);
      background-size: 100% 4px;
    }
    main {
      width: min(1500px, 100%);
      margin: 0 auto;
      padding: 54px 46px 34px;
    }
    header { text-align: center; }
    h1 {
      margin: 0;
      font-family: "Press Start 2P", monospace;
      font-size: 34px;
      line-height: 1.55;
      font-weight: 400;
      letter-spacing: 0;
      text-shadow: 0 0 18px rgba(60,242,255,.08);
    }
    .subtitle {
      margin: 18px auto 34px;
      color: var(--muted);
      font-size: 18px;
      line-height: 1.5;
    }
    .controls {
      display: flex;
      justify-content: space-between;
      align-items: center;
      gap: 16px;
      width: 70%;
      margin: 0 auto 12px;
    }
    .hint {
      color: var(--cyan);
      font-family: "Press Start 2P", monospace;
      font-size: 9px;
      line-height: 1.6;
    }
    button {
      border: 1px solid var(--grid);
      border-radius: 0;
      padding: 11px 13px;
      background: var(--panel);
      color: var(--white);
      font-family: "Press Start 2P", monospace;
      font-size: 9px;
      cursor: pointer;
    }
    button:hover, button:focus-visible {
      border-color: var(--cyan);
      color: var(--cyan);
      outline: none;
      box-shadow: 0 0 12px rgba(60,242,255,.18);
    }
    .levels {
      display: grid;
      grid-template-columns: repeat(4, minmax(0, 1fr));
      gap: 14px;
      width: 70%;
      margin: 0 auto 14px;
    }
    .level-card {
      position: relative;
      min-height: 78px;
      padding: 14px 9px 16px;
      border: 1px solid var(--grid);
      background: var(--panel);
      text-align: center;
      cursor: pointer;
    }
    .level-card::after {
      content: "";
      position: absolute;
      left: 0;
      right: 0;
      bottom: 0;
      height: 5px;
      background: var(--pink);
    }
    .level-card:hover, .level-card:focus-visible, .level-card.active {
      border-color: var(--cyan);
      outline: none;
      box-shadow: 0 0 16px rgba(60,242,255,.16);
    }
    .level-title {
      color: var(--cyan);
      font-family: "Press Start 2P", monospace;
      font-size: 13px;
      line-height: 1.4;
    }
    .level-summary {
      margin-top: 9px;
      font-family: "Press Start 2P", monospace;
      font-size: 8px;
      line-height: 1.45;
    }
    .chart-shell {
      width: 70%;
      margin: 0 auto;
      overflow: hidden;
      border: 1px solid var(--grid);
      background: var(--panel);
    }
    svg {
      display: block;
      width: 100%;
      min-width: 0;
      height: auto;
      background: var(--panel);
    }
    .pixel {
      cursor: pointer;
      transition: opacity 150ms ease, filter 150ms ease, transform 150ms ease;
      transform-box: fill-box;
      transform-origin: center;
    }
    .pixel:hover, .pixel:focus, .pixel.selected {
      stroke: var(--white);
      stroke-width: 2.5;
      filter: drop-shadow(0 0 6px var(--cyan));
      transform: scale(1.35);
      outline: none;
    }
    .pixel.dimmed { opacity: .12; }
    .trend-hit { cursor: help; }
    .tooltip {
      position: fixed;
      z-index: 20;
      width: min(330px, calc(100vw - 28px));
      padding: 13px 15px;
      border: 2px solid var(--cyan);
      background: rgba(8,10,18,.98);
      color: var(--white);
      box-shadow: 0 0 18px rgba(60,242,255,.32);
      pointer-events: none;
      font-size: 13px;
      line-height: 1.5;
    }
    .tooltip[hidden] { display: none; }
    .tooltip-title {
      margin-bottom: 8px;
      color: var(--cyan);
      font-family: "Press Start 2P", monospace;
      font-size: 10px;
      line-height: 1.5;
    }
    .tooltip-score { color: var(--yellow); font-weight: 700; }
    .legend {
      width: min(420px, 72%);
      margin: 24px auto 0;
      text-align: center;
    }
    .legend-title {
      margin-bottom: 9px;
      color: var(--cyan);
      font-family: "Press Start 2P", monospace;
      font-size: 10px;
    }
    .legend-bar {
      height: 14px;
      border: 1px solid var(--grid);
      background: linear-gradient(90deg, #FF2B55, #FF6438 25%, #FFE66D 50%, #3CF2FF 75%, #7CFFB2);
    }
    .legend-labels {
      display: flex;
      justify-content: space-between;
      margin-top: 7px;
      color: var(--muted);
      font-family: "Press Start 2P", monospace;
      font-size: 8px;
    }
    footer {
      margin-top: 34px;
      text-align: center;
      color: var(--muted);
      font-size: 12px;
      line-height: 1.7;
    }
    @media (max-width: 850px) {
      main { padding: 34px 18px 26px; }
      h1 { font-size: 22px; line-height: 1.55; }
      .subtitle { font-size: 15px; }
      .controls, .levels, .chart-shell { width: 100%; }
      .levels { grid-template-columns: repeat(2, minmax(0, 1fr)); }
      .controls { align-items: flex-start; flex-direction: column; }
    }
    @media (max-width: 470px) {
      h1 { font-size: 17px; }
      .levels { grid-template-columns: 1fr; }
    }
    @media (prefers-reduced-motion: reduce) {
      .pixel { transition: none; }
    }
  </style>
</head>
<body>
  <div class="page">
    <main>
      <header>
        <h1>DO VIDEO-GAME MOVIES<br>FINALLY GET GOOD?</h1>
        <p class="subtitle">Three decades of Rotten Tomatoes scores suggest the genre may have found an extra life&nbsp; // &nbsp;73 reviewed releases</p>
      </header>

      <div class="controls">
        <div class="hint">HOVER PIXELS FOR DETAILS&nbsp; // &nbsp;SELECT A LEVEL TO FILTER</div>
        <button id="reset" type="button">RESET VIEW</button>
      </div>

      <section id="levels" class="levels" aria-label="Decade summaries"></section>

      <div class="chart-shell">
        <svg id="chart" viewBox="0 0 1400 620" role="img" aria-labelledby="chart-title chart-desc">
          <title id="chart-title">Rotten Tomatoes scores for films based on video games from 1993 to 2026</title>
          <desc id="chart-desc">Each square represents a film. A cyan stepped line shows the rolling five-year median score.</desc>
        </svg>
      </div>

      <div class="legend" aria-label="Critic score color scale">
        <div class="legend-title">CRITIC SCORE</div>
        <div class="legend-bar"></div>
        <div class="legend-labels"><span>0</span><span>25</span><span>50</span><span>75</span><span>100</span></div>
      </div>

      <footer>
        #TidyTuesday 2026 Week 23 | Source: Wikipedia data compiled by TidyTuesday<br>
        Wajdi Ben Saad | www.WajdiBenSaad.com | Bluesky: wajdi.bsky.social
      </footer>
    </main>
  </div>

  <div id="tooltip" class="tooltip" role="status" hidden></div>

  <script>
    const films = __FILMS__;
    const trend = __TREND__;
    const decades = __DECADES__;
    const labels = __LABELS__;

    const colors = {
      bg: "#080A12", panel: "#0D1020", grid: "#26304B",
      white: "#F4F7FF", muted: "#AAB3CB", cyan: "#3CF2FF",
      pink: "#FF2B55", yellow: "#FFE66D"
    };
    const svg = document.getElementById("chart");
    const tooltip = document.getElementById("tooltip");
    const levels = document.getElementById("levels");
    const NS = "http://www.w3.org/2000/svg";
    const W = 1400, H = 620;
    const margin = { top: 24, right: 32, bottom: 78, left: 112 };
    const innerW = W - margin.left - margin.right;
    const innerH = H - margin.top - margin.bottom;
    const xmin = 1992.5, xmax = 2026.6;
    let activeDecade = null;
    let pinnedPoint = null;

    const sx = value => margin.left + (value - xmin) / (xmax - xmin) * innerW;
    const sy = value => margin.top + (100 - value) / 100 * innerH;
    const esc = value => String(value ?? "").replace(/[&<>"']/g, ch => ({"&":"&amp;","<":"&lt;",">":"&gt;",'"':"&quot;","'":"&#39;"}[ch]));

    function add(tag, attrs = {}, parent = svg, text = null) {
      const el = document.createElementNS(NS, tag);
      Object.entries(attrs).forEach(([key, value]) => el.setAttribute(key, value));
      if (text !== null) el.textContent = text;
      parent.appendChild(el);
      return el;
    }

    function scoreColor(score) {
      const stops = [
        [0, [255,43,85]], [25, [255,100,56]], [50, [255,230,109]],
        [75, [60,242,255]], [100, [124,255,178]]
      ];
      let i = 0;
      while (i < stops.length - 2 && score > stops[i + 1][0]) i++;
      const [a, ca] = stops[i], [b, cb] = stops[i + 1];
      const t = Math.max(0, Math.min(1, (score - a) / (b - a)));
      const rgb = ca.map((v, j) => Math.round(v + (cb[j] - v) * t));
      return `rgb(${rgb.join(",")})`;
    }

    function positionTooltip(event) {
      const gap = 16;
      const box = tooltip.getBoundingClientRect();
      let left = event.clientX + gap;
      let top = event.clientY + gap;
      if (left + box.width > window.innerWidth - 10) left = event.clientX - box.width - gap;
      if (top + box.height > window.innerHeight - 10) top = event.clientY - box.height - gap;
      tooltip.style.left = `${Math.max(10, left)}px`;
      tooltip.style.top = `${Math.max(10, top)}px`;
    }

    function showFilm(event, film) {
      const rows = [
        `<div class="tooltip-title">${esc(film.title)}</div>`,
        `<div class="tooltip-score">Rotten Tomatoes: ${film.score}%</div>`,
        `<div>Released: ${esc(film.release_date)}</div>`,
        film.metacritic !== null ? `<div>Metacritic: ${film.metacritic}/100</div>` : "",
        film.cinema_score ? `<div>CinemaScore: ${esc(film.cinema_score)}</div>` : "",
        film.director ? `<div>Director: ${esc(film.director)}</div>` : "",
        film.publisher ? `<div>Game publisher: ${esc(film.publisher)}</div>` : "",
        `<div>Format: ${esc(film.format)}</div>`
      ];
      tooltip.innerHTML = rows.join("");
      tooltip.hidden = false;
      positionTooltip(event);
    }

    function hideTooltip() {
      if (!pinnedPoint) tooltip.hidden = true;
    }

    function drawTextBox(x, y, text, anchor = "start") {
      const group = add("g", { "pointer-events": "none" });
      const label = add("text", {
        x, y, "text-anchor": anchor, fill: colors.white,
        "font-family": "Press Start 2P", "font-size": 10
      }, group, text);
      const box = label.getBBox();
      const rect = document.createElementNS(NS, "rect");
      rect.setAttribute("x", box.x - 7);
      rect.setAttribute("y", box.y - 5);
      rect.setAttribute("width", box.width + 14);
      rect.setAttribute("height", box.height + 10);
      rect.setAttribute("fill", colors.bg);
      rect.setAttribute("stroke", colors.white);
      rect.setAttribute("stroke-width", ".8");
      group.insertBefore(rect, label);
      return group;
    }

    function applyFilter() {
      document.querySelectorAll(".pixel").forEach(point => {
        point.classList.toggle("dimmed", activeDecade !== null && Number(point.dataset.decade) !== activeDecade);
      });
      document.querySelectorAll(".level-card").forEach(card => {
        card.classList.toggle("active", Number(card.dataset.decade) === activeDecade);
      });
    }

    async function renderChart() {
      await document.fonts.load('12px "Press Start 2P"');

    decades.forEach(item => {
      const card = document.createElement("button");
      card.type = "button";
      card.className = "level-card";
      card.dataset.decade = item.decade;
      card.innerHTML = `<div class="level-title">${esc(item.level)}</div><div class="level-summary">${item.decade}s&nbsp; // &nbsp;MEDIAN ${Math.round(item.median_score)}<br>${item.films} REVIEWED FILMS</div>`;
      card.addEventListener("click", () => {
        activeDecade = activeDecade === item.decade ? null : item.decade;
        applyFilter();
      });
      levels.appendChild(card);
    });

    document.getElementById("reset").addEventListener("click", () => {
      activeDecade = null;
      pinnedPoint = null;
      tooltip.hidden = true;
      document.querySelectorAll(".pixel.selected").forEach(el => el.classList.remove("selected"));
      applyFilter();
    });

    const defs = add("defs");
    const glow = add("filter", { id: "glow", x: "-40%", y: "-40%", width: "180%", height: "180%" }, defs);
    add("feGaussianBlur", { stdDeviation: 5, result: "blur" }, glow);
    const merge = add("feMerge", {}, glow);
    add("feMergeNode", { in: "blur" }, merge);
    add("feMergeNode", { in: "SourceGraphic" }, merge);

    const bands = [
      [1992.5, 2000, "#10162A"], [2000, 2010, "#0B1326"],
      [2010, 2020, "#10162A"], [2020, 2026.6, "#0B1326"]
    ];
    bands.forEach(([a, b, fill]) => add("rect", {
      x: sx(a), y: margin.top, width: sx(b) - sx(a), height: innerH, fill
    }));

    for (let value = 0; value <= 100; value += 2) {
      add("line", {
        x1: margin.left, x2: W - margin.right, y1: sy(value), y2: sy(value),
        stroke: colors.white, "stroke-width": .25, opacity: .045
      });
    }
    [0, 25, 50, 75, 100].forEach(value => {
      add("line", {
        x1: margin.left, x2: W - margin.right, y1: sy(value), y2: sy(value),
        stroke: colors.grid, "stroke-width": 1
      });
      const label = value === 0 ? "0  GAME OVER" : value === 100 ? "100  HIGH SCORE" : String(value);
      add("text", {
        x: margin.left - 14, y: sy(value) + 4, "text-anchor": "end",
        fill: colors.muted, "font-family": "Press Start 2P", "font-size": 10
      }, svg, label);
    });
    [2000, 2010, 2020].forEach(year => add("line", {
      x1: sx(year), x2: sx(year), y1: margin.top, y2: H - margin.bottom,
      stroke: colors.cyan, "stroke-width": 1, opacity: .34, "stroke-dasharray": "6 7"
    }));
    [1995, 2000, 2005, 2010, 2015, 2020, 2025].forEach(year => add("text", {
      x: sx(year), y: H - margin.bottom + 28, "text-anchor": "middle",
      fill: colors.muted, "font-family": "Press Start 2P", "font-size": 10
    }, svg, String(year)));
    add("rect", {
      x: margin.left, y: margin.top, width: innerW, height: innerH,
      fill: "none", stroke: colors.grid, "stroke-width": 2
    });

    let path = `M ${sx(trend[0].year)} ${sy(trend[0].score)}`;
    for (let i = 1; i < trend.length; i++) {
      const mid = (sx(trend[i - 1].year) + sx(trend[i].year)) / 2;
      path += ` H ${mid} V ${sy(trend[i].score)} H ${sx(trend[i].year)}`;
    }
    add("path", { d: path, fill: "none", stroke: colors.cyan, "stroke-width": 13, opacity: .10, filter: "url(#glow)" });
    add("path", { d: path, fill: "none", stroke: colors.cyan, "stroke-width": 5, opacity: .24 });
    const trendLine = add("path", {
      d: path, fill: "none", stroke: colors.cyan, "stroke-width": 2.5,
      class: "trend-hit", tabindex: 0
    });
    trendLine.addEventListener("pointerenter", event => {
      tooltip.innerHTML = `<div class="tooltip-title">5-YEAR MEDIAN PATH</div><div>Rolling median calculated from films released within two years on either side of each year.</div>`;
      tooltip.hidden = false;
      positionTooltip(event);
    });
    trendLine.addEventListener("pointermove", positionTooltip);
    trendLine.addEventListener("pointerleave", hideTooltip);

    films.forEach((film, index) => {
      add("rect", {
        x: sx(film.plot_year) - 7, y: sy(film.score) - 7,
        width: 14, height: 14, fill: "#000000", opacity: .72
      });
      const point = add("rect", {
        x: sx(film.plot_year) - 5.5, y: sy(film.score) - 5.5,
        width: 11, height: 11, fill: scoreColor(film.score),
        class: "pixel", tabindex: 0, role: "button",
        "aria-label": `${film.title}, ${film.score} percent`,
        "data-decade": film.decade, "data-index": index
      });
      point.addEventListener("pointerenter", event => showFilm(event, film));
      point.addEventListener("pointermove", positionTooltip);
      point.addEventListener("pointerleave", hideTooltip);
      point.addEventListener("click", event => {
        event.stopPropagation();
        document.querySelectorAll(".pixel.selected").forEach(el => {
          if (el !== point) el.classList.remove("selected");
        });
        point.classList.toggle("selected");
        pinnedPoint = point.classList.contains("selected") ? point : null;
        if (pinnedPoint) showFilm(event, film); else tooltip.hidden = true;
      });
      point.addEventListener("keydown", event => {
        if (event.key === "Enter" || event.key === " ") point.click();
      });
    });

    labels.forEach(item => {
      const x1 = sx(item.plot_year), y1 = sy(item.score);
      const x2 = x1 + item.dx, y2 = y1 + item.dy;
      add("line", { x1, y1, x2, y2, stroke: colors.yellow, "stroke-width": .8 });
      drawTextBox(x2, y2, item.text, item.anchor);
    });

    drawTextBox(W - margin.right - 12, sy(trend[trend.length - 1].score) - 22, "5-YEAR MEDIAN PATH", "end");
    add("text", {
      x: margin.left + innerW / 2, y: H - 15, "text-anchor": "middle",
      fill: colors.white, "font-family": "Press Start 2P", "font-size": 12
    }, svg, "RELEASE YEAR  >  NEXT LEVEL");
    const yTitle = add("text", {
      x: 28, y: margin.top + innerH / 2, "text-anchor": "middle",
      fill: colors.white, "font-family": "Press Start 2P", "font-size": 11,
      transform: `rotate(-90 28 ${margin.top + innerH / 2})`
    }, svg, "ROTTEN TOMATOES SCORE");

    document.addEventListener("click", event => {
      if (!event.target.classList.contains("pixel")) {
        pinnedPoint = null;
        tooltip.hidden = true;
        document.querySelectorAll(".pixel.selected").forEach(el => el.classList.remove("selected"));
      }
    });
    }

    renderChart();
  </script>
</body>
</html>)---"

page <- gsub("__FONT__", font_base64, page, fixed = TRUE)
page <- gsub("__FILMS__", films_json, page, fixed = TRUE)
page <- gsub("__TREND__", trend_json, page, fixed = TRUE)
page <- gsub("__DECADES__", decades_json, page, fixed = TRUE)
page <- gsub("__LABELS__", labels_json, page, fixed = TRUE)

writeLines(page, output_path, useBytes = TRUE)
message("Saved: ", output_path)
