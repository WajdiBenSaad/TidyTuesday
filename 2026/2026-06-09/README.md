# TidyTuesday 2026-06-09: Films Based on Video Games

![Do Video-Game Movies Finally Get Good?](video_game_movies_scores.png)

## Interactive Preview

The interactive version reveals film details on hover and allows each decade to be explored separately.

![Interactive video-game movie chart preview](video_game_movies_interactive_demo.gif)

## About

This week's TidyTuesday dataset contains films based on video games, including theatrical releases, television films, direct-to-video productions, documentaries, and short films.

The chart asks: **do video-game movies finally get good?** Each pixel represents a film with a reported Rotten Tomatoes score. The glowing path follows the rolling five-year median, while the level cards summarize each decade.

Data source:

- [TidyTuesday 2026-06-09: Films Based on Video Games](https://github.com/rfordatascience/tidytuesday/blob/main/data/2026/2026-06-09/readme.md)

The raw CSV is downloaded to `2026/data/` when needed. That directory is ignored by git.

## Reading The Chart

- Median scores were nearly unchanged between the 1990s and 2000s: 17 and 18, respectively.
- The median rose to 38 in the 2010s and 51 in the 2020s.
- Strong recent releases coexist with poorly reviewed adaptations, but the overall critical baseline has moved upward.
- The chart includes the 73 films in the dataset with reported Rotten Tomatoes scores, so it describes the reviewed subset rather than every listed adaptation.

## Design

The design treats the timeline as progress through an arcade game. Square film markers act as pixels, decades become successive levels, and the rolling median becomes a glowing player path. CRT scanlines, a dark screen, score-based neon colors, and pixel typography reinforce the theme while preserving a conventional time-versus-score reading.

The title and interface accents use [Press Start 2P](https://fonts.google.com/specimen/Press+Start+2P), distributed under the SIL Open Font License.
