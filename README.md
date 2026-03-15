# World Cup Home Field Advantage Dashboard
**ESADE MIBA · Data Analytics & Reporting · Dataviz Club 2025 — Team 9**

> *Do nations perform better — reach further, score more, and dominate the podium — when they host the FIFA World Cup on home soil?*

---

## Overview

This project investigates the **home field advantage** in international football by analysing every FIFA World Cup tournament from **1930 to 2018** (21 editions). The evidence is presented as a single, self-contained HTML dashboard composed of three complementary visualisations, each probing the question from a different analytical angle.

---

## The Dashboard

The output is a single file — `dashboard.html` — which can be opened in any web browser (fonts load from Google CDN; all chart data is embedded as SVG).

### Chart 1 — The Home Spike
**Type:** Swimlane dot chart
**Question:** How far did each host nation advance in the tournaments they hosted, compared to all other years?

One row per host nation (14 nations, 1930–2018). Each dot represents a tournament appearance; dot size and colour encode the furthest round reached on a 6-point scale (Groups → Winner). Hosting years are marked with an **H** label and the year printed above. The pattern is immediate: almost every nation's darkest, largest dot — their best-ever result — falls in the hosting year.

### Chart 2 — The Scoring Boost
**Type:** Scatter plot with country flags
**Question:** Did hosting lead nations to score more goals per game?

- x-axis = average goals per game across all non-hosting tournaments
- y-axis = average goals per game in hosting tournaments

Points above the y = x diagonal scored **more** as hosts. A numeric difference label (+/−X.XX) sits beside each flag. 13 of the 17 hosts scored at a higher rate on home soil.

### Chart 3 — The Top 4 Grid
**Type:** Tile heatmap
**Question:** Does the host nation's continent dominate the final standings?

One column per tournament, four rows (1st–4th place). Each cell is coloured by the finisher's relationship to the host:
- 🟦 **Dark blue** — the host nation itself
- 🔵 **Light blue** — same FIFA confederation as host
- ⬜ **Warm grey** — different continent

The colour clusters make it clear: when a continent hosts, it consistently over-performs in the top 4.

---

## How to Run

### Prerequisites

Install the required R packages (run once):

```r
install.packages(c("tidyverse", "ggplot2", "ggtext", "showtext",
                   "svglite", "ggrepel", "patchwork", "scales"))

# GitHub-only package:
remotes::install_github("rensa/ggflags")
```

### Generate the dashboard

```r
# From the R console — set working directory first:
setwd("/path/to/Datawiz_WORLD_-CUP")
source("worldcup_dashboard.R")
# → Saves dashboard.html in the same directory

# Or from the terminal:
Rscript worldcup_dashboard.R
```

Open `dashboard.html` in any modern web browser.

---

## Data Source

| Object | Description |
|---|---|
| `worldcup$wcmatches` | One row per match — year, home/away team, scores, stage, winning team |
| `worldcup$worldcups` | One row per tournament — year, host, winner, 2nd, 3rd, 4th place |

Source: **`worldcup` R package** — FIFA World Cup 1930–2018
Loaded from: `13_worldcup.RData`

**Data preparation steps applied in the script:**
- West Germany unified with Germany across all records (matches and final standings) — treated as a single continuous national team for historical coherence
- Stage strings (free text) mapped to an ordinal 1–6 score to derive the furthest round each team reached per tournament
- Goals-per-game computed by averaging across all individual matches (not per-tournament averages) to avoid small-sample weighting bias

---


## R Packages Used

| Package | Role |
|---|---|
| `tidyverse` | Data wrangling for Chart 3 (`pivot_longer`, `mutate`, `case_when`) |
| `ggplot2` | Core visualisation engine for all three charts |
| `ggtext` | `element_markdown` — inline HTML/Markdown in titles and subtitles |
| `ggflags` | Circular country flag images in Chart 2 |
| `showtext` | IBM Plex Sans / IBM Plex Mono from Google Fonts |
| `svglite` | Captures plot output as an SVG string for HTML embedding |
| `ggrepel` | Repels difference labels away from flag positions (Chart 2) |
| `patchwork` | Combines three ggplot objects into a single dashboard layout |
| `scales` | `pretty_breaks()` for clean axis tick spacing (Chart 2) |

---

## Design Decisions

| Decision | Rationale |
|---|---|
| Single-file script | All logic in `worldcup_dashboard.R`; no `source()` dependencies |
| SVG embedded in HTML | Zero external file dependencies; dashboard opens as one portable file |
| IBM Plex font family | Mono for data labels (numeric precision feel), Sans for all other text |
| Blue-only colour ramp (Charts 1 & 2) | Single-hue progression avoids categorical confusion; darker = better result |
| `wrap_elements(full = p_dash1)` | Prevents patchwork aligning Chart 1's data panel, which would leave dead space above Charts 2 & 3 |
| West Germany → Germany | Maintains a single continuous row for Germany in the swimlane; avoids a sparse second partial row |
| Per-row `nudge_x` for ggrepel | South Korea's label requires a leftward nudge (`−0.25`) to avoid overlapping Mexico's flag |

---

## Assessment Rubric Alignment

| Criterion | Points | Implementation |
|---|---|---|
| **Data Preparation** | 2 | Single `load()`, West Germany normalisation, stage-to-score mapping, goals-per-game averaged across matches |
| **R Code Quality** | 2 | Every block commented with intent and rationale; consistent style; no redundancy |
| **Visualization Design** | 3 | Three chart types (swimlane, scatter, heatmap) each addressing a distinct sub-question about home advantage |
| **Aesthetic Appeal** | 3 | Consistent IBM Plex typography, single-hue blue palette, styled markdown titles, polished minimal theme |
