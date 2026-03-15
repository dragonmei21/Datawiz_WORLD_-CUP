
# worldcup_dashboard.R
# Team 9
# Purpose: Investigate whether hosting the FIFA World Cup gives nations a
#          performance advantage — analysed from three complementary angles.
#
# Research question:
#   Do nations perform better — reach further, score more, and dominate the
#   podium — when they host the World Cup on home soil?
#
# Three charts, one dashboard:
#   Chart 1 — "The Home Spike"    : Swimlane dot chart showing every tournament
#             appearance of each host nation, with hosting years highlighted.
#   Chart 2 — "The Scoring Boost" : Scatter plot comparing average goals scored
#             per game while hosting vs not hosting (all 17 hosts).
#   Chart 3 — "The Top 4 Grid"   : Heatmap of all top-4 finishers across every
#             tournament, coloured by host / same-continent / other.
#
# Data   : worldcup package — FIFA World Cup 1930–2018 (worldcup$wcmatches,
#          worldcup$worldcups). Loaded from 13_worldcup.RData.
# Output : dashboard.html  (self-contained SVG embedded in a single HTML file)
# Run    : Rscript worldcup_dashboard.R

# 1. LIBRARIES

# Each package serves a specific role in the pipeline:
library(tidyverse)   # Chart 3 data wrangling: pivot_longer, mutate, case_when
library(ggplot2)     # Core visualisation engine for all three charts
library(ggtext)      # element_markdown — enables HTML/Markdown in plot titles
library(ggflags)     # geom_flag — renders circular country flag images (Chart 2)
library(showtext)    # Loads Google Fonts (IBM Plex) for consistent typography
library(svglite)     # svgstring() captures plot output as an SVG string for HTML
library(ggrepel)     # geom_text_repel — prevents label collisions (Chart 2)
library(patchwork)   # Combines multiple ggplot objects into a single layout
library(scales)      # pretty_breaks() for clean axis tick spacing (Chart 2)


# 2. TYPOGRAPHY
# IBM Plex Sans (sans-serif) is used for all titles, subtitles and body text.
# IBM Plex Mono (monospace) is used for numeric axis labels and data labels,
# reinforcing a data-first, technical feel consistent throughout the dashboard.
font_add_google("IBM Plex Sans", "ibmplexsans")
font_add_google("IBM Plex Mono", "ibmplexmono")
showtext_auto()           # Activate showtext rendering for all devices
showtext_opts(dpi = 96)   # Match screen DPI so font sizes render correctly

# 3. DATA PREPARATION
# worldcup$wcmatches  — one row per match; key columns used:
#   year, home_team, away_team, home_score, away_score, winning_team, stage
# worldcup$worldcups  — one row per tournament; key columns used:
#   year, host, winner, second, third, fourth
load("./DATA/13_worldcup.RData")
m  <- worldcup$wcmatches   # match-level data
wc <- worldcup$worldcups   # tournament-level data

# ── West Germany normalisation ────────────────────────────────────────────────
# West Germany competed as a separate nation from 1954–1990. For this analysis
# we treat all German appearances as a single continuous entity ("Germany") so
# that the swimlane and goal-rate calculations reflect a single, coherent line
# of national football history. This affects home_team, away_team, winning_team
# in the match table, and winner / second in the tournament table.
m$home_team[m$home_team == "West Germany"]       <- "Germany"
m$away_team[m$away_team == "West Germany"]       <- "Germany"
m$winning_team[m$winning_team == "West Germany"] <- "Germany"
wc$winner[wc$winner == "West Germany"]           <- "Germany"
wc$second[wc$second == "West Germany"]           <- "Germany"

# CHART 1 — THE HOME SPIKE
# Design rationale:
#   A swimlane (strip) chart with one row per host nation and one dot per
#   tournament the nation participated in. Dot colour and size encode the
#   furthest stage reached; dots in the host year are labelled "H" and have the
#   year printed above them. The visual pattern is immediately striking: nearly
#   every host nation's darkest, largest dot — their best ever result — falls in
#   the hosting year column.

# ── Stage-to-score mapping
# Tournament stages are stored as free-text strings (e.g. "Quarter-finals",
# "Semi-finals", "Final"). We map them to an ordinal score 1–6 so that
# aggregate(max) can find the furthest round each team reached in each year.
#   1 = Group stage   2 = Round of 16   3 = Quarter-final
#   4 = Semi-final    5 = Runner-up     6 = Winner
# Note: Third-place play-off is treated the same as semi-final (score 4) because
# reaching the semis is the meaningful hurdle, not the bronze-medal match result.
stage_to_score <- function(s) {
  ifelse(grepl("Final$", s) & !grepl("Semi|Quarter|Third", s, ignore.case = TRUE), 6,
         ifelse(grepl("Third",   s, ignore.case = TRUE), 4,
                ifelse(grepl("Semi",    s, ignore.case = TRUE), 4,
                       ifelse(grepl("Quarter", s, ignore.case = TRUE), 3,
                              ifelse(grepl("Round of 16|Second Round|Last 16",
                                           s, ignore.case = TRUE), 2, 1)))))
}
m$stage_score <- stage_to_score(m$stage)

# ── Build team × year performance table
# Each match gives BOTH the home and away team a score equal to the stage of
# that match. Stacking home and away rows then taking the per-team/year maximum
# gives each nation's furthest round in that tournament.
team_year <- rbind(
  data.frame(year = m$year, team = m$home_team, ss = m$stage_score),
  data.frame(year = m$year, team = m$away_team, ss = m$stage_score)
)
tr <- aggregate(ss ~ team + year, data = team_year, FUN = max)

# Override: the tournament table gives us exact winner (6) and runner-up (5)
# records which are more reliable than inferring from stage strings alone.
for (i in 1:nrow(wc)) {
  tr$ss[tr$team == wc$winner[i] & tr$year == wc$year[i]] <- 6
  tr$ss[tr$team == wc$second[i] & tr$year == wc$year[i]] <- 5
}

# ── Host nation index
# Only nations that have hosted at least once are shown (14 total through 2018).
# c1_host_map maps each team name to its hosting year(s); nations that hosted
# twice (Italy, France, Brazil, Mexico, Germany) have vector values.
# Note: USA, Japan, South Africa are excluded here — they appear in Chart 2
# (scoring boost) but are omitted from Chart 1 because including them would
# add rows with very few World Cup appearances, compressing the chart.
c1_host_map <- list(
  Uruguay = 1930, Italy = c(1934, 1990), France = c(1938, 1998),
  Brazil = c(1950, 2014), Switzerland = 1954, Sweden = 1958,
  Chile = 1962, England = 1966, Mexico = c(1970, 1986),
  Germany = c(1974, 2006), Argentina = 1978, Spain = 1982,
  `South Korea` = 2002, Russia = 2018
)

# TARGET: exact names as they appear in the match data (used for filtering)
# c1_display: short labels used on the y-axis (avoids truncation for "S. Korea")
TARGET     <- c("Uruguay","Italy","France","Brazil","Switzerland","Sweden",
                "Chile","England","Mexico","Germany","Argentina","Spain",
                "South Korea","Russia")
c1_display <- c("Uruguay","Italy","France","Brazil","Switzerland","Sweden",
                "Chile","England","Mexico","Germany","Argentina","Spain",
                "S. Korea","Russia")

# ── Build plot data frame
# For each host nation, subset tr to that team's rows, attach the display label
# and a boolean flag marking which years were hosting years.
plot_df <- do.call(rbind, lapply(seq_along(TARGET), function(idx) {
  team <- TARGET[idx]
  hy   <- c1_host_map[[team]]
  sub  <- tr[tr$team == team, ]
  if (nrow(sub) == 0) return(NULL)
  sub$display <- c1_display[idx]       # short y-axis label
  sub$is_host <- sub$year %in% hy      # TRUE for hosting year(s)
  sub
}))

# Ordered factor for y-axis: Uruguay listed first (top), Russia last (bottom).
# rev(c1_display) is required because ggplot2 factors start at y = 1 (bottom).
plot_df$nation_f <- factor(plot_df$display, levels = rev(c1_display))
plot_df$ss_f     <- factor(plot_df$ss, levels = 1:6)

# Text colour inside "H" labels: white on dark-blue dots (ss ≥ 3), navy otherwise
plot_df$h_textcol <- ifelse(plot_df$ss >= 3, "white", "#1E3A8A")

# ── Visual encoding: colour, stroke, size, legend labels
# A single blue-family ramp encodes progress from Groups (lightest) to Winner
# (darkest navy). Dot size grows with stage to provide a redundant size cue,
# making the "spike" pattern perceptible even in small print.
stage_fill   <- c("1"="#DBEAFE","2"="#93C5FD","3"="#3B82F6",
                  "4"="#1D4ED8","5"="#1E3A8A","6"="#172554")
stage_stroke <- c("1"="#93BBEC","2"="#93C5FD","3"="#3B82F6",
                  "4"="#1D4ED8","5"="#1E3A8A","6"="#172554")
stage_size   <- c("1"=2.8,"2"=3.8,"3"=5.2,"4"=7.0,"5"=9.5,"6"=12.0)
stage_label  <- c("1"="Groups","2"="R16","3"="QF","4"="SF","5"="Final","6"="Winner")

# ── Host-year annotation layer
host_df <- plot_df[plot_df$is_host, ]

# y_nudge pushes the year text above the dot; larger dots need more clearance,
# so nudge distance is keyed to stage score rather than being a fixed constant.
nudge_map       <- c("1"=0.24,"2"=0.28,"3"=0.32,"4"=0.38,"5"=0.46,"6"=0.54)
host_df$y_nudge <- as.numeric(host_df$nation_f) +
                     nudge_map[as.character(host_df$ss)]

# 1930 label is suppressed because 1930 already appears on the x-axis
year_label_df <- host_df[host_df$year != 1930, ]

# ── Alternating row background
# Subtle #F8F9FA bands on every second row improve row tracking across the wide
# x-axis range. We shade the even rows from the top (Italy, Brazil, Sweden…).
even_display <- c1_display[c(2, 4, 6, 8, 10, 12, 14)]
alt_bg_df <- data.frame(
  ymin = as.numeric(factor(even_display, levels = rev(c1_display))) - 0.5,
  ymax = as.numeric(factor(even_display, levels = rev(c1_display))) + 0.5
)

# ── Chart 1: ggplot2 build
p_spike <- ggplot(plot_df, aes(x = year, y = nation_f)) +

  # Alternating row strips (drawn first so all other layers sit on top)
  geom_rect(
    data        = alt_bg_df,
    aes(ymin = ymin, ymax = ymax, xmin = -Inf, xmax = Inf),
    fill        = "#F8F9FA",
    inherit.aes = FALSE
  ) +

  # Faint horizontal rule per row — gives the eye a baseline to follow
  geom_hline(
    yintercept = seq_along(c1_display),
    colour     = "#E5E7EB",
    linewidth  = 0.35
  ) +

  # Main data layer: filled circles, size and colour encode stage reached
  geom_point(
    aes(fill = ss_f, size = ss_f, colour = ss_f),
    shape = 21, stroke = 0.5     # shape 21 = circle with independent fill + stroke
  ) +

  # "H" label overlaid on hosting-year dots to make them immediately scannable
  geom_text(
    data     = host_df,
    aes(label = "H"),
    colour   = host_df$h_textcol,
    size     = 2.9,
    fontface = "bold",
    family   = "ibmplexmono"
  ) +

  # Year printed above each hosting dot (except 1930, already on x-axis tick)
  geom_text(
    data     = year_label_df,
    aes(x = year, y = y_nudge, label = as.character(year)),
    size     = 2.2,
    fontface = "bold",
    family   = "ibmplexmono",
    colour   = "#1A1A2E"
  ) +

  # ── Scales
  scale_fill_manual(
    values = stage_fill,
    labels = stage_label,
    name   = "Stage reached \u2192"
  ) +
  scale_colour_manual(
    values = stage_stroke,
    guide  = "none"              # stroke scale shares legend with fill; suppress duplicate
  ) +
  scale_size_manual(
    values = stage_size,
    labels = stage_label,
    name   = "Stage reached \u2192"
  ) +
  # Sparse x-axis ticks reduce clutter; data spans 1930–2018 but axis stops at 2021
  scale_x_continuous(
    breaks = c(1930, 1950, 1966, 1982, 1998, 2014, 2018),
    limits = c(1927, 2021),
    expand = c(0, 0)
  ) +

  # Merge fill and size into a single combined legend row
  guides(
    fill = guide_legend(
      override.aes = list(size = unname(stage_size), colour = unname(stage_stroke)),
      nrow = 1
    ),
    size = "none"
  ) +

  # ── Labels
  labs(
    title    = "The **Home** *<span style='color:#1D4ED8'>Spike</span>*",
    subtitle = "Every World Cup appearance for each host nation \u2014 how far they went\nAlmost every host nation's best result in history happened the year they played on home soil.",
    caption  = "H\u00a0= hosted that year \u00b7 Source: FIFA World Cup 1930\u20132018 \u00b7 ESADE MIBA Dataviz Club 2025",
    x = NULL, y = NULL
  ) +

  # ── Theme
  theme_minimal(base_size = 11, base_family = "ibmplexsans") +
  theme(
    plot.background    = element_rect(fill = "#FFFFFF", colour = NA),
    panel.background   = element_rect(fill = "#FFFFFF", colour = NA),
    panel.grid         = element_blank(),   # grids removed; alt-row bands do the job

    axis.text.x        = element_text(size = 11,  colour = "#6B7280",
                                      family = "ibmplexmono"),
    axis.text.y        = element_text(size = 13, colour = "#1A1A2E",
                                      face = "bold", hjust = 1,
                                      margin = margin(r = 8)),
    axis.ticks.x       = element_line(colour = "#1A1A2E", linewidth = 0.5),
    axis.ticks.y       = element_blank(),
    axis.ticks.length.x = unit(4, "pt"),

    # element_markdown allows bold/italic/HTML colour spans in the title string
    plot.title         = element_markdown(size = 38, face = "bold",
                                          colour = "#1A1A2E",
                                          family = "ibmplexsans",
                                          margin = margin(b = 6)),
    plot.subtitle      = element_text(size = 13, colour = "#6B7280",
                                      face = "bold", family = "ibmplexsans",
                                      margin = margin(b = 20)),
    plot.caption       = element_text(size = 10, colour = "#9CA3AF",
                                      hjust = 0, family = "ibmplexmono",
                                      margin = margin(t = 10)),

    legend.position      = "top",
    legend.justification = "center",   # centre the legend bar over the chart
    legend.title         = element_text(size = 11, colour = "#6B7280",
                                        face = "bold", family = "ibmplexsans"),
    legend.text          = element_text(size = 11, colour = "#6B7280",
                                        family = "ibmplexmono"),
    legend.key.height    = unit(0.5, "cm"),
    legend.key.width     = unit(0.5, "cm"),
    legend.direction     = "horizontal",
    legend.box.spacing   = unit(8, "pt"),
    legend.spacing.x     = unit(6, "pt"),

    plot.margin             = margin(24, 32, 18, 32),
    plot.title.position     = "panel"   # align title with the data panel, not the full plot
  )


# =============================================================================
# CHART 2 — THE SCORING BOOST
# =============================================================================
# Design rationale:
#   A scatter plot where each point is one of the 17 nations that hosted at
#   least one World Cup from 1930 to 2018 (USA 1994, Japan 2002, and South
#   Africa 2010 are included here, unlike Chart 1, because the metric is purely
#   goal-rate and every host's data is available).
#   x-axis : goals per game averaged across all tournaments NOT hosted
#   y-axis : goals per game averaged across hosting tournaments
#   Points above the y = x diagonal scored MORE as hosts than as visitors.
#   Nation flags replace plain dots to make each country instantly identifiable.
#   The numeric difference (+/-X.XX) is printed beside each flag via ggrepel.

# ── Host-year lookup for all 17 hosts
# This list extends c1_host_map with USA, Japan, and South Africa.
c2_host_map <- list(
  Uruguay = 1930, Italy = c(1934, 1990), France = c(1938, 1998),
  Brazil = c(1950, 2014), Switzerland = 1954, Sweden = 1958,
  Chile = 1962, England = 1966, Mexico = c(1970, 1986),
  Germany = c(1974, 2006), Argentina = 1978, Spain = 1982,
  `United States` = 1994, `South Korea` = 2002,
  Japan = 2002, `South Africa` = 2010, Russia = 2018
)

# Short display names for legend / potential labelling (not directly rendered
# on the scatter — flags serve that role)
c2_display <- c(Uruguay="Uruguay", Italy="Italy", France="France", Brazil="Brazil",
                Switzerland="Switzerland", Sweden="Sweden", Chile="Chile",
                England="England", Mexico="Mexico", Germany="Germany",
                Argentina="Argentina", Spain="Spain", `United States`="USA",
                `South Korea`="S. Korea", Japan="Japan",
                `South Africa`="S. Africa", Russia="Russia")

# ISO 3166-1 alpha-2 codes (lowercase) required by ggflags::geom_flag.
# England has no separate ISO code so "gb" (Great Britain) is the closest match.
CODES <- c(Uruguay="uy", Italy="it", France="fr", Brazil="br",
           Switzerland="ch", Sweden="se", Chile="cl", England="gb",
           Mexico="mx", Germany="de", Argentina="ar", Spain="es",
           `United States`="us", `South Korea`="kr", Japan="jp",
           `South Africa`="za", Russia="ru")

# ── Compute goals-per-game for hosting vs non-hosting periods
# For each nation we collect every goal they scored (as home OR away team) and
# split the rows by whether the match year falls in a hosting year.
# Goals scored = home_score when playing at home, away_score when playing away.
# We average over all matches (not tournaments) to avoid weighting bias.
rows <- lapply(names(c2_host_map), function(team) {
  hy          <- c2_host_map[[team]]
  hg_df       <- m[m$home_team == team, c("year", "home_score")]
  ag_df       <- m[m$away_team == team, c("year", "away_score")]
  names(hg_df) <- c("year", "goals")
  names(ag_df) <- c("year", "goals")
  tg          <- rbind(hg_df, ag_df)         # all matches for this team
  hosting     <- tg[tg$year %in% hy, ]       # matches played in hosting years
  not_hosting <- tg[!tg$year %in% hy, ]      # all other tournament appearances
  if (nrow(hosting) == 0 || nrow(not_hosting) == 0) return(NULL)
  h_gpg <- mean(hosting$goals,     na.rm = TRUE)   # goals per game — hosting
  a_gpg <- mean(not_hosting$goals, na.rm = TRUE)   # goals per game — away
  diff  <- h_gpg - a_gpg                           # positive = home boost
  data.frame(
    team  = team,
    t     = unname(c2_display[team]),
    hg    = h_gpg,
    ag    = a_gpg,
    diff  = diff,
    boost = diff > 0,          # TRUE if hosting led to higher scoring
    code  = unname(CODES[team]),
    stringsAsFactors = FALSE
  )
})
dat <- do.call(rbind, rows)

# Summary stats for the dynamic subtitle
n_boost <- sum(dat$boost)     # number of nations that scored MORE when hosting
n_total <- nrow(dat)          # total nations in the analysis

# Formatted difference label (+0.34, -0.07, etc.) shown next to each flag
dat$d_str <- sprintf("%+.2f", dat$diff)

# ── Per-row label nudge
# All labels are nudged 0.18 units upward to float above their flags.
# South Korea's label is additionally nudged -0.25 units leftward because
# Mexico sits very close by at a similar y-value; without this correction
# ggrepel's repulsion would push Korea's label rightward onto Mexico's flag.
dat$nx <- 0.0
dat$ny <- 0.18
dat$nx[dat$team == "South Korea"] <- -0.25

# ── Axis limits with padding
# Limits are derived from the data so the chart auto-scales to any dataset update.
# y_pad is generous (0.42) to create vertical headroom for ggrepel labels;
# x_pad (0.12) provides a smaller horizontal buffer.
x_pad <- 0.12
y_pad <- 0.42
xMin  <- max(0, floor(min(dat$ag) * 10) / 10 - x_pad)
yMin  <- max(0, floor(min(dat$hg) * 10) / 10 - y_pad)
xMax  <- ceiling(max(dat$ag) * 10) / 10 + x_pad
yMax  <- ceiling(max(dat$hg) * 10) / 10 + y_pad

# ── Zone shading polygons
# Two filled polygons divide the plot into "hosting boost" (above diagonal) and
# "hosting penalty" (below diagonal) zones using a very low-alpha blue fill so
# the zones are visible without distracting from the data points.
upper_zone <- data.frame(           # triangle above y = x line
  x = c(xMin, xMin, xMax, xMax),
  y = c(yMin, yMax, yMax, xMax)
)
lower_zone <- data.frame(           # triangle below y = x line
  x = c(xMin, xMax, xMax),
  y = c(xMin, xMax, yMin)
)

# ── Chart 2: ggplot2 build
p_goals <- ggplot(dat, aes(x = ag, y = hg)) +

  # Zone fills (very low alpha — 0D and 08 hex alpha values)
  geom_polygon(data = upper_zone, aes(x = x, y = y),
               fill = "#1D4ED80D", inherit.aes = FALSE) +
  geom_polygon(data = lower_zone, aes(x = x, y = y),
               fill = "#1E3A8A08", inherit.aes = FALSE) +

  # Zone annotation text — bold.italic to distinguish from data labels
  annotate("text", x = xMin + 0.08, y = yMax - 0.12,
           label = "Teams score more when hosting",
           hjust = 0, size = 3.5, colour = "#1D4ED8",
           alpha = 0.4, fontface = "bold.italic", family = "ibmplexsans") +
  annotate("text", x = xMax - 0.08, y = yMin + 0.12,
           label = "Teams score less when hosting",
           hjust = 1, size = 3.5, colour = "#172554",
           alpha = 0.28, fontface = "bold.italic", family = "ibmplexsans") +

  # y = x equality diagonal — dashed grey reference line
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", colour = "#CBD5E1", linewidth = 1.2) +

  # Black border ring drawn behind each flag to give visual separation from
  # any background colour and make flags pop regardless of their edge colour
  geom_point(aes(x = ag, y = hg),
             shape = 21, size = 11.5,
             fill = "black", colour = "black") +

  # Circular flag images replace plain dots — each nation is immediately legible
  geom_flag(aes(country = code), size = 10) +

  # Difference labels repelled away from flags to prevent overlap.
  # seed = 7 ensures reproducible layout across runs.
  # max.overlaps = Inf ensures no label is ever silently dropped.
  ggrepel::geom_text_repel(
    aes(label = d_str),
    nudge_x            = dat$nx,
    nudge_y            = dat$ny,
    direction          = "both",
    size               = 4.2,
    colour             = "#172554",
    fontface           = "bold",
    family             = "ibmplexsans",
    segment.colour     = "#CBD5E1",
    segment.size       = 0.4,
    min.segment.length = 0.1,
    box.padding        = 0.5,
    point.padding      = 0.8,
    max.overlaps       = Inf,
    force              = 8,
    seed               = 7
  ) +

  # ── Scales
  scale_x_continuous(limits = c(xMin, xMax), breaks = scales::pretty_breaks(n = 5),
                     expand = c(0.02, 0)) +
  scale_y_continuous(limits = c(yMin, yMax), breaks = scales::pretty_breaks(n = 5),
                     expand = c(0.02, 0)) +

  # ── Labels
  # The subtitle embeds n_boost and n_total dynamically so the text updates
  # automatically if the underlying data changes.
  labs(
    title    = paste0("The Scoring *<span style='color:#1D4ED8'>Boost</span>*"),
    subtitle = paste0("<b style='color:#1D4ED8;font-size:16pt'>", n_boost,
                      "</b><span style='color:#6B7280'> of ", n_total,
                      " hosts scored more per game when hosting</span>"),
    x = "Goals per game \u2014 not hosting",
    y = "Goals per game \u2014 hosting",
    caption = "Data: FIFA World Cup 1930\u20132018 \u00b7 ESADE MIBA Dataviz Club 2025"
  ) +

  # ── Theme
  theme_minimal(base_size = 13, base_family = "ibmplexsans") +
  theme(
    plot.background  = element_rect(fill = "#FFFFFF", colour = NA),
    panel.background = element_rect(fill = "#FFFFFF", colour = NA),
    panel.grid.major = element_line(colour = "#E5E7EB", linewidth = 0.5,
                                    linetype = "dashed"),
    panel.grid.minor = element_blank(),

    axis.text        = element_text(size = 12, colour = "#6B7280",
                                    face = "bold", family = "ibmplexmono"),
    axis.title       = element_text(size = 13, colour = "#3A3530",
                                    face = "bold", family = "ibmplexsans"),
    axis.line        = element_line(colour = "#1A1A2E", linewidth = 0.9),
    axis.ticks       = element_line(colour = "#1A1A2E", linewidth = 0.5),

    plot.title       = element_markdown(size = 32, face = "bold",
                                        colour = "#1A1A2E",
                                        family = "ibmplexsans",
                                        margin = margin(b = 6)),
    plot.subtitle    = element_markdown(size = 13, face = "bold",
                                        family = "ibmplexsans",
                                        margin = margin(b = 14)),
    plot.caption     = element_text(size = 10, colour = "#9CA3AF",
                                    hjust = 0, family = "ibmplexmono",
                                    margin = margin(t = 8)),
    plot.margin              = margin(24, 32, 18, 32),
    plot.title.position      = "plot"
  )


# CHART 3 — THE TOP 4 GRID
# Design rationale:
#   A tile heatmap with one column per tournament year and four rows (1st–4th).
#   Each cell shows the finishing nation's three-letter code and is coloured
#   by its relationship to the host:
#     Dark blue  — the host nation itself placed here
#     Light blue — a nation from the same FIFA confederation as the host
#     Warm grey  — all others (different continent)
#   The colour pattern makes it visually clear that hosting a tournament
#   elevates not just the host but the entire continental cohort into the top 4.

# ── Confederation lookup
# Maps every nation that has ever appeared in the top 4 to its FIFA confederation.
# This is used to test whether a finisher shares the host's continental bloc.
conf_map <- c(
  Uruguay="CONMEBOL", Argentina="CONMEBOL", Brazil="CONMEBOL",
  Chile="CONMEBOL", Colombia="CONMEBOL", Paraguay="CONMEBOL",
  Italy="UEFA", France="UEFA", England="UEFA", Germany="UEFA",
  "West Germany"="UEFA", Spain="UEFA", Netherlands="UEFA", Portugal="UEFA",
  Czechoslovakia="UEFA", Hungary="UEFA", Austria="UEFA",
  Yugoslavia="UEFA", Poland="UEFA", Sweden="UEFA", Bulgaria="UEFA",
  Belgium="UEFA", "Soviet Union"="UEFA", Croatia="UEFA", Russia="UEFA",
  Turkey="UEFA", Switzerland="UEFA", Denmark="UEFA", Romania="UEFA",
  USA="CONCACAF", Mexico="CONCACAF", "Costa Rica"="CONCACAF",
  "South Korea"="AFC", Japan="AFC", Qatar="AFC",
  Morocco="CAF", "South Africa"="CAF", Cameroon="CAF", Senegal="CAF"
)

# Vectorised lookup; handles the special "Japan, South Korea" co-host entry
get_conf <- function(team) {
  if_else(team == "Japan, South Korea", "AFC", unname(conf_map[team]))
}

# ── Short team name lookup
# Each nation is rendered as a 3–5 character abbreviation inside the tile cell.
# Teams not explicitly listed fall back to the first 5 characters of their name.
short_map <- c(
  "West Germany"="W.GER", Czechoslovakia="TCH", Netherlands="NED",
  "Soviet Union"="USSR", Yugoslavia="YUG", Argentina="ARG",
  "South Korea"="S.KOR", "South Africa"="S.AFR", "Japan, South Korea"="KOR",
  Switzerland="SUI", Portugal="POR", Belgium="BEL", Bulgaria="BUL",
  Croatia="CRO", Austria="AUT", Hungary="HUN", Turkey="TUR",
  Morocco="MAR", Qatar="QAT", Uruguay="URU", Sweden="SWE",
  Brazil="BRA", Italy="ITA", France="FRA", England="ENG",
  Germany="GER", Poland="POL", Spain="ESP", Chile="CHI",
  Russia="RUS", Mexico="MEX", USA="USA", Japan="JPN"
)
shorten <- function(x) {
  coalesce(unname(short_map[x]), substr(x, 1, 5))
}

# Host abbreviation lookup — slightly different codes for column headers
short_host_map <- c(
  Uruguay="URU", Italy="ITA", France="FRA", Brazil="BRA",
  Switzerland="SUI", Sweden="SWE", Chile="CHI", England="ENG",
  Mexico="MEX", "West Germany"="GER", Germany="GER", Argentina="ARG",
  Spain="ESP", USA="USA", "Japan, South Korea"="KOR/JPN",
  "South Africa"="SAF", Russia="RUS", Qatar="QAT"
)
shorten_host <- function(x) {
  coalesce(unname(short_host_map[x]), substr(x, 1, 5))
}

# ── Tidy data reshape
# The worldcups table has four separate columns (winner, second, third, fourth).
# pivot_longer converts these into a single "team" column with a "rank" key,
# giving one row per (year × finishing position) — the ideal structure for a
# tile geom where x = year, y = rank, fill = state.
wc_tbl <- wc %>%
            select(year, host, winner, second, third, fourth)

rank_levels <- c("winner", "second", "third", "fourth")
rank_labels <- c("1st", "2nd", "3rd", "4th")

wc_long <- wc_tbl %>%
  pivot_longer(
    cols      = all_of(rank_levels),
    names_to  = "rank",
    values_to = "team"
  ) %>%
  mutate(
    rank         = factor(rank, levels = rank_levels, labels = rank_labels),
    host_conf    = get_conf(host),
    team_conf    = get_conf(team),
    # is_host: TRUE if this team is the actual host (or one of the 2002 co-hosts)
    is_host      = (team == host) |
                     (host == "Japan, South Korea" & team %in% c("Japan","South Korea")),
    # is_same_conf: TRUE if the team shares the host's confederation but is not the host
    is_same_conf = !is_host & (team_conf == host_conf),
    state        = case_when(
      is_host      ~ "Host nation",
      is_same_conf ~ "Same continent as host",
      TRUE         ~ "Different continent"
    ),
    year_label   = paste0(year, "\n", shorten_host(host)),   # column header
    team_label   = shorten(team),                            # cell text
    rank_num     = as.integer(rank)
  )

# Colour palettes — deliberately restrained to three values to keep the
# pattern legible at a glance
state_colours <- c(
  "Host nation"            = "#1D4ED8",   # strong blue
  "Same continent as host" = "#DBEAFE",   # pale blue
  "Different continent"    = "#E8E6E2"    # warm grey
)

# Text colours provide sufficient contrast against each fill
text_colours <- c(
  "Host nation"            = "white",
  "Same continent as host" = "#1E3A8A",
  "Different continent"    = "#6A5A4A"
)

# ── Chart 3: ggplot2 build
p <- ggplot(wc_long, aes(x = factor(year), y = fct_rev(rank))) +

  # Tile fill encodes host / same-continent / other
  geom_tile(
    aes(fill = state),
    colour = "white", linewidth = 0.6   # white border separates adjacent tiles
  ) +

  # Team abbreviation centred in each tile; colour matches fill contrast rules
  geom_text(
    aes(label = team_label, colour = state),
    size = 3.2, fontface = "bold", family = "ibmplexmono"
  ) +

  scale_fill_manual(
    values = state_colours,
    name   = NULL               # legend title suppressed; colours are self-explaining
  ) +

  scale_colour_manual(
    values = text_colours,
    guide  = "none"             # text colour follows fill; no separate legend needed
  ) +

  scale_x_discrete() +

  labs(
    title    = "The Top 4 *<span style='color:#1D4ED8'>Grid</span>*",
    subtitle = "When your continent hosts, your continent wins.",
    x = NULL, y = NULL,
    caption = "Source: worldcup$worldcups \u00b7 Confederation per FIFA membership \u00b7 ESADE MIBA Dataviz Club 2025"
  ) +

  theme_minimal(base_size = 12) +
  theme(
    plot.background    = element_rect(fill = "#FFFFFF", colour = NA),
    panel.background   = element_rect(fill = "#FFFFFF", colour = NA),
    panel.grid         = element_blank(),   # tiles provide their own grid structure
    axis.text.x        = element_text(size = 9, face = "bold", colour = "#3A3530",
                                      lineheight = 1.1, hjust = 0.5),
    axis.text.y        = element_text(size = 12, face = "bold", colour = "#3A3530"),
    axis.ticks         = element_blank(),
    plot.title         = element_markdown(size = 22, face = "bold",
                                          colour = "#1A140C", margin = margin(b = 6)),
    plot.subtitle      = element_text(size = 13, face = "bold", colour = "#4A5568",
                                      margin = margin(b = 12)),
    plot.caption       = element_text(size = 9, colour = "#8A7A68",
                                      hjust = 0, margin = margin(t = 8)),
    legend.position    = "bottom",
    legend.text        = element_text(size = 12, face = "bold", colour = "#3A3530"),
    legend.key.width   = unit(1.8, "cm"),
    legend.key.height  = unit(0.55, "cm"),
    plot.margin            = margin(16, 16, 12, 16),
    plot.title.position    = "plot"
  )


# DASHBOARD — ASSEMBLE AND EXPORT
# Layout:
#   Left column  (55% width) — Chart 1: Home Spike (tall, many rows)
#   Right column (45% width) — Chart 2: Scoring Boost (top, 55% of right height)
#                            — Chart 3: Top 4 Grid   (bottom, 45% of right height)
#
# wrap_elements(full = p_dash1) is critical: it tells patchwork to treat Chart 1
# as a single opaque block rather than attempting to align its internal panel
# edges with Chart 2/3. Without this, patchwork would align the data panels
# leaving a large dead zone at the top of the right column (because Chart 1's
# legend, title and subtitle consume ~35% of its height before the panel).

# ── Dashboard-scale theme overrides
# Each chart was designed for standalone output at its own scale. These overrides
# tighten margins and resize text slightly for the combined 32 × 18 inch canvas.

p_dash1 <- p_spike +
  theme(
    plot.title          = ggtext::element_markdown(
                            size = 34, face = "bold", colour = "#1A1A2E",
                            family = "ibmplexsans", margin = margin(b = 4)),
    plot.subtitle       = element_text(
                            size = 14, colour = "#6B7280", face = "bold",
                            family = "ibmplexsans", margin = margin(b = 10)),
    plot.margin         = margin(10, 10, 2, 12),
    plot.title.position = "panel",
    axis.text.x         = element_text(size = 11, colour = "#6B7280",
                                       family = "ibmplexmono", face = "bold"),
    axis.text.y         = element_text(size = 12, colour = "#1A1A2E", face = "bold",
                                       hjust = 1, margin = margin(r = 6)),
    legend.title        = element_text(size = 11, colour = "#6B7280",
                                       face = "bold", family = "ibmplexsans"),
    legend.text         = element_text(size = 11, colour = "#6B7280",
                                       family = "ibmplexmono"),
    plot.caption        = element_text(size = 9,  colour = "#9CA3AF",
                                       hjust = 0, family = "ibmplexmono")
  )

p_dash3 <- p_goals +
  theme(
    plot.title          = ggtext::element_markdown(
                            size = 34, face = "bold", colour = "#1A1A2E",
                            family = "ibmplexsans", margin = margin(b = 4)),
    plot.subtitle       = ggtext::element_markdown(
                            size = 13, face = "bold", family = "ibmplexsans",
                            margin = margin(b = 8)),
    plot.margin         = margin(2, 12, 4, 10),
    plot.title.position = "panel",
    axis.title.x        = element_text(size = 16, colour = "#3A3530",
                                       face = "bold", family = "ibmplexsans",
                                       margin = margin(t = 8)),
    axis.title.y        = element_text(size = 16, colour = "#3A3530",
                                       face = "bold", family = "ibmplexsans",
                                       margin = margin(r = 8)),
    axis.text           = element_text(size = 11, colour = "#6B7280",
                                       face = "bold", family = "ibmplexmono"),
    plot.caption        = element_text(size = 9,  colour = "#9CA3AF",
                                       hjust = 0, family = "ibmplexmono")
  )

p_dash4 <- p +
  theme(
    plot.background     = element_rect(fill = "#FFFFFF", colour = NA),
    panel.background    = element_rect(fill = "#FFFFFF", colour = NA),
    plot.title          = ggtext::element_markdown(size = 34, face = "bold",
                                                   colour = "#1A140C", margin = margin(b = 3)),
    plot.subtitle       = element_text(size = 12, face = "bold", colour = "#4A5568",
                                       margin = margin(b = 4)),
    plot.margin         = margin(4, 12, 10, 10),
    plot.title.position = "panel",
    axis.text.x         = element_text(size = 9.5, colour = "#3A3530",
                                       face = "bold", hjust = 0.5),
    axis.text.y         = element_text(size = 12,  colour = "#3A3530",
                                       face = "bold"),
    axis.ticks          = element_blank(),
    legend.position     = "bottom",
    legend.text         = element_text(size = 11, face = "bold", colour = "#3A3530"),
    legend.key.width    = unit(1.4, "cm"),
    legend.key.height   = unit(0.45, "cm")
  )

# ── Patchwork composition
# Step 1: stack Charts 2 and 3 in the right column (2.2 : 1.8 height ratio)
right_col <- (p_dash3 / p_dash4) + plot_layout(heights = c(2.2, 1.8))

# Step 2: place the Home Spike on the left, right column on the right
combined  <- (wrap_elements(full = p_dash1) | right_col) +
               plot_layout(widths = c(1.05, 1))

# ── SVG capture
# svgstring() redirects plot output to an in-memory SVG string instead of a
# file or screen device. The resulting SVG is embedded directly into the HTML
# body — no external image files required, making the output fully self-contained.
svg_str <- svglite::svgstring(width = 32, height = 18, pointsize = 11)
print(combined)
invisible(dev.off())

# ── HTML output
# The SVG is wrapped in a minimal HTML5 shell. Google Fonts are loaded via CDN
# so the IBM Plex typefaces render correctly in any web browser.
# The SVG uses width:100% / height:auto so the dashboard scales responsively.
html_out <- paste0(
  '<!DOCTYPE html>\n<html lang="en">\n<head>\n',
  '<meta charset="UTF-8"/>\n',
  '<meta name="viewport" content="width=device-width, initial-scale=1.0"/>\n',
  '<title>World Cup Dashboard \u2014 Home Field Advantage</title>\n',
  '<link href="https://fonts.googleapis.com/css2?family=IBM+Plex+Sans:ital,wght@0,400;0,500;0,600;0,700&family=IBM+Plex+Mono:wght@400;500;600&display=swap" rel="stylesheet"/>\n',
  '<style>\n',
  '*,*::before,*::after{box-sizing:border-box;margin:0;padding:0}\n',
  'html,body{background:#FAFAF8;width:100%;height:100%;overflow:auto;}\n',
  'svg{display:block;width:100%;height:auto;}\n',
  '</style>\n',
  '</head>\n<body>\n',
  as.character(svg_str()),
  '\n</body>\n</html>'
)

writeLines(html_out, "dashboard.html")
cat("\u2713 dashboard.html saved.\n")
