# =============================================================================
# Chart 1 - Home Spike.R  (pure ggplot2 rewrite)
# Generates chart1_spike.html from 13_worldcup.RData
#
# Approach: pure R throughout.
#   - Data wrangling : base R (identical logic to original)
#   - Visualisation  : ggplot2 (geom_point + geom_text)
#   - HTML output    : svglite::svgstring() → writeLines()
# =============================================================================

library(ggplot2)
library(ggtext)
library(showtext)
library(svglite)

font_add_google("IBM Plex Sans", "ibmplexsans")
font_add_google("IBM Plex Mono", "ibmplexmono")
showtext_auto()
showtext_opts(dpi = 96)

# ── Load and process data ─────────────────────────────────────────────────────
load("13_worldcup.RData")
m  <- worldcup$wcmatches
wc <- worldcup$worldcups

m$home_team[m$home_team == "West Germany"]       <- "Germany"
m$away_team[m$away_team == "West Germany"]       <- "Germany"
m$winning_team[m$winning_team == "West Germany"] <- "Germany"
wc$winner[wc$winner == "West Germany"]           <- "Germany"
wc$second[wc$second == "West Germany"]           <- "Germany"

stage_to_score <- function(s) {
  ifelse(grepl("Final$", s) & !grepl("Semi|Quarter|Third", s, ignore.case = TRUE), 6,
         ifelse(grepl("Third",   s, ignore.case = TRUE), 4,
                ifelse(grepl("Semi",    s, ignore.case = TRUE), 4,
                       ifelse(grepl("Quarter", s, ignore.case = TRUE), 3,
                              ifelse(grepl("Round of 16|Second Round|Last 16", s, ignore.case = TRUE), 2, 1)))))
}
m$stage_score <- stage_to_score(m$stage)

team_year <- rbind(
  data.frame(year = m$year, team = m$home_team, ss = m$stage_score),
  data.frame(year = m$year, team = m$away_team, ss = m$stage_score)
)
tr <- aggregate(ss ~ team + year, data = team_year, FUN = max)

for (i in 1:nrow(wc)) {
  tr$ss[tr$team == wc$winner[i] & tr$year == wc$year[i]] <- 6
  tr$ss[tr$team == wc$second[i] & tr$year == wc$year[i]] <- 5
}

# ── Nation order and display labels ──────────────────────────────────────────
host_map <- list(
  Uruguay = 1930, Italy = c(1934, 1990), France = c(1938, 1998),
  Brazil = c(1950, 2014), Switzerland = 1954, Sweden = 1958,
  Chile = 1962, England = 1966, Mexico = c(1970, 1986),
  Germany = c(1974, 2006), Argentina = 1978, Spain = 1982,
  `South Korea` = 2002, Russia = 2018
)

TARGET  <- c("Uruguay","Italy","France","Brazil","Switzerland","Sweden",
             "Chile","England","Mexico","Germany","Argentina","Spain",
             "South Korea","Russia")
DISPLAY <- c("Uruguay","Italy","France","Brazil","Switzerland","Sweden",
             "Chile","England","Mexico","Germany","Argentina","Spain",
             "S. Korea","Russia")

# ── Build plot data frame ─────────────────────────────────────────────────────
plot_df <- do.call(rbind, lapply(seq_along(TARGET), function(idx) {
  team <- TARGET[idx]
  hy   <- host_map[[team]]
  sub  <- tr[tr$team == team, ]
  if (nrow(sub) == 0) return(NULL)
  sub$display <- DISPLAY[idx]
  sub$is_host <- sub$year %in% hy
  sub
}))

# Remove any years beyond 2018 (Qatar 2022 not in this chart; prevents stray 2022 tick)
plot_df <- plot_df[!is.na(plot_df$year) & plot_df$year <= 2018, ]

# Factor: Uruguay at top (highest level), Russia at bottom (level 1)
plot_df$nation_f <- factor(plot_df$display, levels = rev(DISPLAY))
# Drop any rows whose display name didn't match DISPLAY — prevents "NA" y-axis label
plot_df <- plot_df[!is.na(plot_df$nation_f), ]
plot_df$ss_f     <- factor(plot_df$ss, levels = 1:6)

# Text colour inside "H" dot: white for darker stages (ss >= 3), dark blue otherwise
plot_df$h_textcol <- ifelse(plot_df$ss >= 3, "white", "#1E3A8A")

# ── Stage colour / size / label scales ───────────────────────────────────────
stage_fill   <- c("1"="#DBEAFE","2"="#93C5FD","3"="#3B82F6",
                  "4"="#1D4ED8","5"="#1E3A8A","6"="#172554")
# Stroke: contrast stroke for ss=1, matches fill for ss>1 (invisible border)
stage_stroke <- c("1"="#93BBEC","2"="#93C5FD","3"="#3B82F6",
                  "4"="#1D4ED8","5"="#1E3A8A","6"="#172554")
# Dot diameter in ggplot2 size units (proportional to original px radii: 4.5,6,8.5,11,15,19)
stage_size   <- c("1"=2.8,"2"=3.8,"3"=5.2,"4"=7.0,"5"=9.5,"6"=12.0)
stage_label  <- c("1"="Groups","2"="R16","3"="QF","4"="SF","5"="Final","6"="Winner")


# ── Host-year subsets ─────────────────────────────────────────────────────────
host_df <- plot_df[plot_df$is_host, ]

# Y-nudge: move year labels above each dot (distance proportional to dot size)
nudge_map        <- c("1"=0.24,"2"=0.28,"3"=0.32,"4"=0.38,"5"=0.46,"6"=0.54)
host_df$y_nudge  <- as.numeric(host_df$nation_f) +
                      nudge_map[as.character(host_df$ss)]

# Year labels: skip 1930 (it is already on the x-axis label)
# Guard against NA years that would render as "NA" text on the chart
year_label_df <- host_df[!is.na(host_df$year) & host_df$year != 1930, ]

# ── Alternating row backgrounds ───────────────────────────────────────────────
# Even visual rows from top (2nd, 4th, ...) = Italy, Brazil, Sweden, England,
# Germany, Spain, S.Korea, Russia
even_display <- DISPLAY[c(2, 4, 6, 8, 10, 12, 14)]
alt_bg_df <- data.frame(
  ymin = as.numeric(factor(even_display, levels = rev(DISPLAY))) - 0.5,
  ymax = as.numeric(factor(even_display, levels = rev(DISPLAY))) + 0.5
)

# ── ggplot2 chart ─────────────────────────────────────────────────────────────
p_spike <- ggplot(plot_df, aes(x = year, y = nation_f)) +

  # Alternating row background strips
  geom_rect(
    data        = alt_bg_df,
    aes(ymin = ymin, ymax = ymax, xmin = -Inf, xmax = Inf),
    fill        = "#F8F9FA",
    inherit.aes = FALSE
  ) +

  # Row baseline rules
  geom_hline(
    yintercept = seq_along(DISPLAY),
    colour     = "#E5E7EB",
    linewidth  = 0.35
  ) +

  # Stage circles (shape 21 = filled circle with stroke)
  geom_point(
    aes(fill = ss_f, size = ss_f, colour = ss_f),
    shape = 21, stroke = 0.5
  ) +

  # "H" label inside host-year dots
  geom_text(
    data    = host_df,
    aes(label = "H"),
    colour  = host_df$h_textcol,
    size    = 2.9,
    fontface = "bold",
    family  = "ibmplexmono"
  ) +

  # Year label above host-year dots (1930 suppressed)
  geom_text(
    data   = year_label_df,
    aes(x = year, y = y_nudge, label = as.character(year)),
    size   = 2.2,
    fontface = "bold",
    family = "ibmplexmono",
    colour = "#1A1A2E"
  ) +

  # ── Scales ────────────────────────────────────────────────────────────────
  scale_fill_manual(
    values = stage_fill[as.character(1:6)],
    labels = stage_label[as.character(1:6)],
    name   = "Stage reached \u2192"
  ) +
  scale_colour_manual(
    values = stage_stroke,
    guide  = "none"
  ) +
  scale_size_manual(
    values = stage_size[as.character(1:6)],
    labels = stage_label[as.character(1:6)],
    name   = "Stage reached \u2192"
  ) +
  scale_x_continuous(
    breaks = c(1930, 1950, 1966, 1982, 1998, 2014),
    limits = c(1927, 2019),
    expand = c(0, 0)
  ) +
  scale_y_discrete(expand = expansion(add = c(0.3, 0.7))) +

  guides(
    fill = guide_legend(
      override.aes = list(
        size   = unname(stage_size[as.character(1:6)]),
        colour = unname(stage_stroke[as.character(1:6)])
      ),
      nrow = 1
    ),
    size = "none"
  ) +

  # ── Labels ────────────────────────────────────────────────────────────────
  labs(
    title    = "The **Home** *<span style='color:#1D4ED8'>Spike</span>*",
    subtitle = "Every World Cup appearance for each host nation \u2014 how far they went\nAlmost every host nation's best result in history happened the year they played on home soil.",
    caption  = "H\u00a0= hosted that year \u00b7 Source: FIFA World Cup 1930\u20132018 \u00b7 ESADE MIBA Dataviz Club 2025",
    x = NULL, y = NULL
  ) +

  # ── Theme ─────────────────────────────────────────────────────────────────
  theme_minimal(base_size = 11, base_family = "ibmplexsans") +
  theme(
    plot.background    = element_rect(fill = "#FFFFFF", colour = NA),
    panel.background   = element_rect(fill = "#FFFFFF", colour = NA),
    panel.grid         = element_blank(),

    axis.text.x        = element_text(size = 11,  colour = "#6B7280",
                                      family = "ibmplexmono"),
    axis.text.y        = element_text(size = 13, colour = "#1A1A2E",
                                      face = "bold", hjust = 1,
                                      margin = margin(r = 8)),
    axis.ticks.x       = element_line(colour = "#1A1A2E", linewidth = 0.5),
    axis.ticks.y       = element_blank(),
    axis.ticks.length.x = unit(4, "pt"),

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
    legend.justification = "left",
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
    plot.title.position     = "plot"
  )

# ── Capture SVG and write HTML ────────────────────────────────────────────────
if (!exists("DASHBOARD_SOURCE")) {
  svg_str <- svglite::svgstring(width = 13, height = 9, pointsize = 11)
  print(p_spike)
  dev.off()

  html_out <- paste0(
    '<!DOCTYPE html>\n<html lang="en">\n<head>\n',
    '<meta charset="UTF-8"/>\n',
    '<meta name="viewport" content="width=device-width, initial-scale=1.0"/>\n',
    '<title>Chart 1 \u2014 The Home Spike</title>\n',
    '<link href="https://fonts.googleapis.com/css2?family=IBM+Plex+Sans:ital,wght@0,400;0,500;0,600;0,700&family=IBM+Plex+Mono:wght@400;500;600&display=swap" rel="stylesheet"/>\n',
    '<style>\n',
    '*,*::before,*::after{box-sizing:border-box;margin:0;padding:0}\n',
    'html,body{background:#FFFFFF;width:100%;}\n',
    'svg{display:block;width:100%;height:auto;}\n',
    '</style>\n',
    '</head>\n<body>\n',
    as.character(svg_str()),
    '\n</body>\n</html>'
  )

  writeLines(html_out, "chart1_spike.html")
  cat("\u2713 chart1_spike.html saved.\n")
}
