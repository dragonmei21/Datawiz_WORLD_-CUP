# =============================================================================
# Chart 3 - Goals.R  (pure ggplot2 rewrite)
# Generates chart3_goals.html from 13_worldcup.RData
#
# Approach: pure R throughout.
#   - Data wrangling : base R (identical logic to original)
#   - Visualisation  : ggplot2 (scatter, ggflags for circular flags)
#   - HTML output    : svglite::svgstring() → writeLines()
# =============================================================================

library(ggplot2)
library(ggtext)
library(ggflags)
library(showtext)
library(svglite)
library(ggrepel)

font_add_google("IBM Plex Sans", "ibmplexsans")
font_add_google("IBM Plex Mono", "ibmplexmono")
showtext_auto()
showtext_opts(dpi = 96)

# ── Load and process data ─────────────────────────────────────────────────────
load("13_worldcup.RData")
m <- worldcup$wcmatches

m$home_team[m$home_team == "West Germany"] <- "Germany"
m$away_team[m$away_team == "West Germany"] <- "Germany"

host_map <- list(
  Uruguay = 1930, Italy = c(1934, 1990), France = c(1938, 1998),
  Brazil = c(1950, 2014), Switzerland = 1954, Sweden = 1958,
  Chile = 1962, England = 1966, Mexico = c(1970, 1986),
  Germany = c(1974, 2006), Argentina = 1978, Spain = 1982,
  `United States` = 1994, `South Korea` = 2002,
  Japan = 2002, `South Africa` = 2010, Russia = 2018
)

DISPLAY <- c(Uruguay="Uruguay", Italy="Italy", France="France", Brazil="Brazil",
             Switzerland="Switzerland", Sweden="Sweden", Chile="Chile",
             England="England", Mexico="Mexico", Germany="Germany",
             Argentina="Argentina", Spain="Spain", `United States`="USA",
             `South Korea`="S. Korea", Japan="Japan",
             `South Africa`="S. Africa", Russia="Russia")

# ggflags uses ISO-2 lowercase codes; England → "gb" (closest available)
CODES <- c(Uruguay="uy", Italy="it", France="fr", Brazil="br",
           Switzerland="ch", Sweden="se", Chile="cl", England="gb",
           Mexico="mx", Germany="de", Argentina="ar", Spain="es",
           `United States`="us", `South Korea`="kr", Japan="jp",
           `South Africa`="za", Russia="ru")

# ── Compute goals per game: hosting vs not hosting ────────────────────────────
rows <- lapply(names(host_map), function(team) {
  hy          <- host_map[[team]]
  hg_df       <- m[m$home_team == team, c("year", "home_score")]
  ag_df       <- m[m$away_team == team, c("year", "away_score")]
  names(hg_df) <- c("year", "goals")
  names(ag_df) <- c("year", "goals")
  tg          <- rbind(hg_df, ag_df)
  hosting     <- tg[tg$year %in% hy, ]
  not_hosting <- tg[!tg$year %in% hy, ]
  if (nrow(hosting) == 0 || nrow(not_hosting) == 0) return(NULL)
  h_gpg <- mean(hosting$goals,     na.rm = TRUE)
  a_gpg <- mean(not_hosting$goals, na.rm = TRUE)
  diff  <- h_gpg - a_gpg
  data.frame(
    team  = team,
    t     = unname(DISPLAY[team]),
    hg    = h_gpg,
    ag    = a_gpg,
    diff  = diff,
    boost = diff > 0,
    code  = unname(CODES[team]),
    stringsAsFactors = FALSE
  )
})
dat <- do.call(rbind, rows)

n_boost <- sum(dat$boost)
n_total <- nrow(dat)

# Diff label (+/-X.XX) shown above each flag
dat$d_str <- sprintf("%+.2f", dat$diff)

# ── Zone shading polygons ─────────────────────────────────────────────────────
# Find data range and add 10% padding
x_pad <- 0.12
y_pad <- 0.12
xMin  <- max(0, floor(min(dat$ag) * 10) / 10 - x_pad)
yMin  <- max(0, floor(min(dat$hg) * 10) / 10 - y_pad)
xMax  <- ceiling(max(dat$ag) * 10) / 10 + x_pad
yMax  <- ceiling(max(dat$hg) * 10) / 10 + y_pad

upper_zone <- data.frame(
  x = c(xMin, xMin, xMax, xMax),
  y = c(yMin, yMax, yMax, xMax)
)
lower_zone <- data.frame(
  x = c(xMin, xMax, xMax),
  y = c(xMin, xMax, yMin)
)

# ── ggplot2 chart ─────────────────────────────────────────────────────────────
p_goals <- ggplot(dat, aes(x = ag, y = hg)) +

  # Zone shading
  geom_polygon(data = upper_zone, aes(x = x, y = y),
               fill = "#1D4ED80D", inherit.aes = FALSE) +
  geom_polygon(data = lower_zone, aes(x = x, y = y),
               fill = "#1E3A8A08", inherit.aes = FALSE) +

  # Zone italic labels
  annotate("text", x = xMin + 0.08, y = yMax - 0.12,
           label = "Teams score more when hosting",
           hjust = 0, size = 3.5, colour = "#1D4ED8",
           alpha = 0.4, fontface = "italic", family = "ibmplexsans") +
  annotate("text", x = xMax - 0.08, y = yMin + 0.12,
           label = "Teams score less when hosting",
           hjust = 1, size = 3.5, colour = "#172554",
           alpha = 0.28, fontface = "italic", family = "ibmplexsans") +

  # y = x diagonal
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", colour = "#CBD5E1", linewidth = 1.2) +

  # Black border circle drawn behind flags
  geom_point(aes(x = ag, y = hg),
             shape = 21, size = 11.5,
             fill = "black", colour = "black") +

  # Circular flag images
  geom_flag(aes(country = code), size = 10) +

  # Diff value label (+/-X.XX) above each flag — repelled to avoid overlaps
  ggrepel::geom_text_repel(
    aes(label = d_str),
    nudge_y            = 0.14,
    nudge_x            = 0.0,
    direction          = "both",
    size               = 4.2,
    colour             = "#172554",
    fontface           = "bold",
    family             = "ibmplexsans",
    segment.colour     = "#CBD5E1",
    segment.size       = 0.4,
    min.segment.length = 0.1,
    box.padding        = 0.3,
    point.padding      = 0.5,
    max.overlaps       = Inf,         # never drop a label
    force              = 3,
    seed               = 42           # reproducible layout
  ) +

  # ── Scales ──────────────────────────────────────────────────────────────
  scale_x_continuous(limits = c(xMin, xMax), breaks = scales::pretty_breaks(n = 5),
                     expand = c(0.02, 0)) +
  scale_y_continuous(limits = c(yMin, yMax), breaks = scales::pretty_breaks(n = 5),
                     expand = c(0.02, 0)) +

  # ── Labels ──────────────────────────────────────────────────────────────
  labs(
    title    = paste0("The Scoring *<span style='color:#1D4ED8'>Boost</span>*"),
    subtitle = paste0("<b style='color:#1D4ED8;font-size:16pt'>", n_boost,
                      "</b><span style='color:#6B7280'> of ", n_total,
                      " hosts scored more per game when hosting</span>"),
    x = "Goals per game \u2014 not hosting",
    y = "Goals per game \u2014 hosting",
    caption = "Data: FIFA World Cup 1930\u20132018 \u00b7 ESADE MIBA Dataviz Club 2025"
  ) +

  # ── Theme ────────────────────────────────────────────────────────────────
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

# ── Capture SVG and write HTML ────────────────────────────────────────────────
if (!exists("DASHBOARD_SOURCE")) {
  svg_str <- svglite::svgstring(width = 11, height = 9.5, pointsize = 13)
  print(p_goals)
  dev.off()

  html_out <- paste0(
    '<!DOCTYPE html>\n<html lang="en">\n<head>\n',
    '<meta charset="UTF-8"/>\n',
    '<meta name="viewport" content="width=device-width, initial-scale=1.0"/>\n',
    '<title>Chart 3 \u2014 The Scoring Boost</title>\n',
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

  writeLines(html_out, "chart3_goals.html")
  cat("\u2713 chart3_goals.html saved.\n")
}
