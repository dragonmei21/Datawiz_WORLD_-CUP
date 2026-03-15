## chart4_timeline.R
## Chart 4 — "Home Field, Home Continent" (top-4 timeline)
##
## Approach: pure R throughout.
##   - Data wrangling : tidyverse
##   - Visualisation  : ggplot2  (geom_tile + geom_text)
##   - HTML output    : svglite::svgstring()  →  writeLines()
##
## Visual logic — cell fill, three states:
##   Dark blue  (#2A4E8A) = host nation placed here
##   Light blue (#A8C8E8) = same continent as host (not the host itself)
##   Warm grey  (#CECDCA) = everyone else
##
## Output: chart4_timeline.html  (self-contained, no external dependencies)

library(tidyverse)
library(svglite)
library(showtext)

font_add_google("IBM Plex Sans", "ibmplexsans")
font_add_google("IBM Plex Mono", "ibmplexmono")
showtext_auto()
showtext_opts(dpi = 96)

# ── Confederation helper ───────────────────────────────────────────────────────
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

get_conf <- function(team) {
  if_else(team == "Japan, South Korea", "AFC", unname(conf_map[team]))
}

# ── Short team name lookup ─────────────────────────────────────────────────────
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

# ── Load & extend data ────────────────────────────────────────────────────────
load("13_worldcup.RData")

wc <- bind_rows(
  worldcup$worldcups %>% select(year, host, winner, second, third, fourth),
  tibble(year = 2022, host = "Qatar", winner = "Argentina",
         second = "France", third = "Croatia", fourth = "Morocco")
)

# ── Build long / tidy data frame ─────────────────────────────────────────────
rank_levels  <- c("winner", "second", "third", "fourth")
rank_labels  <- c("1st", "2nd", "3rd", "4th")

wc_long <- wc %>%
  pivot_longer(
    cols      = all_of(rank_levels),
    names_to  = "rank",
    values_to = "team"
  ) %>%
  mutate(
    rank       = factor(rank, levels = rank_levels, labels = rank_labels),
    host_conf  = get_conf(host),
    team_conf  = get_conf(team),
    is_host    = (team == host) |
                   (host == "Japan, South Korea" & team %in% c("Japan","South Korea")),
    is_same_conf = !is_host & (team_conf == host_conf),
    state      = case_when(
      is_host      ~ "Host nation",
      is_same_conf ~ "Same continent as host",
      TRUE         ~ "Different continent"
    ),
    # Column header label: year + host code
    year_label  = paste0(year, "\n", shorten_host(host)),
    team_label  = shorten(team),
    # Numeric y for ggplot (1st at top)
    rank_num    = as.integer(rank)
  )

# ── Colour palette ────────────────────────────────────────────────────────────
state_colours <- c(
  "Host nation"            = "#1D4ED8",
  "Same continent as host" = "#DBEAFE",
  "Different continent"    = "#E8E6E2"
)

text_colours <- c(
  "Host nation"            = "white",
  "Same continent as host" = "#1E3A8A",
  "Different continent"    = "#6A5A4A"
)

# ── ggplot2 chart ─────────────────────────────────────────────────────────────
p <- ggplot(wc_long, aes(x = factor(year), y = fct_rev(rank))) +

  geom_tile(
    aes(fill = state),
    colour = "white", linewidth = 0.6
  ) +

  geom_text(
    aes(label = team_label, colour = state),
    size = 4.5, fontface = "bold", family = "ibmplexmono"
  ) +

  scale_fill_manual(
    values = state_colours,
    name   = NULL
  ) +

  scale_colour_manual(
    values = text_colours,
    guide  = "none"
  ) +

  scale_x_discrete() +

  labs(
    title    = "The Top 4 Grid",
    subtitle = "When your continent hosts, your continent wins.",
    x = NULL, y = NULL,
    caption = "Source: worldcup$worldcups · Confederation per FIFA membership · 2022 added manually · ESADE MIBA Dataviz Club 2025"
  ) +

  theme_minimal(base_size = 12) +
  theme(
    plot.background    = element_rect(fill = "#FFFFFF", colour = NA),
    panel.background   = element_rect(fill = "#FFFFFF", colour = NA),
    panel.grid         = element_blank(),
    axis.text.x        = element_text(size = 9, face = "bold", colour = "#3A3530",
                                      lineheight = 1.1, hjust = 0.5),
    axis.text.y        = element_text(size = 12, face = "bold", colour = "#3A3530"),
    axis.ticks         = element_blank(),
    plot.title         = element_text(size = 22, face = "bold",
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

# ── Capture SVG and write static HTML ────────────────────────────────────────
if (!exists("DASHBOARD_SOURCE")) {

svg_str <- svglite::svgstring(width = 16, height = 6, pointsize = 12)
print(p)
dev.off()

html_out <- paste0(
  '<!DOCTYPE html>\n<html lang="en">\n<head>\n',
  '<meta charset="UTF-8"/>\n',
  '<meta name="viewport" content="width=device-width, initial-scale=1.0"/>\n',
  '<title>World Cup Top 4 \u2014 Home Field Advantage</title>\n',
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

writeLines(html_out, "chart4_timeline.html")
cat("\u2713 chart4_timeline.html saved.\n")

} # end if (!exists("DASHBOARD_SOURCE"))
