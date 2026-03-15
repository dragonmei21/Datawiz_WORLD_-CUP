# =============================================================================
# dashboard.R
# Combines Chart 1 (Home Spike), Chart 3 (Goals), Chart 4 (Timeline)
# into a single HTML page.
#
# Layout:
#   Left half      → Chart 1 (Home Spike)   — full height
#   Right top half → Chart 3 (Scoring Boost) — 60% of right height
#   Right bot half → Chart 4 (Timeline)      — 40% of right height
#
# Output: dashboard.html (self-contained SVG embedded in HTML)
# =============================================================================

library(patchwork)
library(svglite)
library(showtext)

font_add_google("IBM Plex Sans", "ibmplexsans")
font_add_google("IBM Plex Mono", "ibmplexmono")
showtext_auto()
showtext_opts(dpi = 96)

# ── Source each chart with DASHBOARD_SOURCE flag set ─────────────────────────
# Each script checks !exists("DASHBOARD_SOURCE") before saving HTML.
# Scripts build: p_spike (Chart 1), p_goals (Chart 3), p (Chart 4)
DASHBOARD_SOURCE <- TRUE
source("Chart 1 - Home Spike.R",  local = FALSE)
source("Chart 3 - Goals.R",       local = FALSE)
source("chart4_timeline.R",       local = FALSE)

# ── Adjust themes for tighter dashboard margins ───────────────────────────────
p_dash1 <- p_spike +
  theme(
    plot.title          = ggtext::element_markdown(
                            size = 34, face = "bold", colour = "#1A1A2E",
                            family = "ibmplexsans", margin = margin(b = 4)),
    plot.subtitle       = element_text(
                            size = 14, colour = "#6B7280", face = "bold",
                            family = "ibmplexsans", margin = margin(b = 10)),
    plot.margin         = margin(0, 10, 2, 12),
    plot.title.position = "plot",
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
    plot.margin         = margin(0, 12, 4, 10),
    plot.title.position = "plot",
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
    plot.title          = element_text(size = 34, face = "bold",
                                       colour = "#1A140C", margin = margin(b = 3)),
    plot.subtitle       = element_text(size = 12, face = "bold", colour = "#4A5568",
                                       margin = margin(b = 4)),
    plot.margin         = margin(4, 12, 10, 10),
    plot.title.position = "plot",
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

# ── Combine with patchwork ────────────────────────────────────────────────────
right_col <- (p_dash3 / p_dash4) +
  plot_layout(heights = c(1.5, 2.5)) &
  theme(plot.margin = margin(0, 10, 0, 10))

combined  <- (p_dash1 | right_col) +
  plot_layout(widths = c(1.05, 1), guides = "keep") &
  theme(plot.background = element_rect(fill = "#FAFAF8", colour = NA))

# ── Render to SVG string ──────────────────────────────────────────────────────
svg_str <- svglite::svgstring(width = 32, height = 20, pointsize = 11)
print(combined)
invisible(dev.off())

# ── Write HTML ────────────────────────────────────────────────────────────────
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
