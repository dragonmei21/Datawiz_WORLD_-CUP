# install.packages(c("tidyverse", "showtext", "ggrepel", "patchwork",
#                    "geosphere", "sf", "rnaturalearth", "rnaturalearthdata"))

# =========================DATA PREP====================================================
# data_prep.R
# World Cup Host Advantage — Data Preparation
#
# What this script does:
#   1. Loads the raw worldcup dataset
#   2. Builds host_data  — one row per tournament, with host's stage reached
#   3. Builds era_data   — summary stats grouped by era (for Chart 2)
#   4. Builds arc_data   — host + winner coordinates (for Chart 3)
#   5. Saves all three as .rds files so the chart scripts can load them fast
# =============================================================================

library(tidyverse)  # dplyr, stringr, ggplot2, etc.

# --------------------------------------------------------------------------
# LOAD RAW DATA
# The .RData file contains a list called `worldcup` with two data frames:
#   worldcup$wcmatches  — one row per match
#   worldcup$worldcups  — one row per tournament
# --------------------------------------------------------------------------

load("13_worldcup.RData")

wcmatches <- worldcup$wcmatches
worldcups  <- worldcup$worldcups

cat("wcmatches:", nrow(wcmatches), "rows\n")
cat("worldcups: ", nrow(worldcups), "rows\n")
cat("\nStage values in wcmatches:\n")
print(unique(wcmatches$stage))


# =============================================================================
# STEP 1A — HOST DATA
# For each tournament, find what stage the host country reached.
# We look at every match the host appeared in (as home OR away team),
# assign a numeric stage score, and take the maximum (furthest they got).
# =============================================================================

# -- Helper: convert stage string to a numeric score 1-6 --
# We use str_detect because stage names vary (e.g. "Group A", "Group B", etc.)
stage_to_score <- function(stage_str) {
  case_when(
    str_detect(stage_str, regex("final$", ignore_case = TRUE)) & 
      !str_detect(stage_str, regex("semi|quarter|third", ignore_case = TRUE)) ~ 6,  # "Final" only
    str_detect(stage_str, regex("third", ignore_case = TRUE))                  ~ 4,  # Third place = SF level
    str_detect(stage_str, regex("semi", ignore_case = TRUE))                   ~ 4,  # Semi-final
    str_detect(stage_str, regex("quarter", ignore_case = TRUE))                ~ 3,  # Quarter-final
    str_detect(stage_str, regex("round of 16|second round|last 16", 
                                ignore_case = TRUE))                           ~ 2,  # R16
    str_detect(stage_str, regex("group", ignore_case = TRUE))                  ~ 1,  # Group stage
    TRUE                                                                        ~ 1   # fallback
  )
}

# -- For each match, get its stage score --
wcmatches <- wcmatches %>%
  mutate(stage_score = stage_to_score(stage))

# -- For each tournament, find all matches the host played in --
# The host appears as either home_team or away_team.
# We also need to handle that some hosts are listed differently
# (e.g. "Korea Republic" vs "South Korea") — we'll cross-check below.

host_stage <- worldcups %>%
  select(year, host, winner) %>%
  rowwise() %>%
  mutate(
    # Find all matches where the host team played
    host_matches = list(
      wcmatches %>%
        filter(year == .data$year) %>%
        filter(home_team == host | away_team == host)
    ),
    # Also check if host won the tournament directly (score = 6)
    stage_reached = if (host == winner) {
      6L
    } else if (nrow(host_matches) == 0) {
      # No matches found — try to match with winner column as fallback
      1L
    } else {
      as.integer(max(host_matches$stage_score, na.rm = TRUE))
    }
  ) %>%
  ungroup() %>%
  select(year, host, winner, stage_reached)

# -- Add derived columns --
host_data <- host_stage %>%
  mutate(
    is_winner = (host == winner),
    
    # Human-readable label for the stage
    stage_label = case_when(
      stage_reached == 6 ~ "WIN",
      stage_reached == 5 ~ "FIN",
      stage_reached == 4 ~ "SF",
      stage_reached == 3 ~ "QF",
      stage_reached == 2 ~ "R16",
      TRUE               ~ "GRP"
    ),
    
    # Era grouping — 3 periods for Chart 2
    era = case_when(
      year <= 1966 ~ "Pre-1970",
      year <= 1994 ~ "1970–1994",
      TRUE         ~ "1998–2018"
    ),
    era = factor(era, levels = c("Pre-1970", "1970–1994", "1998–2018"))
  )

# -- Sanity check --
cat("\nhost_data preview:\n")
print(host_data %>% select(year, host, winner, stage_reached, stage_label, era))

# -- Quick fix: 2002 had TWO hosts (South Korea + Japan). 
#    worldcups lists them as "South Korea Japan" or similar. 
#    Check and handle:
cat("\n2002 host entry:", worldcups$host[worldcups$year == 2002], "\n")


# =============================================================================
# STEP 1B — ERA DATA
# Summary table by era — used for Chart 2 annotations
# =============================================================================

era_data <- host_data %>%
  group_by(era) %>%
  summarise(
    n               = n(),
    avg_stage       = mean(stage_reached),
    n_won           = sum(is_winner),
    n_final_or_better = sum(stage_reached >= 5),
    win_rate        = n_won / n,
    .groups = "drop"
  )

cat("\nera_data:\n")
print(era_data)


# =============================================================================
# STEP 1C — ARC DATA
# For each tournament: host country coordinates + winner country coordinates.
# Used in Chart 3 to draw arcs on a world map.
# Confederation data is added manually — it's not in the original dataset.
# =============================================================================

# -- Confederation lookup (manually defined) --
# Which confederation does each country belong to?
conf_lookup <- c(
  "Uruguay"       = "CONMEBOL",
  "Italy"         = "UEFA",
  "France"        = "UEFA",
  "Brazil"        = "CONMEBOL",
  "Switzerland"   = "UEFA",
  "Sweden"        = "UEFA",
  "Chile"         = "CONMEBOL",
  "England"       = "UEFA",
  "Mexico"        = "CONCACAF",
  "West Germany"  = "UEFA",
  "Germany"       = "UEFA",
  "Argentina"     = "CONMEBOL",
  "Spain"         = "UEFA",
  "United States" = "CONCACAF",
  "South Korea"   = "AFC",
  "Japan"         = "AFC",
  "South Africa"  = "CAF",
  "Russia"        = "UEFA",
  # Winners who never hosted
  "Netherlands"      = "UEFA",
  "Czechoslovakia"   = "UEFA",
  "Hungary"          = "UEFA",
  "Poland"           = "UEFA",
  "Croatia"          = "UEFA",
  "Portugal"         = "UEFA"
)

# -- Coordinate lookup (lon, lat) for map arc drawing --
# These are approximate country centroids
coord_lookup <- list(
  "Uruguay"       = c(-56.0, -33.0),
  "Italy"         = c(12.5,  41.9),
  "France"        = c(2.2,   46.2),
  "Brazil"        = c(-51.9, -14.2),
  "Switzerland"   = c(8.2,   46.8),
  "Sweden"        = c(18.6,  60.1),
  "Chile"         = c(-71.5, -35.7),
  "England"       = c(-1.2,  52.4),
  "Mexico"        = c(-102.6, 23.6),
  "West Germany"  = c(10.4,  51.2),
  "Germany"       = c(10.4,  51.2),
  "Argentina"     = c(-63.6, -38.4),
  "Spain"         = c(-3.7,  40.5),
  "United States" = c(-95.7, 37.1),
  "South Korea"   = c(127.8, 35.9),
  "Japan"         = c(138.3, 36.2),
  "South Africa"  = c(22.9, -30.6),
  "Russia"        = c(105.3, 61.5),
  "Netherlands"   = c(5.3,   52.1),
  "Czechoslovakia"= c(15.5,  49.8),
  "Hungary"       = c(19.5,  47.2),
  "Poland"        = c(19.1,  51.9),
  "Croatia"       = c(15.2,  45.1),
  "Portugal"      = c(-8.2,  39.4)
)

# -- Helper functions to pull lon/lat from the lookup --
get_lon <- function(country) {
  coords <- coord_lookup[[country]]
  if (is.null(coords)) NA_real_ else coords[1]
}
get_lat <- function(country) {
  coords <- coord_lookup[[country]]
  if (is.null(coords)) NA_real_ else coords[2]
}

# -- Build arc_data --
arc_data <- worldcups %>%
  select(year, host, winner) %>%
  mutate(
    # Normalise West Germany -> Germany, USA -> United States for the lookup
    host   = if_else(host   == "West Germany", "Germany",       host),
    winner = if_else(winner == "West Germany", "Germany",       winner),
    host   = if_else(host   == "USA",          "United States", host),
    winner = if_else(winner == "USA",          "United States", winner),
    
    # For 2002 co-hosts, simplify to South Korea (the team that advanced furthest)
    host = if_else(str_detect(host, "Korea|Japan") & year == 2002, "South Korea", host),
    
    # Confederation
    host_conf   = conf_lookup[host],
    winner_conf = conf_lookup[winner],
    same_conf   = host_conf == winner_conf,
    
    # Coordinates
    host_lon   = map_dbl(host,   get_lon),
    host_lat   = map_dbl(host,   get_lat),
    winner_lon = map_dbl(winner, get_lon),
    winner_lat = map_dbl(winner, get_lat)
  )

cat("\narc_data preview:\n")
print(arc_data %>% select(year, host, winner, host_conf, winner_conf, same_conf))
cat("\nSame confederation wins:", sum(arc_data$same_conf, na.rm = TRUE), "of", nrow(arc_data), "\n")

# -- Check for any NAs (means a country name wasn't in the lookup) --
missing <- arc_data %>%
  filter(is.na(host_conf) | is.na(winner_conf) | is.na(host_lon) | is.na(winner_lon))

if (nrow(missing) > 0) {
  cat("\nWARNING — these rows have missing values, check country name spelling:\n")
  print(missing %>% select(year, host, winner, host_conf, winner_conf))
} else {
  cat("\nAll lookups matched successfully.\n")
}


# =============================================================================
# SAVE ALL THREE DATA FRAMES
# =============================================================================

saveRDS(host_data, "host_data.rds")
saveRDS(era_data,  "era_data.rds")
saveRDS(arc_data,  "arc_data.rds")

cat("\n✓ Saved: host_data.rds, era_data.rds, arc_data.rds\n")
cat("You can now run chart1_spike.R, chart2_fade.R, chart3_arcs.R\n")