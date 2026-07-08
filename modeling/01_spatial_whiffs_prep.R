library(DBI)
library(RSQLite)
library(dplyr)
library(tidyr)

# 1. Connect to your database
con <- dbConnect(RSQLite::SQLite(), "milb_analytics.sqlite")
raw_data <- dbGetQuery(con, "SELECT * FROM milb_statcast")

# Definitive list of Statcast descriptions that constitute a swing
swing_descriptions <- c(
  "swinging_strike", 
  "Swinging Strike (Blocked)", 
  "foul", 
  "in_play,_run(s)", 
  "in_play,_out(s)", 
  "in_play,_no_out", 
  "foul_tip", 
  "foul_bunt", 
  "missed_bunt"
)

# List of descriptions that specifically mean a swing-and-miss
whiff_descriptions <- c(
  "swinging_strike", 
  "foul_tip",
  "missed_bunt"
)

# 2. Filter and Engineer for Spatial Whiffs
spatial_whiff_df <- raw_data %>%
  # Filter strictly to pitches with valid spatial tracking data
  filter(!is.na(plate_x), !is.na(plate_z)) %>%
  # Filter strictly to the universe of SWINGS
  filter(description %in% swing_descriptions) %>%
  mutate(
    # Create our binary target: 1 = Whiff, 0 = Contact
    is_whiff = if_else(description %in% whiff_descriptions, 1, 0),
  ) %>%
  select(game_pk, batter, pitch_type, p_throws, stand, plate_x, plate_z, is_whiff)

# 3. Spatial Density & Whiff Baseline Report
message("--- Baseline Whiff Rates by Pitch Type ---")
whiff_report <- spatial_whiff_df %>%
  group_by(pitch_type) %>%
  summarise(
    total_swings = n(),
    total_whiffs = sum(is_whiff),
    whiff_rate   = mean(is_whiff)
  ) %>%
  filter(total_swings > 10) %>% # Filter out rare/misclassified pitches
  arrange(desc(whiff_rate))

print(whiff_report)

# Save this specific modeling dataframe locally
saveRDS(spatial_whiff_df, "outputs/spatial_whiff_modeling_data.rds")
dbDisconnect(con)
