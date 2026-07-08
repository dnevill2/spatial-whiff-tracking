library(dplyr)
library(tidyr)
library(ggplot2)
library(mgcv)

# 1. Load data and the baseline model ruler
ff_evaluated   <- readRDS("outputs/ff_hitter_predictions.rds")
baseline_model <- readRDS("outputs/ff_platoon_whiff_model.rds")

# 2. AUTOMATION LAYER: Identify the most underperforming hitter (Highest WOE)
target_leaderboard <- ff_evaluated %>%
  group_by(batter) %>%
  summarise(total_swings = n(), avg_woe = mean(woe)) %>%
  filter(total_swings >= 50) %>%
  arrange(desc(avg_woe))

target_hitter_id <- target_leaderboard$batter[1]
player_swings    <- ff_evaluated %>% filter(batter == target_hitter_id)
player_matchup   <- names(sort(table(player_swings$matchup), decreasing = TRUE))[1]

message(paste("Mapping Player ID:", target_hitter_id, "under Platoon Split:", player_matchup))

# 3. FIT PLAYER-SPECIFIC SURFACE

# Count unique coordinate combinations for this specific player
unique_points <- nrow(unique(player_swings[, c("plate_x", "plate_z")]))

# Dynamically set k to be safely lower than the number of unique points, maxing out at 8
chosen_k <- min(8, unique_points - 1)

if (chosen_k < 3) {
  stop("This player doesn't have enough unique swings to fit a meaningful spatial model.")
}

# Fit a localized GAM strictly on this player's swings to capture their unique shape
player_model <- gam(
  is_whiff ~ s(plate_x, plate_z, k = chosen_k), 
  family = binomial, 
  data = player_swings
)

# 4. GENERATE THE SPATIAL COORDINATE GRID
# Create a 100x100 resolution mesh grid covering the plate and chase zones
spatial_grid <- expand.grid(
  plate_x = seq(-2, 2, length.out = 100),
  plate_z = seq(0.5, 4.5, length.out = 100),
  matchup = factor(player_matchup, levels = levels(ff_evaluated$matchup))
)

# 5. GENERATE PREDICTIONS FOR BOTH PANELS
league_preds <- spatial_grid %>%
  mutate(
    prob = predict(baseline_model, newdata = ., type = "response"),
    type = "League Baseline"
  )

player_preds <- spatial_grid %>%
  mutate(
    prob = predict(player_model, newdata = ., type = "response"),
    type = paste("Player ID:", target_hitter_id)
  )

# Combine into a single plotting dataframe
plot_df <- bind_rows(league_preds, player_preds)

# 6. Plot
strike_zone_box <- geom_rect(
  aes(xmin = -0.85, xmax = 0.85, ymin = 1.6, ymax = 3.5),
  fill = NA, color = "white", linetype = "dashed", linewidth = 0.7
)

spatial_heatmap <- ggplot(plot_df, aes(x = plate_x, y = plate_z)) +
  # Continuous probability surface raster
  geom_raster(aes(fill = prob), interpolate = TRUE) +
  # Add probability contour lines
  stat_contour(aes(z = prob), color = "white", alpha = 0.2, breaks = seq(0.1, 0.9, by = 0.1)) +
  # Overlay the standard strike zone box
  strike_zone_box +
  # Use facet_wrap to create side-by-side comparison panels
  facet_wrap(~type) +
  # Front Office Color Palette (Rocket/Plasma emphasizes danger/whiff zones)
  scale_fill_viridis_c(
    option = "rocket", 
    labels = scales::percent_format(accuracy = 1),
    name = "Whiff Prob %"
  ) +
  # Coordinate normalization to ensure a 1:1 physical aspect ratio (1ft x = 1ft z)
  coord_fixed(xlim = c(-2, 2), ylim = c(0.5, 4.5)) +
  # Professional styling labels
  labs(
    title = paste("Spatial Whiff Probability Profile:", player_matchup),
    subtitle = "Player Development Analysis: Four-Seam Fastball Swings",
    x = "Horizontal Plate Location (Feet from Center)",
    y = "Vertical Plate Location (Height from Ground)",
    caption = "Source: MiLB Statcast Data | Model Framework: Binomial GAM"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.background = element_rect(fill = "#111625", color = NA),
    panel.background = element_rect(fill = "#111625", color = NA),
    text = element_text(color = "white"),
    plot.title = element_text(face = "bold", size = 16, margin = margin(b = 5)),
    plot.subtitle = element_text(color = "gray80", margin = margin(b = 15)),
    strip.text = element_text(face = "bold", size = 14, color = "white"),
    panel.grid = element_blank(),
    axis.text = element_text(color = "gray60"),
    axis.title = element_text(face = "bold")
  )

# Save image directly to your outputs folder
ggsave("outputs/hitter_spatial_comparison.png", plot = spatial_heatmap, width = 11, height = 6, dpi = 300)
message("Heatmap successfully written to outputs/hitter_spatial_comparison.png")
