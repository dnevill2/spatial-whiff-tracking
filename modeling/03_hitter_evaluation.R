library(dplyr)
library(mgcv)

# 1. Load your dataset and your baseline model ruler
whiff_data    <- readRDS("outputs/spatial_whiff_modeling_data.rds")
baseline_model <- readRDS("outputs/ff_platoon_whiff_model.rds")

# 2. Reconstruct the matchup factor matching the model requirements
ff_swings <- whiff_data %>% 
  filter(pitch_type == "FF") %>%
  mutate(matchup = as.factor(paste0(p_throws, "_vs_", stand)))

message("Generating spatial predictions for every swing...")

# 3. Step 1 & 2: Calculate Whiff Over Expected (WOE)
ff_evaluated <- ff_swings %>%
  mutate(
    # Get the exact league-average probability for this location/matchup
    exp_whiff_prob = predict(baseline_model, newdata = ., type = "response"),
    
    # Residual calculation: Positive = Worse than league average, Negative = Better
    woe = is_whiff - exp_whiff_prob
  )

# 4. Step 3: Aggregate to the Hitter Level
hitter_leaderboard <- ff_evaluated %>%
  group_by(batter) %>% 
  summarise(
    total_swings   = n(),
    actual_whiffpct = mean(is_whiff) * 100,
    expected_whiffpct = mean(exp_whiff_prob) * 100,
    # Cumulative metric: negative is elite (making more contact than expected)
    avg_woe        = mean(woe) * 100 
  ) %>%
  filter(total_swings >= 50) %>% # Protect against tiny sample noise
  arrange(desc(avg_woe))

message("--- Player Dev Priority List (Highest WOE = Biggest Holes) ---")
print(head(hitter_leaderboard, 10))

# Save this evaluated dataset for tomorrow's visualization layer
saveRDS(ff_evaluated, "outputs/ff_hitter_predictions.rds")
