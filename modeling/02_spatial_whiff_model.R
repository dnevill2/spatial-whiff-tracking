library(dplyr)
library(mgcv)

# 1. Load clean data
whiff_data <- readRDS("outputs/spatial_whiff_modeling_data.rds")

# 2. Filter to Fastballs and construct the explicit Platoon Matchup factor
ff_swings <- whiff_data %>% 
  filter(pitch_type == "FF") %>%
  mutate(
    # Create an explicit factor for the four possible hand matchups
    matchup = as.factor(paste0(p_throws, "_vs_", stand))
  )

message("Fitting Platoon-Interactive GAM...")

# 3. Fit the Interactive GA
# 'by = matchup' forces R to calculate a completely unique spatial surface for each platoon split
ff_platoon_model <- gam(
  is_whiff ~ matchup + s(plate_x, plate_z, by = matchup, k = 15), 
  family = binomial, 
  data = ff_swings
)

# 4. Diagnostics Check
message("--- Platoon Model Summary ---")
print(summary(ff_platoon_model))

# Save the interactive model for our visualization step
saveRDS(ff_platoon_model, "outputs/ff_platoon_whiff_model.rds")
