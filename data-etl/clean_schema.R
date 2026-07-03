library(dplyr)
library(stringr)

clean_and_map_milb <- function(raw_milb_df) {
  
  # 1. Check if the critical tracking columns exist in this payload
  if (!"pitchData.coordinates.pX" %in% names(raw_milb_df)) {
    warning("Payload contains no tracking metrics. Skipping dataset.")
    return(data.frame())
  }
  
  processed_df <- raw_milb_df %>%
    # 2. Filter strictly for rows with active trackman/hawkeye coordinates
    dplyr::filter(!is.na(`pitchData.coordinates.pX`)) %>%
    
    # 3. Apply the dictionary translation mapping
    dplyr::select(
      game_pk       = `game_pk`,
      pitch_type    = `details.type.code`,
      release_speed = `pitchData.startSpeed`,
      description   = `details.description`,
      batter        = `matchup.batter.id`,
      pitcher       = `matchup.pitcher.id`,
      balls         = `count.balls.start`,
      strikes       = `count.strikes.start`,
      pitch_number  = `pitchNumber`,
      plate_x       = `pitchData.coordinates.pX`,
      plate_z       = `pitchData.coordinates.pZ`,
      pfx_x         = `pitchData.coordinates.pfxX`,
      pfx_z         = `pitchData.coordinates.pfxZ`,
      stand         = `matchup.batSide.code`,
      p_throws      = `matchup.pitchHand.code`,
      inning        = `about.inning`,
      is_top        = `about.isTopInning`,
      events        = `result.event`,
      bb_type       = `hitData.trajectory`
    ) %>%
    
    # 4. Standardize data formats to match standard MLB Savant styling
    dplyr::mutate(
      # Convert 'Called Strike' -> 'called_strike'
      description = stringr::str_replace_all(tolower(description), " ", "_"),
      description = dplyr::case_when(
        description == "foul_tip" ~ "foul_tip",
        stringr::str_detect(description, "swinging_strike") ~ "swinging_strike",
        TRUE ~ description
      ),
      
      # Transform logical Top/Bottom inning flag to characters
      inning_topbot = dplyr::if_else(is_top, "top", "bot"),
      
      # Force data type consistency across systems
      plate_x       = as.numeric(plate_x),
      plate_z       = as.numeric(plate_z),
      release_speed = as.numeric(release_speed),
      game_pk       = as.integer(game_pk)
    ) %>%
    
    # Remove temporary calculation columns
    dplyr::select(-is_top)
  
  return(processed_df)
}