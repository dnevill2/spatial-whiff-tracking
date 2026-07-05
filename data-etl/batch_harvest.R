library(baseballr)
library(dplyr)
library(purrr)
library(DBI)
library(RSQLite)

# Source your verified cleaning function from yesterday
source("data-etl/clean_schema.R")

# 1. Connect to your local database (Creates it if it doesn't exist)
db_path <- "milb_analytics.sqlite"
con <- dbConnect(RSQLite::SQLite(), db_path)

# 2. Define a short 3-day window for tomorrow's integration test
test_dates <- seq(as.Date("2025-06-15"), as.Date("2025-06-16"), by = "days")

# 3. The automated pipeline engine
harvest_and_store_milb <- function(target_date, connection) {
  message(paste("Processing Date:", target_date))
  
  # Fetch all AAA game keys
  schedule <- tryCatch({
    mlb_game_pks(date = target_date, level_ids = 11)
  }, error = function(e) return(NULL))
  
  # BULLETPROOF CHECK: Handle NULL, non-dataframes (like API error lists), or 0-row dataframes
  if (is.null(schedule) || !is.data.frame(schedule) || nrow(schedule) == 0) {
    message(paste("No scheduled games or invalid data found for:", target_date))
    return(data.frame())
  }
  
  # Double-check that the expected status column exists before filtering
  if (!"status.abstractGameState" %in% names(schedule)) {
    message(paste("Missing status columns for:", target_date))
    return(data.frame())
  }
  
  # Filter strictly for completed games
  game_ids <- schedule %>% 
    filter(status.abstractGameState == "Final") %>% 
    pull(game_pk)
  
  if (length(game_ids) == 0) {
    message(paste("No completed games found for:", target_date))
    return(data.frame())
  }
  
  # Accumulator for the day's play-by-play data
  days_pbp <- list()
  
  for (pk in game_ids) {
    message(paste("Scraping Game:", pk))
    Sys.sleep(0.5) # Guard rail for API rate limits
    
    raw_pbp <- tryCatch({ mlb_pbp(game_pk = pk) }, error = function(e) NULL)
    
    # Defensive check on play-by-play data structure
    if (!is.null(raw_pbp) && is.data.frame(raw_pbp) && nrow(raw_pbp) > 0) {
      clean_pbp <- clean_and_map_milb(raw_pbp)
      
      if (nrow(clean_pbp) > 0) {
        days_pbp[[as.character(pk)]] <- clean_pbp
      }
    }
  }
  
  if (length(days_pbp) == 0) return(data.frame())
  
  # Combine and upload to DB
  final_daily_df <- bind_rows(days_pbp)
  
  tryCatch({
    dbWriteTable(connection, "milb_pbp", as.data.frame(final_daily_df), append = TRUE, row.names = FALSE)
    message(paste("Successfully stored", nrow(final_daily_df), "rows for", target_date))
  }, error = function(e) {
    message(paste("Database write failed for", target_date, ":", e$message))
  })
  
  return(final_daily_df)
}

# 4. Run the 3-day test execution loop
walk(test_dates, ~harvest_and_store_milb(.x, con))

# 5. Verification Check: Count how many rows successfully landed
row_count <- dbGetQuery(con, "SELECT COUNT(*) as total_pitches FROM milb_statcast")
print(row_count)

# 6. Always disconnect to close file handles cleanly
dbDisconnect(con)
