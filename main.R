# main.R
# Main orchestrator script for Great Escape Festival data pipeline
# Coordinates web scraping and Spotify API data fetching

library(yaml)
library(DBI)
library(RSQLite)

# Source component scripts
source("scrape_great_escape.R")
source("fetch_spotify_data.R")

# Set-up ----

# Load Spotify API credentials
client_id <- yaml.load_file("credentials.yml")$client_id
client_secret <- yaml.load_file("credentials.yml")$client_secret

# Execute pipeline ----

# 1. Scrape Great Escape website
message("Step 1: Scraping Great Escape website...")
ge_data <- scrape_great_escape()

# Extract data objects
timetable <- ge_data$timetable
gigs <- ge_data$gigs
artists <- ge_data$artists
venues <- ge_data$venues

# 2. Fetch Spotify data
message("\nStep 2: Fetching Spotify data...")
spotify_data <- fetch_spotify_data(artists, client_id, client_secret)

# Extract data objects
artist_details <- spotify_data$artist_details
spotify_genres <- spotify_data$spotify_genres
artist_from <- spotify_data$artist_from
spotify_playlist <- spotify_data$spotify_playlist
my_songs <- spotify_data$my_songs

# Save files ----
message("\nStep 3: Saving output files...")

# Create SQLite database
db_path <- "db.sqlite"

# Connect to database (creates file if it doesn't exist)
con <- dbConnect(RSQLite::SQLite(), db_path)

# Write each table to the database
tables <- 
    c("gigs",
      "artists",
      "timetable",
      "venues") |>
    walk(function(tbl){
        dbWriteTable(con, tbl, get(tbl), overwrite = TRUE)
        message(sprintf("  Saved table: %s", tbl))
        })

# Disconnect from database
dbDisconnect(con)

message(sprintf("\nPipeline complete! Database saved to %s", db_path))
