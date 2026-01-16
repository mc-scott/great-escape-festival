# main.R
# Main orchestrator script for Great Escape Festival data pipeline
# Coordinates web scraping and Spotify API data fetching

library(yaml)

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

c("gigs",
  "artist_details",
  "timetable",
  "venues",
  "artist_from",
  "spotify_playlist",
  "spotify_genres",
  "my_songs") |>
    walk(function(x){
        saveRDS(get(x), paste0("Data/RDS/", x, ".rds"))
        write.csv(get(x), paste0("Data/CSV/", x, ".csv"), row.names = FALSE)
    })

message("\nPipeline complete! All files saved to Data/RDS/ and Data/CSV/")
