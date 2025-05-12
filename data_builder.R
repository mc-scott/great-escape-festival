library(rvest)
library(httr)
library(yaml)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)

# load creds ----
client_id <- yaml.load_file("credentials.yml")$client_id
client_secret <- yaml.load_file("credentials.yml")$client_secret

# GE website scraper functions ----

# get next artist URL from 'next' chevron on page
get_next_artist <- function(artist_url){
    read_html(artist_url) |> 
        html_elements(".chev-right") |> 
        html_attr("href")  
}

# get artist details from page

## name
artist_name <- function(artist_url){
    read_html(artist_url) |> 
        html_element(".article__title--single") |> 
        html_text() 
}

## artist location
artist_from <- function(artist_url){
    read_html(artist_url) |>
        html_element(xpath = '/html/body/div[2]/div/div/div/article/h2') |>
        html_text() |> 
        as.character() |> 
        str_sub(2, -2) # remove first '(' and last ')'characters
}

# get venues and times

## NOTE: an artist can have multiple gigs, need to loop through all
num_events <- function(artist_url){
    read_html(artist_url) |>
        html_elements(css = "div.event") |> 
        length()
}

event_venues <- function(artist_url) {
    # get number of rows in event grid
    num_events <- num_events(artist_url)    
    # clear variable
    venues = "NA"
    # loop through rows in event grid, finding artist venue
    for (x in 1:num_events) {
        venue <- read_html(artist_url) |>
            html_element(xpath = paste0('/html/body/div[2]/div/div/div/article/div/div[', x+1, ']/div/a')) |> 
            html_attr("title")
        venues <- paste(venues, venue, sep = ", ")
        venues <- str_remove(venues, "NA, ")
    }
    return(venues)
}

event_times <- function(artist_url) {
    # get number of rows in events grid
    num_events <- num_events(artist_url)
    # clear variable
    times = "NA"
    # loop through html elements in event grid, finding gig times
    for (x in 1:num_events) {
        event_time <- read_html(artist_url) |>
            html_element(xpath = paste0('/html/body/div[2]/div[1]/div/div/article/div/div[', x+1, ']/div[2]')) |>
            html_text2() |> 
            str_remove("\r ")
        times <- paste(times, event_time, sep = ", ")
        times <- str_remove(times, "NA, ")
    }
    return(times)
}

# get blurb

artist_blurb <- function(artist_url) {
    read_html(artist_url) |>
        html_element(xpath = '/html/body/div[2]/div/div/div/article/div[2]/p') |> 
        html_text()       
}

# create artist key

create_key <- function(col){
    key <- col |> 
        str_squish() |> 
        str_to_upper() |> 
        str_replace_all("[:space:]", "") |> 
        str_replace_all("[:punct:]", "") |> 
        str_conv("UTF8")
}

# spotify helper functions ----

# get spotify token
token <- function(){
    # get barer token
    res <- POST(
        "https://accounts.spotify.com/api/token",
        config = authenticate(user = client_id,
                              password = client_secret),
        body = list(grant_type = "client_credentials"),
        encode = "form"
    )
    
    return(content(res)$access_token)
}

# get spotify details
get_spotify_details <- function(playlist_id = NA, url_string = NA){
    Sys.sleep(0.1)
    # get data from endpoint
    response <- GET(
        if_else(
            is.character(playlist_id),
            str_glue("https://api.spotify.com/v1/playlists/{playlist_id}/tracks?limit=100"),
            url_string),
        config = add_headers(Authorization = paste0("Bearer ", token()))
    )
    # extract response to a list
    content(response)
}

# scrape great escape lineup ----

## first artist ----

# create df with first artist
artist1 <- "https://greatescapefestival.com/artists/76/"

timetable_df <- tibble(
    ge_artist = artist_name(artist1),
    artist_from = artist_from(artist1),
    artist_blurb = artist_blurb(artist1),
    event_venues = event_venues(artist1),
    event_times = event_times(artist1),
    url = artist1
)

# run a loop getting artist info and then moving to the next page
# break when next artist comes back around to start, alphabetically

## remaining artists ----

x <- 1

while (TRUE) {
    
    # get the next artist URL by querying last row of table url column
    artist <- slice_tail(timetable_df, n = 1) |> 
        select(url) |> 
        as.character()
    
    next_artist <- get_next_artist(artist)
    
    # break if we've cycled round to the first artists
    if (next_artist == artist1) break
    
    # append rows to pre-defined cols
    timetable_df <- timetable_df |> add_row(
        ge_artist = artist_name(next_artist),
        artist_from = artist_from(next_artist),
        artist_blurb = artist_blurb(next_artist),
        event_venues = event_venues(next_artist),
        event_times = event_times(next_artist),
        url = next_artist
    )
    
    print(paste(x, ": ", artist))
    
    x <- x + 1
}

timetable <- timetable_df |>
    # make sure loop didn't duplicate artists
    distinct() |> 
    # create artist key - lower and remove spaces
    mutate(artist_key = create_key(ge_artist))

## parse event times ----

# create gigs table w/ venues and times in for given artist
# clean event timetable to get rows for each gig and format to date-time
gigs <- timetable |> 
    select(artist_key, event_venues, event_times) |> 
    mutate(event_venues = str_squish(event_venues)) |>
    # simultaneously separate two columns by the same delim
    separate_longer_delim(c(event_venues, event_times), ", ") |> 
    mutate(
        parsed_event_times = 
            case_when(
                str_detect(str_to_lower(event_times), "wednesday") ~
                    paste("14/05/2025", str_extract(event_times, "([0-9]|0[0-9]|1[0-9]):([0-5][0-9])([AaPp][Mm])")),
                str_detect(str_to_lower(event_times), "thursday") ~ 
                    paste("15/05/2025", str_extract(event_times, "([0-9]|0[0-9]|1[0-9]):([0-5][0-9])([AaPp][Mm])")),
                str_detect(str_to_lower(event_times), "friday") ~ 
                    paste("16/05/2025", str_extract(event_times, "([0-9]|0[0-9]|1[0-9]):([0-5][0-9])([AaPp][Mm])")),
                str_detect(str_to_lower(event_times), "saturday") ~ 
                    paste("17/05/2025", str_extract(event_times, "([0-9]|0[0-9]|1[0-9]):([0-5][0-9])([AaPp][Mm])")),
                event_times == "NA" ~ NA_character_
            ),
        parsed_event_times = lubridate::parse_date_time(parsed_event_times, orders = "dmy HMOp", tz = "Europe/London"),
        event_venues = if_else(event_venues == "NA", NA_character_, event_venues)
    )

## create dimensions ----

# create artists dimension
artists <- timetable |> 
    select(artist_key, ge_artist, artist_from, artist_blurb)

# create venues dimension
venues <- gigs |> 
    distinct(event_venues) |> 
    drop_na()

# add spotify details ----

# playlist IDs
ge_playlist_id <- "7pskSMBb1Hes8SEM01DGX4" # Great Escape 2025
saved_playlist_id <- "6txLMueNiknIvDCnJFV4np" # my Great Escape Bangers 2025

## get my saved songs ----
my_songs <- 
    get_spotify_details(playlist_id = saved_playlist_id)$items |> 
    map_df( function(i){
        tibble(
            track_name = i$track$name,
            track_id = i$track$id,
            artists = paste(map_chr(i$track$artists, pluck, "name"), collapse = ", "),
            artist_ids = paste(map_chr(i$track$artists, pluck, "id"), collapse = ", ")
        )
    }) |> 
    separate_longer_delim(c(artists, artist_ids), ", ") |> 
    mutate(artist_key = create_key(artists))

## get official GE playlist ----

### first batch ----

# initial 100 tracks
ge_res <- get_spotify_details(playlist_id = ge_playlist_id)

# coerce into data frame
ge_playlist <- 
    ge_res$items |> 
    map_df( function(i){
        tibble(
            added_at = i$added_at,
            track_name = i$track$name,
            track_id = i$track$id,
            track_popularity = i$track$popularity,
            artist_names = paste(map_chr(i$track$artists, pluck, "name"), collapse = ", "),
            artist_ids = paste(map_chr(i$track$artists, pluck, "id"), collapse = ", "),
            album_name = i$track$album$name
        )
    })

### remaining playlist batches ----

next_res <- ge_res$`next`

while (TRUE) {
    # if no more to batches, break loop
    if (is.null(next_res)) break
    
    # get the next batch of songs from playlist
    this_res <- get_spotify_details(url_string = next_res)
    
    # coerce into data frame
    temp_df <-
        this_res$items |> 
        map_df(function(i){
            tibble(
                added_at = i$added_at,
                track_name = i$track$name,
                track_id = i$track$id,
                track_popularity = i$track$popularity,
                artist_names = paste(map_chr(i$track$artists, pluck, "name"), collapse = ", "),
                artist_ids = paste(map_chr(i$track$artists, pluck, "id"), collapse = ", "),
                album_name = i$track$album$name
            )
        })
    # append this batch to existing data frame
    ge_playlist <- 
        ge_playlist |> 
        add_row(temp_df)
    
    # get next endpoint
    next_res <- this_res$`next`
    print(next_res)
    
}

# remove duplicates from playlist
ge_playlist <-
    ge_playlist |> 
    distinct(track_id, artist_ids,
             .keep_all = TRUE)

# save playlist track list
spotify_playlist <-
    ge_playlist |> 
    select(track_id, added_at, track_popularity)

## add spotify artist details ----

artist_details <- artists |> 
    mutate(artist_encode = utils::URLencode(ge_artist)) |> 
    pull(artist_encode) |> 
    map_df(.progress = "getting artist details", function(i){
        x <- get_spotify_details(
                url_string = str_glue("https://api.spotify.com/v1/search?q=", i, "&type=artist&limit=1")
                )$artists$items
        x <- tibble(artist_id = pluck(x, 1, "id"),
                    artist_name = pluck(x, 1, "name"),
                    popularity = pluck(x, 1, "popularity"),
                    followers = pluck(x, 1, "followers", "total"),
                    genres = if_else(length(pluck(x, 1, "genres")) == 0, 
                                     "None", 
                                     str_c(pluck(x, 1, "genres"), collapse = ", ")))
    }) |> 
    mutate(artist_key = create_key(artist_name)) |> 
    distinct() |> 
    right_join(artists,
               by = "artist_key")

## create artist dimensions ----

spotify_genres <- 
    artist_details |> 
    select(artist_id, genres) |> 
    separate_longer_delim(genres, ", ") |> 
    mutate(genres = genres |> 
               str_squish() |> 
               str_to_sentence())

artist_from <- artists |> 
    select(artist_key, artist_from) |> 
    separate_longer_delim(artist_from, "__") |> 
    mutate(artist_from = str_replace(artist_from, "([)}])", "")) |> 
    separate_wider_regex(artist_from, 
                         patterns = c(country = ".*", "[({]", region = ".*"),
                         too_few = "align_start") |> 
    mutate(country = str_squish(country))

# save files ----
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
