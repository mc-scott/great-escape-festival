library(rvest)
library(httr)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)

# web scraping helper functions ----

safe_read_html <- safely(read_html, otherwise = minimal_html("None"))

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