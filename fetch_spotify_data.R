# fetch_spotify_data.R
# Fetches and enriches artist data from Spotify API
# Returns: artist_details, spotify_genres, artist_from, spotify_playlist, my_songs

source("utils.R")

fetch_spotify_data <- function(artists, client_id, client_secret) {

    message("Starting Spotify API data fetch...")

    # playlist IDs
    ge_playlist_id <- "7pskSMBb1Hes8SEM01DGX4" # Great Escape 2025
    saved_playlist_id <- "6txLMueNiknIvDCnJFV4np" # my Great Escape Bangers 2025

    # get my saved songs ----
    message("Fetching saved songs...")

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

    # get official GE playlist ----
    message("Fetching Great Escape playlist...")

    ## first batch ----

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

    ## remaining playlist batches ----

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

    # add spotify artist details ----
    message("Fetching artist details from Spotify...")

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

    # create artist dimensions ----

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

    message("Spotify data fetch complete.")

    # return all data objects
    list(
        artist_details = artist_details,
        spotify_genres = spotify_genres,
        artist_from = artist_from,
        spotify_playlist = spotify_playlist,
        my_songs = my_songs
    )
}
