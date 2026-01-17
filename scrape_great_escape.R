# scrape_great_escape.R
# Scrapes festival line-up data from greatescapefestival.com
# Returns: timetable, gigs, artists, venues dataframes

source("utils.R")

scrape_great_escape <- function() {

    message("Starting Great Escape website scrape...")

    # first artist ----

    # create df with first artist
    artist1_url <- "https://greatescapefestival.com/artists/aint/"

    # download HTML once and extract all data from the parsed document
    artist1_doc <- download_artist_page(artist1_url)

    timetable_df <- tibble(
        ge_artist = artist_name(artist1_doc),
        artist_genre = artist_genre(artist1_doc),
        artist_from = artist_from(artist1_doc),
        artist_blurb = artist_blurb(artist1_doc),
        num_events = num_events(artist1_doc),
        event_venues = event_venues(artist1_doc),
        event_times = event_times(artist1_doc),
        url = artist1_url,
        next_url = get_next_artist(artist1_doc)
    )

    # run a loop getting artist info and then moving to the next page
    # break when next artist comes back around to start, alphabetically

    # remaining artists ----

    x <- 1

    while (TRUE) {

        # get the next artist URL from the last row
        next_artist_url <- slice_tail(timetable_df, n = 1) |>
            pull(next_url)

        # break if we've cycled round to the first artist
        if (next_artist_url == artist1_url) break

        # download HTML once and extract all data from the parsed document
        next_artist_doc <- download_artist_page(next_artist_url)

        # append rows to pre-defined cols
        timetable_df <- timetable_df |> add_row(
            ge_artist = artist_name(next_artist_doc),
            artist_genre = artist_genre(next_artist_doc),
            artist_from = artist_from(next_artist_doc),
            artist_blurb = artist_blurb(next_artist_doc),
            num_events = num_events(next_artist_doc),
            event_venues = event_venues(next_artist_doc),
            event_times = event_times(next_artist_doc),
            url = next_artist_url,
            next_url = get_next_artist(next_artist_doc)
        )

        print(paste(x, ": ", next_artist_url))

        x <- x + 1
    }

    timetable <- timetable_df |>
        # make sure loop didn't duplicate artists
        distinct() |>
        # create artist key - lower and remove spaces
        mutate(artist_key = create_key(ge_artist))

    # parse event times ----

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

    # create dimensions ----

    # create artists dimension
    artists <- timetable |>
        select(artist_key, ge_artist, artist_genre, artist_from, artist_blurb)

    # create venues dimension
    venues <- gigs |>
        distinct(event_venues) |>
        drop_na()

    message("Great Escape scrape complete. Artists found: ", nrow(artists))

    # return all data objects
    list(
        timetable = timetable,
        gigs = gigs,
        artists = artists,
        venues = venues
    )
}
