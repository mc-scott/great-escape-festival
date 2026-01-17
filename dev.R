# Utility script to inspect all tables in db.sqlite

library(DBI)
library(RSQLite)
library(purrr)
library(dplyr)

# Connect to database
con <- dbConnect(RSQLite::SQLite(), "db.sqlite")

# Load all tables into the global environment
dbListTables(con) |>
    set_names() |>
    walk(~assign(.x, dbReadTable(con, .x), envir = globalenv()))

tables_meta <-
    dbListTables(con) |>
    map_df(
        ~ tibble(
            table_name = .,
            column_name = names(get(.)),
            data_type = purrr::map_chr(get(.), class)
        )
    )

message("Loaded tables: ", paste(dbListTables(dbConnect(RSQLite::SQLite(), "db.sqlite")), collapse = ", "))

# Disconnect
dbDisconnect(con)