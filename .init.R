# Load required libraries
library(readr)
library(dplyr)
library(usethis)
library(purrr)

# Set the directory where the files are located
hema_ratings_folder <- "hema_ratings"

# Get a list of all CSV files prefixed with 'hema_'
hema_files <- list.files(
  path = hema_ratings_folder,
  pattern = "^hema_.*\\.csv$", # Regex to match 'hema_' prefix and .csv extension
  full.names = TRUE
)

# Loop through the files, read them, and save with proper names
walk(hema_files, function(file_path) {
  # Extract the base name of the file without extension
  dataframe_name <- gsub("\\.csv$", "", basename(file_path)) # Remove .csv extension
  
  # Read the CSV into a dataframe
  dataframe <- read_csv(file_path,show_col_types = FALSE)
})

usethis::use_data(hema_achievements, overwrite = TRUE)
usethis::use_data(hema_clubs, overwrite = TRUE)
usethis::use_data(hema_events, overwrite = TRUE)
usethis::use_data(hema_fighters, overwrite = TRUE)
usethis::use_data(hema_fights, overwrite = TRUE)
usethis::use_data(hema_match_results, overwrite = TRUE)
usethis::use_data(hema_tournaments, overwrite = TRUE)


# Use the data frame for `use_data`

