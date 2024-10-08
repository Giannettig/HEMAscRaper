README for HEMAscRaper
================
Giuliano Giannetti
2024-10-02

# HEMAscRaper

`HEMAscRaper` is an R package designed to scrape and analyze data from
the Historical European Martial Arts (HEMA) Ratings website. This README
provides a step-by-step guide to using the package to download data on
fighters, clubs, events, and tournaments, and to save the resulting
datasets for further analysis.

## Installation

To install the `HEMAscRaper` package, you can use the `devtools`
package:

``` r
# Uncomment the line below to install the development tools package if not already installed
# install.packages("devtools")

devtools::install_github("Giannettig/HEMAscRaper")
```

## Usage

### 1. Load the Package

``` r
library(HEMAscRaper)
library(dplyr)  # For data manipulation
```

Use the `data()` function to load binary data associated with the
package

``` r
data(hema_clubs)
data(hema_fights)
data(hema_fighters)
data(hema_events)
```

### 2. Download Fighter Data

The `get_fighters()` function scrapes the HEMA Ratings fighters page and
returns a structured tibble of all registered fighters.

``` r
# Download fighter data
hema_fighters <- get_fighters()

# Preview the first few rows of the data
head(hema_fighters)

# Save the fighters data as a CSV file
write.csv(hema_fighters, "fighters_data.csv", row.names = FALSE)
```

### 3. Download Club Data

The `get_clubs()` function scrapes the HEMA Ratings clubs page and
returns a structured tibble with club information.

``` r
# Download club data
hema_clubs <- get_clubs()

# Preview the first few rows of the club data
head(hema_clubs)

# Save the clubs data as a CSV file
write.csv(hema_clubs, "clubs_data.csv", row.names = FALSE)
```

### 4. Download Event Data

The `get_events()` function scrapes the HEMA Ratings events page and
extracts detailed information about HEMA events, including dates,
countries, and cities.

``` r
# Download event data
hema_events <- get_events()

# Preview the first few rows of the event data
head(hema_events)

# Save the events data as a CSV file
write.csv(hema_events, "events_data.csv", row.names = FALSE)
```

### 5. Download Tournament Data for a Specific Event

The `get_tournament()` function takes a URL to a specific tournament and
extracts detailed fight-level information, including tournament names,
disciplines, fighters, and match results.

``` r
# Example tournament URL
tournament_url <- "https://hemaratings.com/tournaments/details/1234/"

# Download tournament data
tournament_data <- get_tournament(tournament_url)

# Preview the first few rows of the tournament data
head(tournament_data)

# Save the tournament data as a CSV file
write.csv(tournament_data, "tournament_data.csv", row.names = FALSE)
```

### 6. Download Fight Data for Specific Years

The `get_fights()` function scrapes the HEMA Ratings events page for
fight statistics across multiple tournaments, allowing you to filter by
specific years. If no year is provided, all available fight data is
extracted.

``` r
# Download fight data for the year 2022
fights_2022 <- get_fights(2022)

# Preview the first few rows of the fight data
head(fights_2022)

# Save the fight data for 2022 as a CSV file
write.csv(fights_2022, "fights_2022_data.csv", row.names = FALSE)

# Download fight data for multiple years (e.g., 2019 and 2020)
fights_2019_2020 <- get_fights(c(2019, 2020))

# Save the fight data for 2019 and 2020 as a CSV file
write.csv(fights_2019_2020, "fights_2019_2020_data.csv", row.names = FALSE)

# Download fight data for all available years
hema_fights <- get_fights()

# Save all fight data as a CSV file
write.csv(hema_fights, "all_fights_data.csv", row.names = FALSE)
```
