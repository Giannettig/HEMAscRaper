% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/HEMAscrapers.R
\name{get_tournament}
\alias{get_tournament}
\title{Download and Extract HEMA Tournament Data}
\usage{
get_tournament(tournament_url)
}
\arguments{
\item{tournament_url}{A character string specifying the URL of the tournament page on the HEMA Ratings website.
This URL should link directly to a specific tournament event.}
}
\value{
A tibble containing detailed tournament data with the following columns:
\describe{
  \item{\code{event_id}}{Unique identifier of the event. Integer.}
  \item{\code{event_name}}{Name of the event. Character.}
  \item{\code{tournament_name}}{Name of the specific tournament or discipline within the event. Character.}
  \item{\code{event_category}}{Category of the tournament (e.g., Mixed, Men's, Women's, Underrepresented Genders). Character.}
  \item{\code{event_note}}{Additional notes about the tournament (e.g., special rules or conditions), if available. Character.}
  \item{\code{event_weapon}}{Weapon used in the tournament (e.g., Longsword, Rapier). Character.}
  \item{\code{fighter_id}}{Unique identifier of the first fighter in a match. Integer.}
  \item{\code{opponent_id}}{Unique identifier of the opponent fighter in a match. Integer.}
  \item{\code{fighter_score}}{Score of the first fighter. Character.}
  \item{\code{opponent_score}}{Score of the opponent fighter. Character.}
}
}
\description{
This function downloads data from a specified HEMA tournament page on the *HEMA Ratings* website and extracts detailed fight statistics,
including tournament names, disciplines, fighters, and fight results.
}
\details{
This function scrapes the specified tournament page and extracts match-level information for all the fights listed.
It organizes the data into a tidy format, suitable for further analysis or visualization. If the page layout changes, adjustments to the scraping logic may be necessary.

The function also attempts to handle anonymized fighters by assigning them a placeholder ID.
}
\note{
This function relies on the `rvest` and `dplyr` packages for web scraping and data manipulation.
}
\examples{
\dontrun{
# Download HEMA tournament data for a specific event and display the first few rows
tournament_data <- get_tournament("https://hemaratings.com/tournament/details/1234/")
head(tournament_data)
}

}
