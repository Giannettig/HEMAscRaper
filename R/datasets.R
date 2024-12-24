#' HEMA Datasets
#'
#' This file documents all datasets used in the HEMA package, including clubs, countries, events, fighters, fights, match results, and tournaments.
#'

#' HEMA Clubs Dataset
#'
#' This dataset contains information about historical European martial arts (HEMA) clubs, including their names and associated metadata.
#'
#' @format A data frame with the following columns:
#' \describe{
#'   \item{club_id}{Integer. Unique identifier for each club.}
#'   \item{club_name}{Character. Name of the club.}
#'   \item{club_country}{Character. The country where the club is located.}
#'   \item{club_state}{Character. State or region of the club.}
#'   \item{club_city}{Character. City of the club.}
#'   \item{club_members}{Integer. Number of members in the club.}
#'   \item{club_parent_id}{Integer. ID of the parent club, if applicable.}
#'   \item{club_url}{Character. Website URL of the club.}
#' }
#'
#' @examples
#' data(hema_clubs)
#' head(hema_clubs)
"hema_clubs"

#' HEMA Countries Dataset
#'
#' This dataset contains a list of countries associated with HEMA activities.
#' Note: the community were calculated using the generate community graph for Longsword with a travel threshold of 5 travels
#'
#' @format A data frame with the following columns:
#' \describe{
#'   \item{id}{Integer. Unique identifier for the country.}
#'   \item{name}{Character. Name of the country.}
#'   \item{region}{Character. Region of the country.}
#'   \item{sub_region}{Character. Sub-region of the country.}
#'   \item{population}{Int Active Longsword fencers in community}
#'   \item{community}{Factor Longsword community id}
#'   \item{community_label}{Character Community Label}
#' }
#'
#' @examples
#' data(hema_countries)
#' head(hema_countries)
"hema_countries"

#' HEMA Events Dataset
#'
#' This dataset contains details about HEMA-related events, including tournaments and other activities.
#'
#' @format A data frame with the following columns:
#' \describe{
#'   \item{event_id}{Integer. Unique identifier for each event.}
#'   \item{event_name}{Character. Name of the event.}
#'   \item{event_year}{Integer. Year of the event.}
#'   \item{event_date}{Date. Date of the event.}
#'   \item{event_city}{Character. City where the event took place.}
#'   \item{event_country}{Character. Country of the event.}
#'   \item{event_brand}{Character. Brand or organizer of the event.}
#'   \item{event_url}{Character. Website URL of the event.}
#' }
#'
#' @examples
#' data(hema_events)
#' head(hema_events)
"hema_events"

#' HEMA Fighters Dataset
#'
#' This dataset contains information about individual HEMA fighters, including their affiliations and metadata.
#'
#' @format A data frame with the following columns:
#' \describe{
#'   \item{fighter_id}{Integer. Unique identifier for each fighter.}
#'   \item{fighter_name}{Character. Name of the fighter.}
#'   \item{fighter_nationality}{Character. Nationality of the fighter.}
#'   \item{fighter_club_id}{Integer. Club ID associated with the fighter.}
#'   \item{fighter_club_name}{Character. Club name associated with the fighter.}
#'   \item{fighter_url}{Character. Profile URL of the fighter.}
#' }
#'
#' @examples
#' data(hema_fighters)
#' head(hema_fighters)
"hema_fighters"

#' HEMA Fights Dataset
#'
#' This dataset contains information about individual HEMA fights, including the fighters involved and outcomes.
#'
#' @format A data frame with the following columns:
#' \describe{
#'   \item{match_id}{Integer. Unique identifier for each match.}
#'   \item{event_id}{Integer. Identifier for the event in which the match took place.}
#'   \item{event_name}{Character. Name of the event.}
#'   \item{fighter_1}{Character. Name of the first fighter.}
#'   \item{fighter_1_result}{Character. Result of the fight for the first fighter (e.g., Win, Loss).}
#'   \item{fighter_2}{Character. Name of the second fighter.}
#'   \item{fighter_2_result}{Character. Result of the fight for the second fighter.}
#'   \item{fighter_id}{Integer. ID of the fighter.}
#'   \item{opponent_id}{Integer. ID of the opponent.}
#'   \item{stage}{Character. Stage of the tournament.}
#'   \item{tournament_category}{Character. Category of the tournament.}
#'   \item{tournament_id}{Integer. ID of the tournament.}
#'   \item{tournament_name}{Character. Name of the tournament.}
#'   \item{tournament_note}{Character. Notes about the tournament.}
#'   \item{tournament_weapon}{Character. Weapon category used in the tournament.}
#' }
#'
#' @examples
#' data(hema_fights)
#' head(hema_fights)
"hema_fights"

#' HEMA Match Results Dataset
#'
#' This dataset contains detailed results for individual HEMA matches, including match outcomes and metadata.
#'
#' @format A data frame with the following columns:
#' \describe{
#'   \item{club_id}{Integer. Identifier for the fighter's club.}
#'   \item{debut_fight}{Logical. Indicates if this is the fighter's debut match.}
#'   \item{event_id}{Integer. Identifier for the associated event.}
#'   \item{event_name}{Character. Name of the event.}
#'   \item{fighter_id}{Integer. Identifier for the fighter.}
#'   \item{fighter_name}{Character. Name of the fighter.}
#'   \item{match_id}{Integer. Unique identifier for each match.}
#'   \item{opponent_id}{Integer. Identifier for the opponent.}
#'   \item{opponent_name}{Character. Name of the opponent.}
#'   \item{result}{Character. Outcome of the match for the fighter (e.g., Win, Loss).}
#'   \item{stage}{Character. Stage of the tournament.}
#'   \item{tournament_category}{Character. Category of the tournament.}
#'   \item{tournament_id}{Integer. Identifier for the tournament.}
#'   \item{tournament_name}{Character. Name of the tournament.}
#'   \item{tournament_note}{Character. Notes about the tournament.}
#'   \item{tournament_weapon}{Character. Weapon category used in the tournament.}
#' }
#'
#' @examples
#' data(hema_match_results)
#' head(hema_match_results)
"hema_match_results"

#' HEMA Tournaments Dataset
#'
#' This dataset contains aggregated information about HEMA tournaments, including participant counts and match statistics.
#'
#' @format A data frame with the following columns:
#' \describe{
#'   \item{tournament_id}{Integer. Unique identifier for each tournament.}
#'   \item{tournament_name}{Character. Name of the tournament.}
#'   \item{event_id}{Integer. Identifier for the associated event.}
#'   \item{tournament_category}{Character. Category of the tournament.}
#'   \item{tournament_note}{Character. Notes about the tournament.}
#'   \item{tournament_weapon}{Character. Weapon category used in the tournament.}
#'   \item{match_count}{Integer. Total number of matches in the tournament.}
#'   \item{fighter_count}{Integer. Total number of fighters in the tournament.}
#' }
#'
#' @examples
#' data(hema_tournaments)
#' head(hema_tournaments)
"hema_tournaments"


#' HEMA Schievement Test Data
#'
#' This dataset contains aggregated information about HEMA tournaments, including participant counts and match statistics.
#'
#'
#' @examples
#' data(test_data)
#' head(test_data)
"test_data"