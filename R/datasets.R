#' @title HEMA Datasets
#'
#' @description
#' Internal documentation for all datasets used in the HEMA package, including clubs,
#' countries, events, fighters, fights, match results, and tournaments.
#'
#' @keywords internal

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
#'   \item{country_id}{Integer. Unique identifier for the country.}
#'   \item{name}{Character. Name of the country.}
#'   \item{region}{Character. Region of the country.}
#'   \item{sub_region}{Character. Sub-region of the country.}
#'   \item{population}{Int Active Longsword fencers in community}
#'   \item{community}{Factor Longsword community id}
#'   \item{community_label}{Character Community Label}
#'   \item{country_code}{Character Alpha-2 ISO code}
#'   
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
#'   \item{is_final}{Character. True if the stage is a final or gold match}
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

#' @title HEMA Fighter Rankings Dataset
#' @description A dataset containing rankings for historical European martial arts (HEMA) fighters, including their weighted ratings, club affiliations, and metadata such as competition categories and dates.
#' @format A tibble with the following columns:
#' \describe{
#'   \item{rank}{An integer representing the fighter's rank in the competition.}
#'   \item{fighter_id}{A numeric identifier uniquely assigned to each fighter.}
#'   \item{fighter_name}{A character string indicating the name of the fighter.}
#'   \item{category}{A character string describing the competition category (e.g., "Longsword (Mixed & Men's, Steel)").}
#'   \item{month}{A character string representing the ranking period, such as "December 2024".}
#'   \item{weighted_rating}{A numeric value representing the weighted rating of the fighter based on their performance.}
#'   \item{club}{A character string indicating the club or academy the fighter represents.}
#'   \item{month_date}{A date object representing the exact date of the ranking period (e.g., "2024-12-28").}
#' }
#' @details This dataset provides detailed rankings of fighters participating in HEMA competitions. It includes their rank, weighted rating, associated club, and metadata about the competition's category and date. This dataset is particularly useful for analyzing fighter performance and tracking historical rankings across different periods.
#' @examples
#' \dontrun{
#'   # View the first few rows of the dataset
#'   head(hema_rankings)
#'
#'   # Filter fighters ranked in the top 5
#'   dplyr::filter(hema_rankings, rank <= 5)
#'
#'   # Summarize the average weighted rating by category
#'   dplyr::group_by(hema_rankings, category) %>%
#'     dplyr::summarize(avg_rating = mean(weighted_rating, na.rm = TRUE))
#' }
"hema_rankings"

#' @title HEMA Achievements Dataset
#' @description A dataset containing detailed information on HEMA fighters' achievements, including tier levels, percentiles, and dynamic descriptions of their accomplishments.
#' @format A tibble with 8 columns:
#' \describe{
#'   \item{fighter_id}{A numeric identifier uniquely assigned to each fighter.}
#'   \item{tier_id}{A numeric identifier representing the tier of the achievement (e.g., Bronze = 1, Silver = 2, Gold = 3, Epic = 4).}
#'   \item{achieved}{A logical value indicating whether the achievement was attained (`TRUE`) or not (`FALSE`).}
#'   \item{percentile}{A numeric value representing the fighter's percentile rank for the achievement, expressed as a percentage.}
#'   \item{achievement_tier}{A character string indicating the tier of the achievement (e.g., "Bronze", "Silver", "Gold", "Epic").}
#'   \item{achievement_name}{A character string providing the name of the achievement (e.g., "The Pilgrim 2012").}
#'   \item{achievement_description}{A character string describing the achievement dynamically (e.g., "You fought in the most countries (3) in 2012!").}
#'   \item{achievement_icon}{A character string indicating the filename of the icon associated with the achievement (e.g., "pilgrim_epic.png").}
#' }
#' @details This dataset is generated as part of the HEMA Analyzer system and provides detailed insights into the achievements of fighters in various competitions. Achievements are dynamically generated based on specific criteria such as participation in tournaments, victories, and diversity of competition regions.
"hema_achievements"