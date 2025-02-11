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
#' This dataset contains information about individual HEMA fights, including the fighters involved,
#' their performance metrics, and match outcomes.
#'
#' @format A data frame with the following columns:
#' \describe{
#'   \item{match_id}{Integer. Unique identifier for each match.}
#'   \item{event_id}{Integer. Identifier for the event in which the match took place.}
#'   \item{event_name}{Character. Name of the event.}
#'   \item{event_date}{Date. Date of the event.}
#'   \item{fighter_1}{Character. Name of the first fighter.}
#'   \item{fighter_1_result}{Character. Result of the match for the first fighter (e.g., "Win", "Loss").}
#'   \item{fighter_2}{Character. Name of the second fighter.}
#'   \item{fighter_2_result}{Character. Result of the match for the second fighter.}
#'   \item{fighter_id}{Integer. Identifier for the primary fighter associated with the match record.}
#'   \item{opponent_id}{Integer. Identifier for the opponent fighter.}
#'   \item{stage}{Character. Stage of the tournament.}
#'   \item{tournament_category}{Character. Category of the tournament.}
#'   \item{tournament_id}{Integer. Identifier for the tournament.}
#'   \item{tournament_name}{Character. Name of the tournament.}
#'   \item{tournament_note}{Character. Additional notes regarding the tournament.}
#'   \item{tournament_weapon}{Character. Weapon category used in the tournament.}
#'   \item{elo_1}{Numeric. Elo rating of fighter 1.}
#'   \item{elo_2}{Numeric. Elo rating of fighter 2.}
#'   \item{update_1}{Numeric. Updated metric 1 (please adjust the description as needed).}
#'   \item{update_2}{Numeric. Updated metric 2 (please adjust the description as needed).}
#'   \item{win_chance_1}{Numeric. Win chance for fighter 1 (expressed as a proportion or percentage).}
#'   \item{win_chance_2}{Numeric. Win chance for fighter 2 (expressed as a proportion or percentage).}
#' }
#'
#' @examples
#' data(hema_fights)
#' head(hema_fights)
"hema_fights"

#' HEMA Match Results Dataset
#'
#' This dataset contains detailed results for individual HEMA matches, including match outcomes,
#' fighter ratings, and tournament metadata.
#'
#' @format A data frame with the following columns:
#' \describe{
#'   \item{club_id}{Integer. Identifier for the fighter's club.}
#'   \item{debut_fight}{Logical. Indicates if this is the fighter's debut match.}
#'   \item{event_date}{Date. The date when the event took place.}
#'   \item{event_id}{Integer. Identifier for the associated event.}
#'   \item{event_name}{Character. Name of the event.}
#'   \item{fighter_elo}{Numeric. Elo rating of the fighter before the match.}
#'   \item{fighter_elo_gain}{Numeric. Change in the fighter's Elo rating after the match.}
#'   \item{fighter_id}{Integer. Identifier for the fighter.}
#'   \item{fighter_name}{Character. Name of the fighter.}
#'   \item{is_final}{Logical. TRUE if the match is a final stage match.}
#'   \item{match_id}{Integer. Unique identifier for the match.}
#'   \item{match_result_id}{Integer. Unique identifier for the match result record.}
#'   \item{opponent_elo}{Numeric. Elo rating of the opponent before the match.}
#'   \item{opponent_id}{Integer. Identifier for the opponent fighter.}
#'   \item{opponent_name}{Character. Name of the opponent fighter.}
#'   \item{result}{Character. Outcome of the match for the fighter (e.g., "Win" or "Loss").}
#'   \item{stage}{Character. Stage of the tournament (e.g., "Pool", "Quarterfinal", "Final").}
#'   \item{tournament_category}{Character. Category of the tournament.}
#'   \item{tournament_id}{Integer. Identifier for the tournament.}
#'   \item{tournament_name}{Character. Name of the tournament.}
#'   \item{tournament_note}{Character. Additional notes regarding the tournament.}
#'   \item{tournament_weapon}{Character. Weapon category used in the tournament.}
#'   \item{win_chance}{Numeric. Estimated probability (or chance) of the fighter winning the match.}
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

#' HEMA Rankings Dataset
#'
#' This dataset contains monthly ranking information for HEMA fighters,
#' including performance metrics and associated club information.
#'
#' @format A data frame with 70,053 rows and 8 columns:
#' \describe{
#'   \item{rank}{Numeric. The ranking position of the fighter.}
#'   \item{fighter_id}{Numeric. Unique identifier for the fighter.}
#'   \item{fighter_name}{Character. Name of the fighter.}
#'   \item{category}{Character. The competition category (e.g., "Longsword (Mixed & Men's, Steel)").}
#'   \item{month}{Character. The month for which the ranking applies (e.g., "January 2025").}
#'   \item{weighted_rating}{Numeric. The weighted rating score for the fighter.}
#'   \item{club}{Character. The club with which the fighter is associated.}
#'   \item{month_date}{Date. The reference date within the month (e.g., "2025-01-22").}
#' }
#'
#' @details
#' The dataset also includes a column specification attribute (`spec`) that details the expected
#' column types and records any parsing problems encountered when reading the data.
#'
#' @examples
#' str(hema_rankings)
"hema_rankings"
