#' Hero of the Nation Achievement
#'
#' @description
#' Awards achievements to fighters who win the most fights in their home country
#' within a given year.
#'
#' @details
#' This is a unique tier achievement awarded to fighters who achieve the highest
#' number of wins in their respective countries for a given year.
#'
#' @param data A data frame containing HEMA tournament match data
#'
#' @return A data frame of achievements with columns:
#' \itemize{
#'   \item fighter_id: Unique fighter identifier
#'   \item tier_id: Achievement tier level (always 1)
#'   \item achieved: Logical indicating if achievement earned
#'   \item percentile: Fighter's percentile for this achievement
#'   \item achievement_tier: Text description of tier (always "Unique")
#'   \item achievement_name: Name of the achievement
#'   \item achievement_description: Description of what was achieved
#'   \item achievement_icon: Icon file name for the achievement
#' }
#'
#' @examples
#' \dontrun{
#' achievements <- ach_hero_of_the_nation(tournament_data)
#' }
#'
#' @keywords internal
ach_hero_of_the_nation <- function(data) {
  # Define tiers
  tiers <- tibble::tribble(
    ~achievement_tier, ~tier_id, ~achievement_name,              ~achievement_description_template,                                                           ~achievement_icon,
    "Unique",           1,        "Every Nation Needs a Hero {event_year}",   "You won the most fights ({total_wins} wins) in your country ({club_country}) in {event_year}!", "hero_nation_unique.png"
  )
  
  # Ensure required columns are present
  required_cols <- c("event_year", "club_country", "fighter_id", "result")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  
  # Filter out rows with missing event_year or club_country
  data <- data %>% dplyr::filter(!is.na(event_year) & !is.na(club_country))
  
  # Calculate wins by fighter, year, and country
  wins_by_country <- data %>%
    dplyr::filter(result == "WIN") %>%
    dplyr::group_by(event_year, club_country, fighter_id) %>%
    dplyr::summarize(total_wins = n(), .groups = "drop")
  
  # Determine the fighter(s) with the most wins for each country and year
  top_winners <- wins_by_country %>%
    dplyr::group_by(event_year, club_country) %>%
    dplyr::filter(total_wins == max(total_wins)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      tier_id = 1,
      achieved = TRUE
    )
  
  # Handle case where no fighters meet the criteria
  if (nrow(top_winners) == 0) {
    return(data.frame(
      fighter_id = integer(0), tier_id = integer(0), achieved = logical(0),
      percentile = numeric(0), achievement_tier = character(0),
      achievement_name = character(0), achievement_description = character(0),
      achievement_icon = character(0), stringsAsFactors = FALSE
    ))
  }
  
  # Total fighters per year for percentile calculation
  total_fighters <- data %>%
    dplyr::group_by(event_year) %>%
    dplyr::summarize(total_fighters_in_year = n_distinct(fighter_id), .groups = "drop")
  
  # Calculate cumulative counts for percentiles
  tier_counts <- top_winners %>%
    dplyr::group_by(event_year) %>%
    dplyr::summarize(tier_count = n(), .groups = "drop") %>%
    dplyr::left_join(total_fighters, by = "event_year") %>%
    dplyr::mutate(
      percentile = ifelse(total_fighters_in_year == 0, 0, (1 - (tier_count / total_fighters_in_year))*100)  # Handle division by zero
    )
  
  # Join tier details and calculate dynamic descriptions
  achievements <- top_winners %>%
    dplyr::left_join(tiers, by = "tier_id") %>%
    dplyr::left_join(total_fighters, by = "event_year") %>%
    dplyr::left_join(tier_counts, by = "event_year") %>%
    dplyr::mutate(
      achievement_name = stringr::str_replace(
        achievement_name,"\\{event_year\\}", as.character(event_year)
      ),
      achievement_description = stringr::str_replace(
        achievement_description_template,"\\{club_country\\}", club_country)%>%
        stringr::str_replace("\\{event_year\\}",as.character(event_year))%>%
        stringr::str_replace("\\{total_wins\\}", as.character(total_wins))
    
    ) %>%
    dplyr::select(
      fighter_id, tier_id, achieved, percentile, 
      achievement_tier, achievement_name, 
      achievement_description, achievement_icon
    )
  
  return(achievements)
}