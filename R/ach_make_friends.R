#' Make Friends Achievement
#'
#' @description
#' Awards achievements to fighters who face the most unique opponents in a specific
#' weapon category during a given year.
#'
#' @details
#' This is a unique tier achievement awarded to fighters who face the highest number
#' of different opponents in each weapon category for a given year.
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
#' achievements <- ach_make_friends_weapon(tournament_data)
#' }
#'
#' @keywords internal
#' 
ach_make_friends_weapon <- function(data) {
  # Define tiers
  tiers <- tibble::tribble(
    ~achievement_tier, ~tier_id, ~achievement_name_template,                   ~achievement_description_template,                                                    ~achievement_icon,
    "Unique",           1,        "{tournament_weapon} Friendmaker {event_year}",   "You faced the most unique opponents ({unique_opponents}) in {tournament_weapon} in {event_year}!", "friends_unique_weapon.png"
  )
  
  # Filter out rows with missing event_year, tournament_weapon, or opponent_id
  data <- data %>%
    dplyr::filter(!is.na(event_year) & !is.na(tournament_weapon) & !is.na(opponent_id))
  
  # Calculate unique opponents per fighter, weapon, and year
  unique_opponents <- data %>%
    dplyr::group_by(event_year, tournament_weapon, fighter_id) %>%
    dplyr::summarize(unique_opponents = dplyr::n_distinct(opponent_id), .groups = "drop")
  
  # Determine the fighters with the most unique opponents per weapon and year (allowing ties)
  top_fighters <- unique_opponents %>%
    dplyr::group_by(event_year, tournament_weapon) %>%
    dplyr::filter(unique_opponents == max(unique_opponents, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      tier_id = 1,
      achieved = TRUE
    )
  
  # Handle case where no fighters have faced any opponents in a given year and weapon category
  if (nrow(top_fighters) == 0) {
    return(data.frame(
      fighter_id = integer(0), tier_id = integer(0), achieved = logical(0),
      percentile = numeric(0), achievement_tier = character(0),
      achievement_name = character(0), achievement_description = character(0),
      achievement_icon = character(0), stringsAsFactors = FALSE
    ))
  }
  
  # Total fighters per year and weapon for percentile calculation
  total_fighters_per_weapon_year <- data %>%
    dplyr::group_by(event_year, tournament_weapon) %>%
    dplyr::summarize(total_fighters = dplyr::n_distinct(fighter_id), .groups = "drop")
  
  # Calculate percentiles
  achievements <- top_fighters %>%
    dplyr::left_join(total_fighters_per_weapon_year, by = c("event_year", "tournament_weapon")) %>%
    dplyr::mutate(
      percentile = ifelse(total_fighters == 0, 0, (1 / total_fighters) * 100)  # Percentile calculation
    )
  
  # Join tier details and calculate dynamic descriptions
  achievements <- achievements %>%
    dplyr::left_join(tiers, by = "tier_id") %>%
    dplyr::mutate(
      achievement_name = stringr::str_replace(
        achievement_name_template,
        "\\{event_year\\}",as.character(event_year))%>%
        stringr::str_replace("\\{tournament_weapon\\}", tournament_weapon),
      achievement_description = stringr::str_replace(
        achievement_description_template,"\\{unique_opponents\\}", as.character(unique_opponents))%>%
        stringr::str_replace("\\{event_year\\}",as.character(event_year))%>%
                               stringr::str_replace("\\{tournament_weapon\\}", tournament_weapon)
    ) %>%
    dplyr::select(
      fighter_id, tier_id, achieved, percentile,
      achievement_tier, achievement_name,
      achievement_description, achievement_icon
    )
  
  return(achievements)
}