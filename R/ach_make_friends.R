#' It Is Good to Make Friends Achievement (Per Weapon)
#'
#' @description
#' Internal function to award the "It Is Good to Make Friends" achievement to fighters who faced the most unique opponents in a specific weapon category and year. Ties are allowed.
#'
#' @param data A data frame with at least the following columns:
#' \describe{
#'   \item{fighter_id}{Identifier of the fighter.}
#'   \item{event_year}{Year of the event.}
#'   \item{opponent_id}{Identifier of the opponent.}
#'   \item{tournament_weapon}{The weapon category of the tournament.}
#' }
#'
#' @return A data frame with columns:
#' \describe{
#'   \item{fighter_id}{ID of the fighter.}
#'   \item{tier_id}{Numeric tier ID.}
#'   \item{achieved}{Logical, TRUE if achieved.}
#'   \item{percentile}{Proportion of fighters who achieved this tier, relative to all fighters.}
#'   \item{achievement_tier}{Tier name (Unique).}
#'   \item{achievement_name}{Name of the achievement.}
#'   \item{achievement_description}{Description of the achievement.}
#'   \item{achievement_icon}{Filename of the achievement icon.}
#' }
#'
#' @keywords internal
ach_make_friends_weapon <- function(data) {
  # Define tiers
  tiers <- tibble::tribble(
    ~achievement_tier, ~tier_id, ~achievement_name,               ~achievement_description,                                                    ~achievement_icon,
    "Unique",           1,        "It Is Good to Make Friends! {tournament_weapon} - {event_year}",   "You faced the most unique opponents ({unique_opponents}) in {tournament_weapon} in {event_year}!", "friends_unique_weapon.png"
  )
  
  # Calculate unique opponents per fighter, weapon, and year
  unique_opponents <- data %>%
    dplyr::group_by(event_year, tournament_weapon, fighter_id) %>%
    dplyr::summarise(unique_opponents = n_distinct(opponent_id), .groups = "drop")
  
  # Determine the fighters with the most unique opponents per weapon and year (allowing ties)
  top_fighters <- unique_opponents %>%
    dplyr::group_by(event_year, tournament_weapon) %>%
    dplyr::filter(unique_opponents == max(unique_opponents)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      tier_id = 1,
      achieved = TRUE
    )
  
  # Total fighters in the dataset for percentile calculation
  total_fighters <- data %>% dplyr::distinct(fighter_id)%>%nrow()
  
  # Join tier details and calculate dynamic descriptions
  achievements <- top_fighters %>%
    dplyr::left_join(tiers, by = "tier_id") %>%
    dplyr::mutate(
      achievement_name = stringr::str_replace_all(achievement_name, "\\{event_year\\}", as.character(event_year))%>%stringr::str_replace_all("\\{tournament_weapon\\}", tournament_weapon),
      achievement_description = stringr::str_replace_all(
        achievement_description,
        "\\{unique_opponents\\}",
        as.character(unique_opponents)
      ) %>%
        stringr::str_replace_all("\\{event_year\\}", as.character(event_year)) %>%
        stringr::str_replace_all("\\{tournament_weapon\\}", tournament_weapon),
      percentile = dplyr::n() / total_fighters
    ) %>%
    dplyr::select(
      fighter_id, tier_id, achieved, percentile, 
      achievement_tier, achievement_name, 
      achievement_description, achievement_icon
    )
  
  return(achievements)
}

