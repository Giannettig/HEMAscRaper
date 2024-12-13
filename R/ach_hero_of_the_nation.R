#' Every Nation Needs a Hero Achievement
#'
#' @description
#' Internal function to award the "Every Nation Needs a Hero" achievement to fighters who won the most fights in their country (by `club_country`) in a given year.
#'
#' @param data A data frame with at least the following columns:
#' \describe{
#'   \item{fighter_id}{Identifier of the fighter.}
#'   \item{event_year}{Year of the event.}
#'   \item{club_country}{Country of the fighter's club.}
#'   \item{result}{Result of the fight (e.g., "WIN").}
#' }
#'
#' @return A data frame with columns:
#' \describe{
#'   \item{fighter_id}{ID of the fighter.}
#'   \item{tier_id}{Numeric tier ID.}
#'   \item{achieved}{Logical, TRUE if achieved.}
#'   \item{percentile}{Proportion of fighters who achieved this tier.}
#'   \item{achievement_tier}{Tier name (Unique).}
#'   \item{achievement_name}{Name of the achievement.}
#'   \item{achievement_description}{Description of the achievement.}
#'   \item{achievement_icon}{Filename of the achievement icon.}
#' }
#'
#' @keywords internal
ach_hero_of_the_nation <- function(data) {
  # Define tiers
  tiers <- tibble::tribble(
    ~achievement_tier, ~tier_id, ~achievement_name,              ~achievement_description,                                                           ~achievement_icon,
    "Unique",           1,        "Every Nation Needs a Hero",   "You won the most fights ({total_wins} wins) in your country ({club_country}) in {event_year}!", "hero_nation_unique.png"
  )
  
  # Calculate wins by fighter, year, and country
  wins_by_country <- data %>%
    dplyr::filter(result == "WIN") %>%
    dplyr::group_by(event_year, club_country, fighter_id) %>%
    dplyr::summarise(total_wins = n(), .groups = "drop")
  
  # Determine the fighter with the most wins for each country and year
  top_winners <- wins_by_country %>%
    dplyr::group_by(event_year, club_country) %>%
    dplyr::filter(total_wins == max(total_wins)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      tier_id = 1,
      achieved = TRUE
    )
  
  # Total fighters per year for percentile calculation
  total_fighters <- data %>%
    dplyr::group_by(event_year) %>%
    dplyr::summarise(total_fighters_in_year = n_distinct(fighter_id), .groups = "drop")
  
  # Join tier details and calculate dynamic descriptions
  achievements <- top_winners %>%
    dplyr::left_join(tiers, by = "tier_id") %>%
    dplyr::left_join(total_fighters, by = "event_year") %>%
    dplyr::mutate(
      achievement_name = stringr::str_replace_all(achievement_name, "\\{club_country\\}", club_country),
      achievement_description = stringr::str_replace_all(
        achievement_description,
        "\\{club_country\\}",
        club_country
      ) %>%
        stringr::str_replace_all("\\{event_year\\}", as.character(event_year)) %>%
        stringr::str_replace_all("\\{total_wins\\}", as.character(total_wins)),
      percentile = n() / total_fighters_in_year
    ) %>%
    dplyr::select(
      fighter_id, tier_id, achieved, percentile, 
      achievement_tier, achievement_name, 
      achievement_description, achievement_icon
    )
  
  return(achievements)
}
