#' Island Breaker Achievement
#'
#' @description
#' Internal function to award the "Island Breaker" achievement to fighters who fought in different HEMA scenes in a given year.
#' - Bronze: Participated in 2 scenes.
#' - Silver: Participated in 3 scenes.
#' - Gold: Participated in 4 scenes.
#' - Epic: Participated in 5 or more scenes.
#'
#' @param data A data frame with at least the following columns:
#' \describe{
#'   \item{fighter_id}{Identifier of the fighter.}
#'   \item{event_year}{Year of the event.}
#'   \item{event_community}{Community where the event was held.}
#' }
#'
#' @return A data frame with columns:
#' \describe{
#'   \item{fighter_id}{ID of the fighter.}
#'   \item{tier_id}{Numeric tier ID.}
#'   \item{achieved}{Logical, TRUE if achieved.}
#'   \item{percentile}{Proportion of fighters who achieved this tier.}
#'   \item{achievement_tier}{Tier name (Bronze, Silver, Gold, Epic).}
#'   \item{achievement_name}{Name of the achievement.}
#'   \item{achievement_description}{Description of the achievement.}
#'   \item{achievement_icon}{Filename of the achievement icon.}
#' }
#'
#' @keywords internal
ach_island_breaker <- function(data) {
  # Define tiers
  tiers <- tibble::tribble(
    ~achievement_tier, ~tier_id, ~achievement_name,         ~achievement_description,                                              ~achievement_icon,
    "Bronze",           1,        "Island Breaker {event_year}",         "You fought in 2 different HEMA scenes in {event_year}!",              "island_breaker_bronze.png",
    "Silver",           2,        "Island Breaker {event_year}",         "You fought in 3 different HEMA scenes in {event_year}!",              "island_breaker_silver.png",
    "Gold",             3,        "Island Breaker {event_year}",         "You fought in 4 different HEMA scenes in {event_year}!",              "island_breaker_gold.png",
    "Epic",             4,        "Island Breaker {event_year}",         "You fought in 5 or more different HEMA scenes in {event_year}!",      "island_breaker_epic.png"
  )
  
  # Calculate the number of scenes each fighter participated in per year
  yearly_scenes <- data %>%
    dplyr::filter(!is.na(event_community)) %>%
    dplyr::group_by(event_year, fighter_id) %>%
    dplyr::summarize(
      distinct_scenes = n_distinct(event_community),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      tier_id = dplyr::case_when(
        distinct_scenes >= 5 ~ 4,
        distinct_scenes >= 4 ~ 3,
        distinct_scenes >= 3 ~ 2,
        distinct_scenes >= 2 ~ 1,
        TRUE                 ~ NA_integer_
      )
    ) %>%
    dplyr::filter(!is.na(tier_id))  # Only keep fighters who achieved a tier
  
  # Calculate percentile
  total_fighters <- data %>%
    dplyr::group_by(event_year) %>%
    dplyr::summarize(total_fighters_in_year = n_distinct(fighter_id), .groups = "drop")
  
  # Join tier details and calculate dynamic descriptions
  achievements <- yearly_scenes %>%
    dplyr::left_join(tiers, by = "tier_id") %>%
    dplyr::left_join(total_fighters, by = "event_year") %>%
    dplyr::mutate(
      achievement_name = stringr::str_replace_all(achievement_name, "\\{event_year\\}", as.character(event_year)),
      achievement_description = stringr::str_replace_all(
        achievement_description,
        "\\{event_year\\}",
        as.character(event_year)
      ),
      achieved = TRUE,
      percentile = n() / total_fighters_in_year
    ) %>%
    dplyr::select(
      fighter_id, tier_id, achieved, percentile, 
      achievement_tier, achievement_name, 
      achievement_description, achievement_icon
    )
  
  return(achievements)
}

