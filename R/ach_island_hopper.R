#' Island Hopper Achievement
#'
#' @description
#' Internal function to award the "Island Hopper" achievement to fighters who fought in a country outside their HEMA scene.
#' - Unique (tier_id=1): Awarded for participating in a tournament in a country where the club_community is different from the event_community.
#'
#' @param data A data frame with at least the following columns:
#' \describe{
#'   \item{fighter_id}{Identifier of the fighter}
#'   \item{club_community}{Community of the fighter's club}
#'   \item{event_community}{Community where the event was held}
#' }
#'
#' @return A data frame with columns:
#' \describe{
#'   \item{fighter_id}{ID of the fighter}
#'   \item{tier_id}{Numeric tier ID (1 for Unique)}
#'   \item{achieved}{Logical, TRUE if achieved}
#'   \item{percentile}{Proportion of fighters who achieved this tier}
#'   \item{achievement_tier}{"Unique"}
#'   \item{achievement_name}{"Island Hopper"}
#'   \item{achievement_description}{"You have fought in a country outside your HEMA Scene!"}
#'   \item{achievement_icon}{"island_hopper_unique.png"}
#' }
#'
#' @importFrom dplyr filter mutate group_by summarize distinct select left_join
#' @importFrom tibble tribble
#' @keywords internal
ach_island_hopper <- function(data) {
  # Define tier details
  tiers <- tibble::tribble(
    ~achievement_tier, ~tier_id, ~achievement_name,       ~achievement_description,                                   ~achievement_icon,
    "Unique",          1,        "Island Hopper",        "You have fought in a country outside your HEMA Scene!",    "island_hopper_unique.png"
  )
  
  # Filter for fighters who participated in events outside their HEMA scene
  eligible_fighters <- data %>%
    dplyr::filter(
      !is.na(club_community) & !is.na(event_community) & club_community != event_community
    ) %>%
    dplyr::distinct(fighter_id) %>%
    dplyr::mutate(
      tier_id = 1,
      achieved = TRUE
    )
  
  # Calculate percentile
  total_fighters <- data %>%
    dplyr::distinct(fighter_id) %>%
    nrow()
  
  achievements <- eligible_fighters %>%
    dplyr::mutate(percentile = n() / total_fighters) %>%
    dplyr::left_join(tiers, by = "tier_id") %>%
    dplyr::select(
      fighter_id, tier_id, achieved, percentile, 
      achievement_tier, achievement_name, 
      achievement_description, achievement_icon
    )
  
  return(achievements)
}