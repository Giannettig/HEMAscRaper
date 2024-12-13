#' Showing Up Achievement
#'
#' @description
#' Internal function to award the "Showing Up is Part of the Success!" achievement to fighters who participated in a tournament in a given year.
#' - Unique (tier_id=1): Awarded to all fighters who participated in at least one tournament in a year.
#'
#' @param data A data frame with at least the following columns:
#' \describe{
#'   \item{fighter_id}{Identifier of the fighter}
#'   \item{event_year}{Year of the event}
#' }
#'
#' @return A data frame with columns:
#' \describe{
#'   \item{fighter_id}{ID of the fighter}
#'   \item{tier_id}{Numeric tier ID (1 for Unique)}
#'   \item{achieved}{Logical, TRUE if achieved}
#'   \item{percentile}{Proportion of fighters who achieved this tier in that year}
#'   \item{achievement_tier}{"Unique"}
#'   \item{achievement_name}{"Showing Up is Part of the Success! - <Year>"}
#'   \item{achievement_description}{Dynamic description mentioning the year}
#'   \item{achievement_icon}{"showing_up_unique.png"}
#' }
#'
#' @importFrom dplyr group_by summarize mutate ungroup
#' @importFrom tibble tribble
#' @importFrom stringr str_replace_all
#' @keywords internal
ach_showing_up <- function(data) {
  # Define tier details
  tier_details <- tibble::tribble(
    ~achievement_tier, ~tier_id, ~achievement_name_template,                       ~achievement_description_template,                       ~achievement_icon,
    "Unique",          1,        "Showing Up is Part of the Success! {event_year}", "You showed up to a tournament in {event_year}!",        "showing_up_unique.png"
  )
  
  # Calculate achievements per year
  yearly_achievements <- data %>%
    dplyr::group_by(event_year, fighter_id) %>%
    dplyr::summarize(tournaments_attended = dplyr::n_distinct(tournament_id), .groups = "drop") %>%
    dplyr::filter(tournaments_attended > 0) %>%
    dplyr::mutate(
      tier_id = 1,
      achieved = TRUE
    )
  
  # Total fighters per year for percentile calculation
  total_fighters <- data %>%
    dplyr::group_by(event_year) %>%
    dplyr::summarize(total_fighters_in_year = dplyr::n_distinct(fighter_id), .groups = "drop")
  
  # Join tier details and compute the achievement
  achievements <- yearly_achievements %>%
    dplyr::left_join(tier_details, by = "tier_id") %>%
    dplyr::left_join(total_fighters, by = "event_year") %>%
    dplyr::mutate(
      percentile = 1 / total_fighters_in_year,
      achievement_name = stringr::str_replace_all(achievement_name_template, "\\{event_year\\}", as.character(event_year)),
      achievement_description = stringr::str_replace_all(achievement_description_template, "\\{event_year\\}", as.character(event_year))
    ) %>%
    dplyr::select(
      fighter_id,
      tier_id,
      achieved,
      percentile,
      achievement_tier,
      achievement_name,
      achievement_description,
      achievement_icon
    )
  
  return(achievements)
}

