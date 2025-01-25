#' Showing Up Achievement
#'
#' @description
#' Awards achievements to fighters for participating in tournaments during a
#' given year.
#'
#' @details
#' This is a unique tier achievement awarded to all fighters who participate in
#' at least one tournament during the year.
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
#' achievements <- ach_showing_up(tournament_data)
#' }
#'
#' @keywords internal
ach_showing_up <- function(data) {
  # Define tier details
  tier_details <- tibble::tribble(
    ~achievement_tier, ~tier_id, ~achievement_name_template,                       ~achievement_description_template,                       ~achievement_icon,
    "Unique",          1,        "Showing Up is Part of the Success! - {event_year}", "You showed up to a tournament in {event_year}!",        "showing_up_unique.png"
  )
  
  # Calculate achievements per year
  yearly_achievements <- data %>%
    dplyr::group_by(event_year, fighter_id) %>%
    dplyr::summarize(achieved = dplyr::n_distinct(tournament_id) > 0, .groups = "drop") %>%
    dplyr::filter(achieved) %>%
    dplyr::mutate(tier_id = 1)
  
  # Total fighters per year for percentile calculation
  total_fighters <- data %>%
    dplyr::group_by(event_year) %>%
    dplyr::summarize(total_fighters_in_year = dplyr::n_distinct(fighter_id), .groups = "drop")
  
  # Calculate the number of fighters who achieved the tier in each year
  tier_counts <- yearly_achievements %>%
    dplyr::group_by(event_year) %>%
    dplyr::summarize(tier_count = dplyr::n(), .groups = "drop")
  
  # Join tier details and compute the achievement
  achievements <- yearly_achievements %>%
    dplyr::left_join(tier_details, by = "tier_id") %>%
    dplyr::left_join(total_fighters, by = "event_year") %>%
    dplyr::left_join(tier_counts, by = "event_year") %>%
    dplyr::mutate(
      percentile = (tier_count / total_fighters_in_year) * 100, # Correct percentile calculation (percentage of fighters with the achievement)
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