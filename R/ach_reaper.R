#' The Reaper Achievement
#'
#' @description
#' Awards achievements to fighters who achieve the highest number of wins globally
#' in a given year.
#'
#' @details
#' This is a unique tier achievement awarded to fighters who win the most fights
#' in a given year.
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
#' achievements <- ach_reaper(tournament_data)
#' }
#'
#' @keywords internal
ach_reaper <- function(data) {
  # Define achievement tier (unique for this achievement)
  tiers <- dplyr::tribble(
    ~achievement_tier, ~tier_id, ~achievement_name_template, ~achievement_description_template, ~achievement_icon,
    "Unique",          1,        "The Reaper {event_year}", "You won the most fights globally ({max_wins}) in {event_year}!", "reaper_unique.png"
  )
  
  # Calculate the total wins per fighter per year
  yearly_wins <- data %>%
    dplyr::filter(result == "WIN") %>%
    dplyr::group_by(event_year, fighter_id) %>%
    dplyr::summarize(
      total_wins = dplyr::n(),
      .groups = "drop"
    )
  
  # Identify the maximum wins per year
  max_wins_per_year <- yearly_wins %>%
    dplyr::group_by(event_year) %>%
    dplyr::summarize(
      max_wins = max(total_wins, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Total fighters per year for percentile calculation
  total_fighters_per_year <- data %>%
    dplyr::group_by(event_year) %>%
    dplyr::summarize(
      total_fighters = dplyr::n_distinct(fighter_id),
      .groups = "drop"
    )
  
  # Filter fighters who achieved the maximum wins
  achievements <- yearly_wins %>%
    dplyr::inner_join(max_wins_per_year, by = "event_year") %>%
    dplyr::filter(total_wins == max_wins) %>%
    dplyr::left_join(total_fighters_per_year, by = "event_year") %>%
    dplyr::mutate(
      tier_id = 1,
      achieved = TRUE,
      percentile = (1 / total_fighters) * 100 # Percentile is based on total fighters in the year
    ) %>%
    dplyr::left_join(tiers, by = "tier_id") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      achievement_name = stringr::str_replace_all(
        achievement_name_template,
        "\\{event_year\\}",
        as.character(event_year)
      ),
      achievement_description = stringr::str_replace_all(
        achievement_description_template,
        c("\\{max_wins\\}" = as.character(max_wins),
          "\\{event_year\\}" = as.character(event_year))
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      fighter_id, tier_id, achieved, percentile,
      achievement_tier, achievement_name,
      achievement_description, achievement_icon
    )
  
  return(achievements)
}