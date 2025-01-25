#' The Old Guard Achievement
#'
#' @description
#' Awards achievements to fighters who have been competing in HEMA tournaments for
#' the longest period of time.
#'
#' @details
#' This is a unique tier achievement awarded to fighters who have the maximum
#' interval between their first and last recorded fights.
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
#' achievements <- ach_the_old_guard(tournament_data)
#' }
#'
#' @keywords internal
ach_the_old_guard <- function(data) {
  # Filter out rows with missing event_year or fighter_id
  data <- data %>% dplyr::filter(!is.na(event_year) & !is.na(fighter_id))
  
  # Compute min and max year per fighter
  fighter_intervals <- data %>%
    dplyr::group_by(fighter_id) %>%
    dplyr::summarize(
      min_year = min(event_year, na.rm = TRUE),
      max_year = max(event_year, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      interval = max_year - min_year
    )
  
  # Find the maximum interval
  max_interval <- max(fighter_intervals$interval, na.rm = TRUE)
  
  # Identify fighters who have this maximum interval
  longest_fighters <- fighter_intervals %>%
    dplyr::filter(interval == max_interval)
  
  # Compute total fighters for percentile
  total_fighters <- length(unique(data$fighter_id))
  percentile <- nrow(longest_fighters) / total_fighters
  
  # Achievement details
  tier_id <- 1
  achieved <- TRUE
  achievement_tier <- "Unique"
  achievement_name <- "The Old Guard"
  achievement_description <- paste0(
    "You are among the longest fighting fencers on the planet with an interval of ",
    max_interval, " years between your first and last recorded fights."
  )
  achievement_icon <- "the_old_guard.png"
  
  # Construct output
  achievements <- longest_fighters %>%
    dplyr::mutate(
      tier_id = tier_id,
      achieved = achieved,
      percentile = percentile,
      achievement_tier = achievement_tier,
      achievement_name = achievement_name,
      achievement_description = achievement_description,
      achievement_icon = achievement_icon
    ) %>%
    dplyr::select(
      fighter_id, tier_id, achieved, percentile, 
      achievement_tier, achievement_name, achievement_description, achievement_icon
    )
  
  # Handle case where no achievements exist
  if (nrow(achievements) == 0) {
    return(data.frame(
      fighter_id = integer(0), tier_id = integer(0), achieved = logical(0),
      percentile = numeric(0), achievement_tier = character(0),
      achievement_name = character(0), achievement_description = character(0),
      achievement_icon = character(0), stringsAsFactors = FALSE
    ))
  }
  
  return(achievements)
}