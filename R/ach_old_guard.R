#' Identify "The Old Guard"
#'
#' @description
#' Internal function to award "The Old Guard" achievement.
#' Determines which fighter(s) have the longest fighting interval, defined as the
#' difference between their earliest and latest recorded `event_year`.
#'
#' @param data A data frame containing at least `fighter_id` and `event_year`.
#'
#' @return A data frame with columns:
#' \describe{
#'   \item{fighter_id}{Identifier of the fighter(s) who achieved this.}
#'   \item{tier_id}{Numeric tier ID for the achievement.}
#'   \item{achieved}{Logical, always TRUE for those who achieved it.}
#'   \item{percentile}{Proportion of total fighters achieving this.}
#'   \item{achievement_tier}{Character, "Unique".}
#'   \item{achievement_name}{Character, "The Old Guard".}
#'   \item{achievement_description}{Character, description of the achievement.}
#'   \item{achievement_icon}{Character, icon filename for this achievement.}
#' }
#'
#' @keywords internal
ach_the_old_guard <- function(data) {
  # Filter out rows with missing event_year or fighter_id
  data <- data %>% dplyr::filter(!is.na(event_year) & !is.na(fighter_id))
  
  # Compute min and max year per fighter
  min_years <- tapply(data$event_year, data$fighter_id, min, na.rm = TRUE)
  max_years <- tapply(data$event_year, data$fighter_id, max, na.rm = TRUE)
  
  # Handle cases where no valid min or max year exists
  intervals <- max_years - min_years
  intervals[is.infinite(intervals)] <- NA  # Set invalid intervals to NA
  
  # Find the maximum interval
  max_interval <- max(intervals, na.rm = TRUE)
  
  # Identify fighters who have this maximum interval
  longest_fighters <- names(intervals)[intervals == max_interval]
  
  # Convert fighter_id names back to numeric if possible
  fighter_id_final <- suppressWarnings(as.numeric(longest_fighters))
  if (all(is.na(fighter_id_final))) {
    fighter_id_final <- longest_fighters
  }
  
  # Compute total fighters for percentile
  total_fighters <- length(unique(data$fighter_id))
  percentile <- length(longest_fighters) / total_fighters
  
  # Achievement details
  tier_id <- 1
  achieved <- TRUE
  achievement_tier <- "Unique"
  achievement_name <- "The Old Guard"
  achievement_description <- paste0(
    "You are the longest fighting fencer on the planet with an interval of ", 
    max_interval, " years between your first and last recorded fights."
  )
  achievement_icon <- "the_old_guard.png"
  
  # Construct output
  achievements <- data.frame(
    fighter_id = fighter_id_final,
    tier_id = tier_id,
    achieved = achieved,
    percentile = percentile,
    achievement_tier = achievement_tier,
    achievement_name = achievement_name,
    achievement_description = achievement_description,
    achievement_icon = achievement_icon,
    stringsAsFactors = FALSE
  )
  
  return(achievements)
}