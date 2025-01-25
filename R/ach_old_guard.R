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