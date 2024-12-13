#' @title ach_reaper
#' @description Awarded to the fighter who won the most fights in the world during a specific year. 
#' If multiple fighters share the most wins, they all receive the achievement.
#' @param data A dataset containing match data, including `event_year`, `fighter_id`, and `result`.
#' @return A data frame with fighters who won the most fights globally in a given year.
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
  
  # Filter fighters who achieved the maximum wins
  achievements <- yearly_wins %>%
    dplyr::inner_join(max_wins_per_year, by = "event_year") %>%
    dplyr::filter(total_wins == max_wins) %>%
    dplyr::mutate(
      tier_id = 1,
      achieved = TRUE,
      percentile = dplyr::n() / dplyr::n_distinct(data$fighter_id)
    ) %>%
    dplyr::left_join(tiers, by = "tier_id") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      achievement_name = stringr::str_replace_all(
        achievement_name_template,
        c("\\{event_year\\}" = as.character(event_year))
      ),
      achievement_description = stringr::str_replace_all(
        achievement_description_template,
        c("\\{max_wins\\}" = as.character(max_wins))
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      fighter_id, tier_id, achieved, percentile,
      achievement_tier, achievement_name,
      achievement_description, achievement_icon
    )%>%filter(!is.na(achievement_name))
  
  return(achievements)
}