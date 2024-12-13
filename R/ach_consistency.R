#' @title ach_consistency_is_key
#' @description Awarded to the fighter who participated in the most events during a specific year.
#' If multiple fighters share the highest number of events participated in, they all receive the achievement.
#' @param data A dataset containing match data, including `event_year`, `fighter_id`, and `event_id`.
#' @return A data frame with fighters who participated in the most events in a given year.
#' @keywords internal
ach_consistency_is_key <- function(data) {
  
  # Define achievement tier (unique for this achievement)
  tiers <- dplyr::tribble(
    ~achievement_tier, ~tier_id, ~achievement_name_template, ~achievement_description_template, ~achievement_icon,
    "Unique",          1,        "Consistency is Key {event_year}", "You participated in {max_events} events in {event_year}!", "consistency_key_unique.png"
  )
  
  # Calculate the number of events participated in by each fighter per year
  yearly_events <- data %>%
    dplyr::filter(!is.na(event_id)) %>%
    dplyr::group_by(event_year, fighter_id) %>%
    dplyr::summarize(
      total_events = dplyr::n_distinct(event_id, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Identify the maximum number of events participated in per year
  max_events_per_year <- yearly_events %>%
    dplyr::group_by(event_year) %>%
    dplyr::summarize(
      max_events = max(total_events, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Filter fighters who participated in the maximum number of events
  achievements <- yearly_events %>%
    dplyr::inner_join(max_events_per_year, by = "event_year") %>%
    dplyr::filter(total_events == max_events) %>%
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
        c("\\{max_events\\}" = as.character(max_events))
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      fighter_id, tier_id, achieved, percentile,
      achievement_tier, achievement_name,
      achievement_description, achievement_icon
    ) %>%
    dplyr::filter(!is.na(achievement_name)) # Ensure only valid achievements are returned
  
  return(achievements)
}
