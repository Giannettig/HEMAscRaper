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