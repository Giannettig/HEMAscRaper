#' @title ach_marco_polo
#' @description Awarded to the fighter who traveled to the most countries (at least 2) during a specific year.
#' If multiple fighters share the most countries visited, they all receive the achievement.
#' @param data A dataset containing match data, including `event_year`, `fighter_id`, and `event_country`.
#' @return A data frame with fighters who traveled to the most countries in a given year.
#' @keywords internal
ach_marco_polo <- function(data) {
  
  # Define achievement tier (unique for this achievement)
  tiers <- dplyr::tribble(
    ~achievement_tier, ~tier_id, ~achievement_name_template, ~achievement_description_template, ~achievement_icon,
    "Unique",          1,        "Marco Polo {event_year}", "You traveled to {max_countries} countries in {event_year}!", "marco_polo_unique.png"
  )
  
  # Calculate the distinct countries visited by each fighter per year
  yearly_travel <- data %>%
    dplyr::filter(!is.na(event_country)) %>%
    dplyr::group_by(event_year, fighter_id) %>%
    dplyr::summarize(
      total_countries = dplyr::n_distinct(event_country, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Identify the maximum countries visited per year where the number of countries is greater than one
  max_countries_per_year <- yearly_travel %>%
    dplyr::filter(total_countries > 1) %>%
    dplyr::group_by(event_year) %>%
    dplyr::summarize(
      max_countries = max(total_countries, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Filter fighters who achieved the maximum countries visited
  achievements <- yearly_travel %>%
    dplyr::inner_join(max_countries_per_year, by = "event_year") %>%
    dplyr::filter(total_countries == max_countries) %>%
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
        c("\\{max_countries\\}" = as.character(max_countries))
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