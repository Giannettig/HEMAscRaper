ach_marco_polo <- function(data) {
  # Define achievement tier (unique for this achievement)
  tiers <- tibble::tribble(
    ~achievement_tier, ~tier_id, ~achievement_name_template, ~achievement_description_template, ~achievement_icon,
    "Unique",          1,        "Marco Polo {event_year}", "You traveled to {max_countries} countries in {event_year}!", "marco_polo_unique.png"
  )
  
  # Filter out rows with missing event_country
  if (!all(c("event_year", "fighter_id", "event_country") %in% names(data))) {
    stop("Input data must contain `event_year`, `fighter_id`, and `event_country` columns.")
  }
  data <- data %>% dplyr::filter(!is.na(event_country))
  
  # Calculate the distinct countries visited by each fighter per year
  yearly_travel <- data %>%
    dplyr::group_by(event_year, fighter_id) %>%
    dplyr::summarize(
      total_countries = dplyr::n_distinct(event_country, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Identify the maximum countries visited per year
  max_countries_per_year <- yearly_travel %>%
    dplyr::group_by(event_year) %>%
    dplyr::summarize(
      max_countries = max(total_countries, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Handle case where no fighters traveled to at least 1 country in any year
  if (nrow(max_countries_per_year) == 0) {
    return(data.frame(
      fighter_id = integer(0), tier_id = integer(0), achieved = logical(0),
      percentile = numeric(0), achievement_tier = character(0),
      achievement_name = character(0), achievement_description = character(0),
      achievement_icon = character(0), stringsAsFactors = FALSE
    ))
  }
  
  # Filter fighters who achieved the maximum countries visited
  achievements <- yearly_travel %>%
    dplyr::inner_join(max_countries_per_year, by = "event_year") %>%
    dplyr::filter(total_countries == max_countries) %>%
    dplyr::mutate(
      tier_id = 1,  # Add tier_id column
      achieved = TRUE
    )
  
  # Calculate total fighters per year for percentile calculation
  total_fighters_per_year <- data %>%
    dplyr::group_by(event_year) %>%
    dplyr::summarize(
      total_fighters = dplyr::n_distinct(fighter_id),
      .groups = "drop"
    )
  
  # Calculate percentiles for the achievement
  achievements <- achievements %>%
    dplyr::left_join(total_fighters_per_year, by = "event_year") %>%
    dplyr::mutate(
      percentile = ifelse(total_fighters == 0, 0, (1 / total_fighters) * 100)  # Percent of total fighters
    )
  
  # Join tier details and create dynamic descriptions
  achievements <- achievements %>%
    dplyr::left_join(tiers, by = "tier_id") %>%
    dplyr::mutate(
      achievement_name = stringr::str_replace(
        achievement_name_template,"\\{event_year\\}", as.character(event_year)),
      achievement_description = stringr::str_replace(
        achievement_description_template,"\\{max_countries\\}",as.character(max_countries))%>%
          stringr::str_replace("\\{event_year\\}", as.character(event_year)
      )
    ) %>%
    dplyr::select(
      fighter_id, tier_id, achieved, percentile,
      achievement_tier, achievement_name,
      achievement_description, achievement_icon
    )
  
  return(achievements)
}