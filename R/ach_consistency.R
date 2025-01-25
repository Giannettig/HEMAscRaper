#' Consistency is Key Achievement 
#'
#' @description
#' Awards achievements to fighters who participate in the most events within a
#' given year.
#'
#' @details
#' This is a unique tier achievement awarded only to fighters who participated in
#' the maximum number of events in a given year.
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
#' achievements <- ach_consistency_is_key(tournament_data)
#' }
#'
#' @keywords internal
ach_consistency_is_key <- function(data) {
  # Define achievement tier (unique for this achievement)
  tiers <- dplyr::tribble(
    ~achievement_tier, ~tier_id, ~achievement_name_template, ~achievement_description_template, ~achievement_icon,
    "Unique",          1,        "Consistency is Key {event_year}", "You participated in {max_events} events in {event_year}!", "consistency_key_unique.png"
  )
  
  # Filter out rows with missing event_id
  data <- data %>% dplyr::filter(!is.na(event_id))
  
  # Calculate the number of events participated in by each fighter per year
  yearly_events <- data %>%
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
  
  # Handle case where no fighters meet the criteria
  if (nrow(max_events_per_year) == 0) {
    return(data.frame(
      fighter_id = integer(0), tier_id = integer(0), achieved = logical(0),
      percentile = numeric(0), achievement_tier = character(0),
      achievement_name = character(0), achievement_description = character(0),
      achievement_icon = character(0), stringsAsFactors = FALSE
    ))
  }
  
  # Filter fighters who participated in the maximum number of events
  achievements <- yearly_events %>%
    dplyr::inner_join(max_events_per_year, by = "event_year") %>%
    dplyr::filter(total_events == max_events) %>%
    dplyr::mutate(
      tier_id = 1,
      achieved = TRUE
    )
  
  # Total fighters per year for percentile calculation
  total_fighters <- data %>%
    dplyr::group_by(event_year) %>%
    dplyr::summarize(
      total_fighters = dplyr::n_distinct(fighter_id),
      .groups = "drop"
    )
  
  # Calculate cumulative counts for percentiles
  tier_counts <- achievements %>%
    dplyr::group_by(event_year) %>%
    dplyr::summarize(
      tier_count = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::left_join(total_fighters, by = "event_year") %>%
    dplyr::mutate(
      percentile = ifelse(total_fighters == 0, 0, (1 - (tier_count / total_fighters)) * 100)%>%as.numeric()  # Correct percentile calculation
    )
  
  # Join tier details and calculate dynamic descriptions
  achievements <- achievements %>%
    dplyr::left_join(tiers, by = "tier_id") %>%
    dplyr::left_join(total_fighters, by = "event_year") %>%
    dplyr::left_join(tier_counts, by = "event_year")
  
  achievements$achievement_name<-stringr::str_replace_all(
    achievements$achievement_name_template,"\\{event_year\\}", as.character(achievements$event_year))
  
  achievements$achievement_description<-stringr::str_replace_all(
    achievements$achievement_description_template,"\\{max_events\\}", as.character(achievements$max_events))
  
  achievements$achievement_description<-stringr::str_replace_all(
    achievements$achievement_description,"\\{event_year\\}", as.character(achievements$event_year))
      

  achievements <- achievements %>%
    dplyr::select(
      fighter_id, tier_id, achieved, percentile,
      achievement_tier, achievement_name,
      achievement_description, achievement_icon
    )
  
  return(achievements)
}