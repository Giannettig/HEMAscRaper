ach_king_midas <- function(data) {
  # Define tier details
  tier_details <- tibble::tribble(
    ~achievement_tier, ~tier_id, ~achievement_name,  ~achievement_description_template,                           ~achievement_icon,
    "Epic",            4,        "King Midas {event_year}",      "You won the most gold medals ({gold_medals}) in {event_year}!", "king_midas_epic.png"
  )
  
  # Filter for gold medal matches (finals where the fighter won)
  gold_medal_matches <- data %>%
    dplyr::filter(.data$is_final == TRUE, .data$result == "WIN")
  
  # Handle case where no gold medal matches exist
  if (nrow(gold_medal_matches) == 0) {
    return(data.frame(
      fighter_id = double(0), tier_id = double(0), achieved = logical(0),
      percentile = numeric(0), achievement_tier = character(0),
      achievement_name = character(0), achievement_description = character(0),
      achievement_icon = character(0), stringsAsFactors = FALSE
    ))
  }
  
  # Calculate the number of gold medals per fighter per year
  yearly_gold_counts <- gold_medal_matches %>%
    dplyr::group_by(event_year, fighter_id) %>%
    dplyr::summarize(gold_medals = dplyr::n_distinct(tournament_id), .groups = "drop")
  
  # Identify the fighter(s) with the most gold medals for each year
  yearly_midas <- yearly_gold_counts %>%
    dplyr::group_by(event_year) %>%
    dplyr::slice_max(gold_medals, with_ties = TRUE) %>%
    dplyr::mutate(
      tier_id = 4,
      achieved = TRUE
    )
  
  # Total fighters per year for percentile calculation
  total_fighters <- data %>%
    dplyr::group_by(event_year) %>%
    dplyr::summarize(total_fighters_in_year = dplyr::n_distinct(fighter_id), .groups = "drop")
  
  # Calculate cumulative counts for percentiles
  tier_counts <- yearly_midas %>%
    dplyr::group_by(event_year) %>%
    dplyr::summarize(tier_count = dplyr::n(), .groups = "drop") %>%
    dplyr::left_join(total_fighters, by = "event_year") %>%
    dplyr::mutate(
      percentile = ifelse(total_fighters_in_year == 0, 0, (tier_count / total_fighters_in_year) * 100)  # Percentile is proportion achieving the tier
    )
  
  # Join tier details and compute the achievement
  achievements <- yearly_midas %>%
    dplyr::left_join(tier_details, by = "tier_id") %>%
    dplyr::left_join(tier_counts, by = "event_year") %>%
    dplyr::mutate(
      achievement_name = stringr::str_replace(
        achievement_name,
        "\\{event_year\\}",as.character(event_year)
      ),
      achievement_description = stringr::str_replace(
        achievement_description_template,
        "\\{gold_medals\\}", as.character(gold_medals))%>%
        stringr::str_replace( "\\{event_year\\}", as.character(event_year)
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      fighter_id,
      tier_id,
      achieved,
      percentile,
      achievement_tier,
      achievement_name,
      achievement_description,
      achievement_icon
    )
  
  return(achievements)
}