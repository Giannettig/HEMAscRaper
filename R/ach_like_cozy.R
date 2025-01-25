ach_like_cozy <- function(data) {
  # Define tiers
  tiers <- tibble::tribble(
    ~achievement_tier, ~tier_id, ~achievement_name,  ~achievement_description_template,                                            ~achievement_icon,
    "Unique",           1,        "I like it Cozy",  "You fought a total of {matches_home} matches in your home country without ever leaving.",     "i_like_it_cozy_unique.png"
  )
  
  # Filter out rows with missing club_country or event_country
  data <- data %>% dplyr::filter(!is.na(club_country) & !is.na(event_country))
  
  # Determine if each match was fought in the home country
  ach <- data %>%
    dplyr::mutate(
      is_home = (club_country == event_country)  # TRUE if the match is in the home country
    ) %>%
    dplyr::group_by(fighter_id) %>%
    dplyr::summarize(
      total_matches = dplyr::n_distinct(match_id, na.rm = TRUE),
      matches_home = sum(is_home, na.rm = TRUE),  # Count of matches fought at home
      .groups = "drop"
    ) %>%
    dplyr::filter(
      total_matches > 100,  # Only consider fighters with more than 100 matches
      matches_home == total_matches  # Ensure all matches were fought at home
    ) %>%
    dplyr::mutate(
      tier_id = 1,
      achieved = TRUE
    )
  
  # Handle case where no fighters meet the criteria
  if (nrow(ach) == 0) {
    return(data.frame(
      fighter_id = integer(0), tier_id = integer(0), achieved = logical(0),
      percentile = numeric(0), achievement_tier = character(0),
      achievement_name = character(0), achievement_description = character(0),
      achievement_icon = character(0), stringsAsFactors = FALSE
    ))
  }
  
  # Calculate percentile
  total_fighters <- dplyr::n_distinct(data$fighter_id)
  ach <- ach %>%
    dplyr::mutate(
      percentile = (1 / total_fighters) * 100  # Percentile calculation for Unique achievement
    )
  
  # Join tier details and create dynamic descriptions
  achievements <- ach %>%
    dplyr::left_join(tiers, by = "tier_id") %>%
    dplyr::mutate(
      achievement_description = stringr::str_replace_all(
        achievement_description_template,
        c("\\{matches_home\\}" = as.character(matches_home))
      )
    ) %>%
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