ach_rank_longsword <- function(data) {
  # Define achievement tiers
  tiers <- dplyr::tribble(
    ~achievement_tier, ~tier_id, ~achievement_name_template, ~achievement_description_template, ~achievement_icon,
    "Epic",            4,        "Top 1% {tournament_weapon} Fighter {event_year}", "You're in the top 1% of {tournament_weapon} fighters! In {event_year} you won {posterior_mean}% of your fights!", "rank_epic.png",
    "Gold",            3,        "Top 5% {tournament_weapon} Fighter {event_year}", "You're in the top 5% of {tournament_weapon} fighters! In {event_year} you won {posterior_mean}% of your fights!", "rank_gold.png",
    "Silver",          2,        "Top 15% {tournament_weapon} Fighter {event_year}", "You're in the top 15% of {tournament_weapon} fighters! In {event_year} you won {posterior_mean}% of your fights!", "rank_silver.png",
    "Bronze",          1,        "Top 20% {tournament_weapon} Fighter {event_year}", "You're in the top 30% of {tournament_weapon} fighters! In {event_year} you won {posterior_mean}% of your fights!", "rank_bronze.png"
  )
  
  # Filter out rows with missing values early
  data <- data %>%
    dplyr::filter(!is.na(event_year) & !is.na(tournament_weapon) & !is.na(result))
  
  # Calculate fighter stats and rankings per tournament weapon and year
  weapon_year_stats <- data %>%
    dplyr::group_by(event_year, tournament_weapon, fighter_id) %>%
    dplyr::summarize(
      wins = sum(result == "WIN", na.rm = TRUE),
      losses = sum(result != "WIN", na.rm = TRUE),
      total_matches = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      alpha_posterior = 1 + wins,
      beta_posterior = 1 + losses,
      posterior_mean = alpha_posterior / (alpha_posterior + beta_posterior)  # Posterior mean win rate
    ) %>%
    dplyr::group_by(event_year, tournament_weapon) %>%
    dplyr::arrange(dplyr::desc(posterior_mean)) %>%
    dplyr::mutate(
      rank = dplyr::row_number(),
      total_fighters = dplyr::n(),
      percentile = (1 - (rank - 1) / total_fighters) * 100  # Percentile calculation
    ) %>%
    dplyr::ungroup()
  
  # Assign tiers based on percentiles
  weapon_year_tiers <- weapon_year_stats %>%
    dplyr::mutate(
      tier_id = dplyr::case_when(
        percentile >= 99 ~ 4,  # Epic (top 1%)
        percentile >= 95 ~ 3,  # Gold (top 5%)
        percentile >= 85 ~ 2,  # Silver (top 15%)
        percentile >= 80 ~ 1,  # Bronze (top 30%)
        TRUE             ~ NA_real_
      )
    ) %>%
    dplyr::filter(!is.na(tier_id))
  
  # Join tier details and format output
  achievements <- weapon_year_tiers %>%
    dplyr::left_join(tiers, by = "tier_id") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      achievement_name = stringr::str_replace_all(achievement_name_template, "\\{tournament_weapon\\}", tournament_weapon) %>%
        stringr::str_replace_all("\\{event_year\\}", as.character(event_year)),
      
      achievement_description = stringr::str_replace_all(achievement_description_template, "\\{tournament_weapon\\}", tournament_weapon) %>%
        stringr::str_replace_all("\\{event_year\\}", as.character(event_year)) %>%
        stringr::str_replace_all("\\{posterior_mean\\}", sprintf("%.2f", posterior_mean * 100)),  # Convert to percentage
      
      achieved = TRUE
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
  
  # Handle case where no achievements exist
  if (nrow(achievements) == 0) {
    return(data.frame(
      fighter_id = double(0), tier_id = double(0), achieved = logical(0),
      percentile = numeric(0), achievement_tier = character(0),
      achievement_name = character(0), achievement_description = character(0),
      achievement_icon = character(0), stringsAsFactors = FALSE
    ))
  }
  
  return(achievements)
}