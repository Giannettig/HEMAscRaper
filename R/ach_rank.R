#' @title ach_rank_longsword
#' @description Find the top fighters for each tournament weapon and year! Fighters are ranked based on their Bayesian win percentage, and achievements are awarded:
#' - Epic: Top 1% of fighters.
#' - Gold: Top 5% of fighters.
#' - Silver: Top 15% of fighters.
#' - Bronze: Top 30% of fighters.
#' @param data A dataset containing match data, including `tournament_weapon`, `event_year`, and `result`.
#' @return A data frame of fighters, their ranks, and corresponding achievements for each weapon and year.
#' @keywords internal
ach_rank_longsword <- function(data) {
  # Define achievement tiers
  tiers <- dplyr::tribble(
    ~achievement_tier, ~tier_id, ~achievement_name_template, ~achievement_description_template, ~achievement_icon,
    "Epic",            4,        "Top 1% {tournament_weapon} Fighter {event_year}", "You're in the top 1% of {tournament_weapon} fighters! In {event_year} you won {posterior_mean}% of your fights!", "rank_epic.png",
    "Gold",            3,        "Top 5% {tournament_weapon} Fighter {event_year}", "You're in the top 5% of {tournament_weapon} fighters! In {event_year} you won {posterior_mean}% of your fights!", "rank_gold.png",
    "Silver",          2,        "Top 15% {tournament_weapon} Fighter {event_year}", "You're in the top 15% of {tournament_weapon} fighters! In {event_year} you won {posterior_mean}% of your fights!", "rank_silver.png",
    "Bronze",          1,        "Top 30% {tournament_weapon} Fighter {event_year}", "You're in the top 30% of {tournament_weapon} fighters! In {event_year} you won {posterior_mean}% of your fights!", "rank_bronze.png"
  )

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
      posterior_mean = alpha_posterior / (alpha_posterior + beta_posterior)
    ) %>%
    dplyr::group_by(event_year, tournament_weapon) %>%
    dplyr::arrange(dplyr::desc(posterior_mean)) %>%
    dplyr::mutate(
      rank = dplyr::row_number(),
      total_fighters = dplyr::n(),
      percentile = rank / total_fighters
    ) %>%
    dplyr::ungroup()

  # Assign tiers based on percentiles
  weapon_year_tiers <- weapon_year_stats %>%
    dplyr::mutate(
      tier_id = dplyr::case_when(
        percentile <= 1  ~ 4,  # Epic
        percentile <= 5  ~ 3,  # Gold
        percentile <= 15 ~ 2,  # Silver
        percentile <= 30 ~ 1,  # Bronze,
        TRUE             ~ NA_real_
      )
    ) %>%
    dplyr::filter(!is.na(tier_id))

  # Join tier details and format output
  achievements <- weapon_year_tiers %>%
    filter(!is.na(event_year) & !is.na(tournament_weapon) & !is.na(posterior_mean)) %>%
    dplyr::left_join(tiers, by = "tier_id") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      achievement_name = achievement_name_template %>%
        stringr::str_replace_all("\\{tournament_weapon\\}" , tournament_weapon)%>%
        stringr::str_replace_all("\\{event_year\\}" , as.character(event_year)),

      achievement_description = achievement_description_template %>%
        stringr::str_replace_all( "\\{tournament_weapon\\}", tournament_weapon) %>%
        stringr::str_replace_all("\\{event_year\\}", as.character(event_year)) %>%
        stringr::str_replace_all( "\\{posterior_mean\\}" , sprintf("%.2f", posterior_mean * 100))
      ,
      achieved = !is.na(achievement_name)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(achieved == TRUE) %>%
    dplyr::select(
      fighter_id,
      tier_id,
      achieved,
      percentile,
      achievement_tier,
      achievement_name,
      achievement_description,
      achievement_icon
    ) %>%
    dplyr::mutate(
      fighter_id = as.double(fighter_id),  # Ensure correct type
      tier_id = as.double(tier_id)        # Ensure correct type
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

