ach_pig_slayer <- function(data) {
  # Define tiers
  tiers <- tibble::tribble(
    ~achievement_tier, ~tier_id, ~achievement_name,    ~achievement_description_template,                       ~achievement_icon,
    "Epic",            4,        "Pig Slayer!",       "You fought Alexander Stankievich {wins} times and never lost.", "pig_slayer_epic.png",
    "Gold",            3,        "Pig Slayer!",       "You fought Alexander Stankievich {wins} times.",               "pig_slayer_gold.png",
    "Silver",          2,        "Pig Slayer!",       "You fought Alexander Stankievich {wins} times.",               "pig_slayer_silver.png",
    "Bronze",          1,        "Pig Slayer!",       "You defeated Alexander Stankievich in a match.",               "pig_slayer_bronze.png"
  )
  
  # Summarize victories and losses against Alexander Stankievich (opponent_id = 152)
  stats <- data %>%
    dplyr::filter(opponent_id == 152) %>%
    dplyr::group_by(fighter_id) %>%
    dplyr::summarize(
      wins = sum(result == "WIN", na.rm = TRUE),
      losses = sum(result != "WIN", na.rm = TRUE),
      .groups = "drop"
    )
  
  # Assign tiers based on conditions
  achievements <- stats %>%
    dplyr::mutate(
      tier_id = dplyr::case_when(
        wins >= 5 & losses == 0 ~ 4,  # Epic
        wins >= 5               ~ 3,  # Gold
        wins >= 3               ~ 2,  # Silver
        wins >= 1               ~ 1,  # Bronze
        TRUE                    ~ NA_integer_
      )
    ) %>%
    dplyr::filter(!is.na(tier_id))  # Keep only fighters with achievements
  
  # Handle case where no achievements exist
  if (nrow(achievements) == 0) {
    return(data.frame(
      fighter_id = integer(0), tier_id = integer(0), achieved = logical(0),
      percentile = numeric(0), achievement_tier = character(0),
      achievement_name = character(0), achievement_description = character(0),
      achievement_icon = character(0), stringsAsFactors = FALSE
    ))
  }
  
  # Select the highest tier per fighter
  achievements <- achievements %>%
    dplyr::group_by(fighter_id) %>%
    dplyr::slice_max(tier_id, with_ties = FALSE) %>%
    dplyr::ungroup()
  
  # Calculate percentile
  total_fighters <- dplyr::n_distinct(data$fighter_id)
  tier_counts <- achievements %>%
    dplyr::group_by(tier_id) %>%
    dplyr::summarize(tier_count = dplyr::n(), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(tier_id)) %>%
    dplyr::mutate(cumulative_count = cumsum(tier_count))
  
  achievements <- achievements %>%
    dplyr::left_join(tier_counts, by = "tier_id") %>%
    dplyr::mutate(
      achieved = TRUE,
      percentile = (cumulative_count / total_fighters) * 100  # Adjusted percentile calculation
    )
  
  # Join with tier details and create dynamic descriptions
  achievements <- achievements %>%
    dplyr::left_join(tiers, by = "tier_id") %>%
    dplyr::mutate(
      achievement_description = stringr::str_replace_all(
        achievement_description_template,
        c("\\{wins\\}" = as.character(wins))
      ),
      achievement_icon = paste0("pig_slayer_", tolower(achievement_tier), ".png")
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