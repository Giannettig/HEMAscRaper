ach_epiphany_of_steel <- function(data) {
  # 7 main steel HEMA weapons
  main_steel_weapons <- c(
    "Steel Sabre",
    "Steel Single Rapier",
    "Steel Rapier & Dagger",
    "Steel Single Sidesword",
    "Steel Smallsword",
    "Steel Messer",
    "Steel Longsword"
  )
  
  # Tier definitions
  tiers <- tibble::tribble(
    ~achievement_tier, ~tier_id, ~achievement_name,         ~achievement_description_template, ~achievement_icon,
    "Epic",            4,        "Epiphany of Steel",      "You fought in tournaments with {weapon_count} of the 7 main steel HEMA weapons!", "epiphany_of_steel_epic.png",
    "Gold",            3,        "Epiphany of Steel",      "You fought in tournaments with {weapon_count} of the 7 main steel HEMA weapons!", "epiphany_of_steel_gold.png",
    "Silver",          2,        "Epiphany of Steel",      "You fought in tournaments with {weapon_count} of the 7 main steel HEMA weapons!", "epiphany_of_steel_silver.png",
    "Bronze",          1,        "Epiphany of Steel",      "You fought in tournaments with {weapon_count} of the 7 main steel HEMA weapons!", "epiphany_of_steel_bronze.png"
  )
  
  # Filter out rows with missing tournament_weapon
  data <- data %>% dplyr::filter(!is.na(tournament_weapon))
  
  # Compute weapon counts per fighter
  weapon_counts <- data %>%
    dplyr::filter(tournament_weapon %in% main_steel_weapons) %>%
    dplyr::group_by(fighter_id) %>%
    dplyr::summarize(weapon_count = dplyr::n_distinct(tournament_weapon), .groups = "drop")
  
  # Handle case where no fighters meet the criteria
  if (nrow(weapon_counts) == 0) {
    return(data.frame(
      fighter_id = integer(0), tier_id = integer(0), achieved = logical(0),
      percentile = numeric(0), achievement_tier = character(0),
      achievement_name = character(0), achievement_description = character(0),
      achievement_icon = character(0), stringsAsFactors = FALSE
    ))
  }
  
  # Assign tiers based on weapon_count
  weapon_counts <- weapon_counts %>%
    dplyr::mutate(
      tier_id = dplyr::case_when(
        weapon_count >= 7 ~ 4,  # Epic
        weapon_count >= 6 ~ 3,  # Gold
        weapon_count >= 5 ~ 2,  # Silver
        weapon_count >= 3 ~ 1,  # Bronze
        TRUE               ~ NA_integer_
      )
    ) %>%
    dplyr::filter(!is.na(tier_id))  # Only keep fighters who achieved a tier
  
  # Handle case where no fighters meet the criteria
  if (nrow(weapon_counts) == 0) {
    return(data.frame(
      fighter_id = integer(0), tier_id = integer(0), achieved = logical(0),
      percentile = numeric(0), achievement_tier = character(0),
      achievement_name = character(0), achievement_description = character(0),
      achievement_icon = character(0), stringsAsFactors = FALSE
    ))
  }
  
  # Total fighters for percentile calculation
  total_fighters <- dplyr::n_distinct(data$fighter_id)
  
  # Calculate cumulative counts for percentiles
  tier_counts <- weapon_counts %>%
    dplyr::group_by(tier_id) %>%
    dplyr::summarize(tier_count = dplyr::n(), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(tier_id)) %>%
    dplyr::mutate(cumulative_count = cumsum(tier_count))
  
  # Join tier details and calculate dynamic descriptions
  achievements <- weapon_counts %>%
    dplyr::left_join(tiers, by = "tier_id") %>%
    dplyr::left_join(tier_counts, by = "tier_id") %>%
    dplyr::mutate(
      achievement_description = stringr::str_replace(
        achievement_description_template,"\\{weapon_count\\}",as.character(weapon_count)),
      achieved = TRUE,
      percentile = (1 - (cumulative_count / total_fighters)) * 100  # Correct percentile calculation
    ) %>%
    dplyr::select(
      fighter_id, tier_id, achieved, percentile,
      achievement_tier, achievement_name,
      achievement_description, achievement_icon
    )
  
  return(achievements)
}

