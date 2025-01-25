#' Epiphany of Steel Achievement
#'
#' @description
#' Awards achievements to fighters based on the number of different steel weapon
#' categories they've competed with.
#'
#' @details
#' Tiers are awarded based on number of distinct steel weapon categories:
#' - Epic (4): 7+ weapons
#' - Gold (3): 6 weapons
#' - Silver (2): 5 weapons
#' - Bronze (1): 3+ weapons
#'
#' The main steel weapons considered are: Steel Sabre, Steel Single Rapier,
#' Steel Rapier & Dagger, Steel Single Sidesword, Steel Smallsword,
#' Steel Messer, and Steel Longsword.
#'
#' @param data A data frame containing HEMA tournament match data
#'
#' @return A data frame of achievements with columns:
#' \itemize{
#'   \item fighter_id: Unique fighter identifier
#'   \item tier_id: Achievement tier level (1-4)
#'   \item achieved: Logical indicating if achievement earned
#'   \item percentile: Fighter's percentile for this achievement
#'   \item achievement_tier: Text description of tier
#'   \item achievement_name: Name of the achievement
#'   \item achievement_description: Description of what was achieved
#'   \item achievement_icon: Icon file name for the achievement
#' }
#'
#' @examples
#' \dontrun{
#' achievements <- ach_epiphany_of_steel(tournament_data)
#' }
#'
#' @keywords internal
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

