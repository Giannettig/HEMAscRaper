#' Iron Maiden Achievement
#'
#' @description
#' Awards achievements to fighters based on the total number of different HEMA
#' weapon categories they've competed with.
#'
#' @details
#' Tiers are awarded based on number of distinct weapon categories:
#' - Epic (4): 20+ weapons
#' - Gold (3): 15+ weapons
#' - Silver (2): 5+ weapons
#' - Bronze (1): 2+ weapons
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
#' achievements <- ach_iron_maiden(tournament_data)
#' }
#'
#' @keywords internal
ach_iron_maiden <- function(data) {
  # Define tiers
  tiers <- tibble::tribble(
    ~achievement_tier, ~tier_id, ~achievement_name, ~achievement_description_template, ~achievement_icon,
    "Epic",    4, "Iron Maiden", "You fought in {weapon_count} HEMA weapon categories!", "iron_maiden_epic.png",
    "Gold",    3, "Iron Maiden", "You fought in {weapon_count} HEMA weapon categories!", "iron_maiden_gold.png",
    "Silver",  2, "Iron Maiden", "You fought in {weapon_count} HEMA weapon categories!", "iron_maiden_silver.png",
    "Bronze",  1, "Iron Maiden", "You fought in {weapon_count} HEMA weapon categories!", "iron_maiden_bronze.png"
  )
  
  # Filter out rows with missing tournament_weapon
  data <- data %>% dplyr::filter(!is.na(tournament_weapon))
  
  # Compute weapon_count per fighter
  weapon_counts <- data %>%
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
  
  # Assign tier_id based on weapon_count
  weapon_counts <- weapon_counts %>%
    dplyr::mutate(
      tier_id = dplyr::case_when(
        weapon_count >= 20 ~ 4,  # Epic
        weapon_count >= 15 ~ 3,  # Gold
        weapon_count >= 5  ~ 2,  # Silver
        weapon_count >= 2  ~ 1,  # Bronze
        TRUE               ~ NA_integer_
      )
    ) %>%
    dplyr::filter(!is.na(tier_id))  # Only keep fighters who achieved a tier
  
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
        achievement_description_template,"\\{weapon_count\\}", as.character(weapon_count)
      ),
      achieved = TRUE,
      percentile = (cumulative_count / total_fighters) * 100  # Correct percentile calculation
    ) %>%
    dplyr::select(
      fighter_id, tier_id, achieved, percentile,
      achievement_tier, achievement_name,
      achievement_description, achievement_icon
    )
  
  return(achievements)
}