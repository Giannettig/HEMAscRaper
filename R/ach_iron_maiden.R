#' Iron Maiden Achievement
#'
#' @description
#' Internal function to award "Iron Maiden" based on how many distinct HEMA weapon categories
#' a fighter has fought in. The tiers are defined as:
#' - Epic: ≥20 weapon categories
#' - Gold: ≥15 weapon categories
#' - Silver: ≥5  weapon categories
#' - Bronze: ≥2  weapon categories
#'
#' The achievement description dynamically includes the fighter's actual number of weapon categories fought.
#'
#' @param data A data frame with at least `fighter_id` and `tournament_weapon`.
#'
#' @return A data frame with columns:
#' \describe{
#'   \item{fighter_id}{ID of the fighter}
#'   \item{tier_id}{Numeric tier ID (1-4)}
#'   \item{achieved}{Logical, TRUE if achieved}
#'   \item{percentile}{Proportion of fighters who achieved this tier or higher}
#'   \item{achievement_tier}{One of "Bronze", "Silver", "Gold", "Epic"}
#'   \item{achievement_name}{"Iron Maiden"}
#'   \item{achievement_description}{Includes the actual number of weapon categories fought}
#'   \item{achievement_icon}{e.g. "iron_maiden_bronze.png"}
#' }
#'
#' @importFrom dplyr group_by summarize n_distinct mutate filter ungroup left_join slice_max select
#' @importFrom tibble tribble
#' @keywords internal
ach_iron_maiden <- function(data) {
  # Define tiers
  tiers <- tibble::tribble(
    ~achievement_tier, ~tier_id, ~achievement_name, ~achievement_description, ~achievement_icon,
    "Epic",    4, "Iron Maiden", "You fought in 20 or more HEMA weapon categories!",    "iron_maiden_epic.png",
    "Gold",    3, "Iron Maiden", "You fought in 15 or more HEMA weapon categories!",    "iron_maiden_gold.png",
    "Silver",  2, "Iron Maiden", "You fought in 5 or more HEMA weapon categories!",     "iron_maiden_silver.png",
    "Bronze",  1, "Iron Maiden", "You fought in 2 or more HEMA weapon categories!",     "iron_maiden_bronze.png"
  )
  
  # Compute weapon_count per fighter
  weapon_counts <- data %>%
    dplyr::group_by(fighter_id) %>%
    dplyr::summarize(weapon_count = dplyr::n_distinct(tournament_weapon), .groups = "drop")
  
  if (nrow(weapon_counts) == 0) {
    return(data.frame(
      fighter_id = integer(0), tier_id = integer(0), achieved = logical(0),
      percentile = numeric(0), achievement_tier = character(0),
      achievement_name = character(0), achievement_description = character(0),
      achievement_icon = character(0), stringsAsFactors = FALSE
    ))
  }
  
  # Assign tier_id based on weapon_count
  assign_tier_id <- function(wc) {
    if (wc >= 20) return(4)
    else if (wc >= 15) return(3)
    else if (wc >= 5)  return(2)
    else if (wc >= 2)  return(1)
    else return(NA)
  }
  
  weapon_counts$tier_id <- sapply(weapon_counts$weapon_count, assign_tier_id)
  achieved_idx <- !is.na(weapon_counts$tier_id)
  
  if (!any(achieved_idx)) {
    return(data.frame(
      fighter_id = integer(0), tier_id = integer(0), achieved = logical(0),
      percentile = numeric(0), achievement_tier = character(0),
      achievement_name = character(0), achievement_description = character(0),
      achievement_icon = character(0), stringsAsFactors = FALSE
    ))
  }
  
  achievers <- weapon_counts[achieved_idx, , drop=FALSE]
  
  # If a fighter somehow appears multiple times (shouldn't), select highest tier
  achievers <- achievers %>%
    dplyr::group_by(fighter_id) %>%
    dplyr::slice_max(tier_id, with_ties = FALSE) %>%
    dplyr::ungroup()
  
  total_fighters <- length(unique(data$fighter_id))
  percentile <- nrow(achievers)/total_fighters
  
  achievements <- dplyr::left_join(achievers, tiers, by = "tier_id")
  
  # Add achieved=TRUE and dynamic description
  # Incorporate actual weapon_count in the description
  achievements <- achievements %>%
    dplyr::mutate(
      achieved = TRUE,
      percentile = percentile,
      achievement_description = paste0("You fought in ", weapon_count, " HEMA weapon categories!"),
      achievement_icon = dplyr::case_when(
        achievement_tier == "Epic" ~ "iron_maiden_epic.png",
        achievement_tier == "Gold" ~ "iron_maiden_gold.png",
        achievement_tier == "Silver" ~ "iron_maiden_silver.png",
        achievement_tier == "Bronze" ~ "iron_maiden_bronze.png"
      )
    ) %>%
    dplyr::select(
      fighter_id, tier_id, achieved, percentile,
      achievement_tier, achievement_name,
      achievement_description, achievement_icon
    )
  
  return(achievements)
}