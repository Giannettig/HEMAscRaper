#' Epiphany of Steel Achievement
#'
#' @description
#' Internal function to award "Epiphany of Steel" based on how many different main steel HEMA weapons
#' a fighter has fought with. Tiers are assigned as follows:
#' - Epic: fought with all 7
#' - Gold: fought with 6
#' - Silver: fought with 5
#' - Bronze: fought with 3
#'
#' The achievement description dynamically includes the actual number of weapons the fighter used.
#'
#' @param data A data frame with at least `fighter_id` and `tournament_weapon` columns.
#'
#' @return A data frame with columns:
#' \describe{
#'   \item{fighter_id}{ID of the fighter}
#'   \item{tier_id}{Numeric tier ID (1-4)}
#'   \item{achieved}{Logical, TRUE if achieved}
#'   \item{percentile}{Proportion of fighters who achieved this tier or higher}
#'   \item{achievement_tier}{One of "Bronze", "Silver", "Gold", "Epic"}
#'   \item{achievement_name}{"Epiphany of Steel"}
#'   \item{achievement_description}{Dynamically includes weapon_count}
#'   \item{achievement_icon}{e.g. "epiphany_of_steel_bronze.png"}
#' }
#'
#' @importFrom dplyr filter group_by summarize n_distinct ungroup slice_max left_join mutate select
#' @importFrom tibble tribble
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
    ~achievement_tier, ~tier_id, ~achievement_name,         ~achievement_description, ~achievement_icon,
    "Epic",            4,        "Epiphany of Steel",      "",                       "epiphany_of_steel_epic.png",
    "Gold",            3,        "Epiphany of Steel",      "",                       "epiphany_of_steel_gold.png",
    "Silver",          2,        "Epiphany of Steel",      "",                       "epiphany_of_steel_silver.png",
    "Bronze",          1,        "Epiphany of Steel",      "",                       "epiphany_of_steel_bronze.png"
  )
  
  # Compute weapon counts per fighter
  weapon_counts <- data %>%
    dplyr::filter(tournament_weapon %in% main_steel_weapons) %>%
    dplyr::group_by(fighter_id) %>%
    dplyr::summarize(weapon_count = dplyr::n_distinct(tournament_weapon), .groups = "drop")
  
  if (nrow(weapon_counts) == 0) {
    # No one fought with any main steel weapon
    return(data.frame(
      fighter_id = integer(0), tier_id = integer(0), achieved = logical(0),
      percentile = numeric(0), achievement_tier = character(0),
      achievement_name = character(0), achievement_description = character(0),
      achievement_icon = character(0), stringsAsFactors = FALSE
    ))
  }
  
  # Assign tiers based on weapon_count
  assign_tier <- function(wc) {
    if (wc >= 7) return(4)
    else if (wc >= 6) return(3)
    else if (wc >= 5) return(2)
    else if (wc >= 3) return(1)
    else return(NA)
  }
  
  weapon_counts$tier_id <- sapply(weapon_counts$weapon_count, assign_tier)
  achieved_idx <- !is.na(weapon_counts$tier_id)
  
  if (!any(achieved_idx)) {
    # No achievements
    return(data.frame(
      fighter_id = integer(0), tier_id = integer(0), achieved = logical(0),
      percentile = numeric(0), achievement_tier = character(0),
      achievement_name = character(0), achievement_description = character(0),
      achievement_icon = character(0), stringsAsFactors = FALSE
    ))
  }
  
  # Filter achievers
  achievers <- weapon_counts[achieved_idx, , drop=FALSE]
  
  # If a fighter appears multiple times (shouldn't normally), pick highest tier
  # slice_max from dplyr ensures we get the top tier per fighter
  achievers <- achievers %>%
    dplyr::group_by(fighter_id) %>%
    dplyr::slice_max(tier_id, with_ties = FALSE) %>%
    dplyr::ungroup()
  
  total_fighters <- length(unique(data$fighter_id))
  percentile <- nrow(achievers) / total_fighters
  
  # Join tier details
  achievements <- dplyr::left_join(achievers, tiers, by = "tier_id")
  
  achievements <- achievements %>%
    dplyr::mutate(
      achieved = TRUE,
      percentile = percentile,
      achievement_description = paste0(
        "You fought in tournaments with ", weapon_count, " of the 7 main steel HEMA weapons!"
      ),
      achievement_icon = ifelse(
        achievement_tier == "Epic", "epiphany_of_steel_epic.png",
        ifelse(achievement_tier == "Gold", "epiphany_of_steel_gold.png",
               ifelse(achievement_tier == "Silver", "epiphany_of_steel_silver.png", "epiphany_of_steel_bronze.png"))
      )
    ) %>%
    dplyr::select(
      fighter_id, tier_id, achieved, percentile,
      achievement_tier, achievement_name,
      achievement_description, achievement_icon
    )
  
  return(achievements)
}