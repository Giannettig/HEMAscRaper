#' Pig Slayer Achievement
#'
#' @description
#' Internal function to award the "Pig Slayer!" achievement based on victories against Alexander Stankievich.
#'
#' Tiers:
#' - Epic (tier_id=4): ≥5 victories and no losses
#' - Gold (tier_id=3): ≥5 victories
#' - Silver (tier_id=2): ≥3 victories
#' - Bronze (tier_id=1): ≥1 victory
#'
#' The achievement description dynamically includes the number of victories.
#'
#' @param data A data frame with at least the following columns:
#' \describe{
#'   \item{fighter_id}{Identifier of the fighter}
#'   \item{match_id}{Identifier of the match}
#'   \item{opponent_id}{Identifier of the opponent}
#'   \item{result}{Result of the match ("WIN" or "LOSS")}
#' }
#'
#' @return A data frame with columns:
#' \describe{
#'   \item{fighter_id}{ID of the fighter}
#'   \item{tier_id}{Numeric tier ID (1-4)}
#'   \item{achieved}{Logical, TRUE if achieved}
#'   \item{percentile}{Proportion of fighters who achieved this tier or higher}
#'   \item{achievement_tier}{"Bronze", "Silver", "Gold", or "Epic"}
#'   \item{achievement_name}{"Pig Slayer!"}
#'   \item{achievement_description}{Includes the actual number of victories}
#'   \item{achievement_icon}{e.g., "pig_slayer_bronze.png"}
#' }
#'
#' @importFrom dplyr group_by summarize mutate filter ungroup left_join slice_max select
#' @importFrom tibble tribble
#' @keywords internal
ach_pig_slayer <- function(data) {
  # Define tiers
  tiers <- tibble::tribble(
    ~achievement_tier, ~tier_id, ~achievement_name,    ~achievement_description,                                            ~achievement_icon,
    "Epic",            4,        "Pig Slayer!",       "You fought Alexander Stankievich at least 5 times and never lost.", "pig_slayer_epic.png",
    "Gold",            3,        "Pig Slayer!",       "You fought Alexander Stankievich at least 5 times.",               "pig_slayer_gold.png",
    "Silver",          2,        "Pig Slayer!",       "You fought Alexander Stankievich at least 3 times.",               "pig_slayer_silver.png",
    "Bronze",          1,        "Pig Slayer!",       "You defeated Alexander Stankievich in a match.",                   "pig_slayer_bronze.png"
  )
  
  # Summarize victories and losses against opponent_id = 152 (Alexander Stankievich)
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
    dplyr::filter(!is.na(tier_id))
  
  # Handle case where no achievements exist
  if (nrow(achievements) == 0) {
    return(data.frame(
      fighter_id = double(0), tier_id = double(0), achieved = logical(0),
      percentile = numeric(0), achievement_tier = character(0),
      achievement_name = character(0), achievement_description = character(0),
      achievement_icon = character(0), stringsAsFactors = FALSE
    ))
  }
  
  # Select highest tier per fighter
  achievements <- achievements %>%
    dplyr::group_by(fighter_id) %>%
    dplyr::slice_max(tier_id, with_ties = FALSE) %>%
    dplyr::ungroup()
  
  # Calculate percentile
  total_fighters <- length(unique(data$fighter_id))
  achievements <- achievements %>%
    dplyr::mutate(
      achieved = TRUE,
      percentile = 1 / total_fighters
    )
  
  # Join with tier details
  achievements <- dplyr::left_join(achievements, tiers, by = "tier_id")
  
  # Create dynamic achievement descriptions and correct icon filenames
  achievements <- achievements %>%
    dplyr::mutate(
      achievement_description = dplyr::case_when(
        tier_id == 4 ~ paste0("You fought Alexander Stankievich ", wins, " times and never lost."),
        tier_id == 3 ~ paste0("You fought Alexander Stankievich ", wins, " times."),
        tier_id == 2 ~ paste0("You fought Alexander Stankievich ", wins, " times."),
        tier_id == 1 ~ "You defeated Alexander Stankievich in a match."
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
    ) %>%
    dplyr::mutate(
      fighter_id = as.double(fighter_id),  # Ensure correct type
      tier_id = as.double(tier_id)        # Ensure correct type
    )
  
  return(achievements)
}