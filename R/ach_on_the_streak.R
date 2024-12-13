#' On the Streak Achievement
#'
#' @description
#' Internal function to award the "On the Streak!" achievement based on the longest uninterrupted winning streak.
#'
#' Tiers:
#' - Epic (tier_id=4): ≥50 victories
#' - Gold (tier_id=3): ≥20 victories
#' - Silver (tier_id=2): ≥10 victories
#' - Bronze (tier_id=1): ≥5 victories
#'
#' The achievement description dynamically includes the fighter's actual maximum streak.
#'
#' @param data A data frame with at least the following columns:
#' \describe{
#'   \item{fighter_id}{Identifier of the fighter}
#'   \item{match_id}{Identifier of the match}
#'   \item{event_date}{Date of the event (as Date or character)}
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
#'   \item{achievement_name}{"On the Streak!"}
#'   \item{achievement_description}{Includes the actual number of victories in the streak}
#'   \item{achievement_icon}{e.g., "on_the_streak_bronze.png"}
#' }
#'
#' @importFrom dplyr group_by summarize mutate filter ungroup left_join slice_max select arrange
#' @importFrom tibble tribble
#' @importFrom stringr str_replace
#' @keywords internal
ach_on_the_streak <- function(data) {
  
  # Define tiers
  tiers <- tibble::tribble(
    ~achievement_tier, ~tier_id, ~achievement_name, ~achievement_description,                           ~achievement_icon,
    "Epic",            4,        "On the Streak!", "You have an uninterrupted streak of XXX victories!", "on_the_streak_epic.png",
    "Gold",            3,        "On the Streak!", "You have an uninterrupted streak of XXX victories!", "on_the_streak_gold.png",
    "Silver",          2,        "On the Streak!", "You have an uninterrupted streak of XXX victories!", "on_the_streak_silver.png",
    "Bronze",          1,        "On the Streak!", "You have an uninterrupted streak of XXX victories!", "on_the_streak_bronze.png"
  )
  
  # Ensure event_date is of Date type
  if (!inherits(data$event_date, "Date")) {
    data$event_date <- as.Date(data$event_date)
  }
  
  # Arrange data by fighter_id and event_date
  data_sorted <- data %>%
    dplyr::arrange(fighter_id, event_date, match_id)
  
  # Compute maximum winning streak per fighter
  max_streaks <- data_sorted %>%
    dplyr::group_by(fighter_id) %>%
    dplyr::summarize(
      max_streak = {
        # Use rle to find runs of "WIN"
        rle_results <- rle(result == "WIN")
        if (any(rle_results$values)) {
          max(rle_results$lengths[rle_results$values], na.rm = TRUE)
        } else {
          0
        }
      },
      .groups = "drop"
    )
  
  # Assign tiers based on max_streak
  max_streaks <- max_streaks %>%
    dplyr::mutate(
      tier_id = dplyr::case_when(
        max_streak >= 50 ~ 4,  # Epic
        max_streak >= 20 ~ 3,  # Gold
        max_streak >= 10 ~ 2,  # Silver
        max_streak >= 5  ~ 1,  # Bronze
        TRUE            ~ NA_integer_
      )
    ) %>%
    dplyr::filter(!is.na(tier_id))
  
  # If no achievements, return empty data frame
  if (nrow(max_streaks) == 0) {
    return(data.frame(
      fighter_id = integer(0), tier_id = integer(0), achieved = logical(0),
      percentile = numeric(0), achievement_tier = character(0),
      achievement_name = character(0), achievement_description = character(0),
      achievement_icon = character(0), stringsAsFactors = FALSE
    ))
  }
  
  # Calculate percentile
  total_fighters <- length(unique(data$fighter_id))
  max_streaks <- max_streaks %>%
    dplyr::mutate(
      achieved = TRUE,
      percentile = 1 / total_fighters  # Since each fighter can only have one max_streak
    )
  
  # Join with tiers to get achievement details
  achievements <- dplyr::left_join(max_streaks, tiers, by = "tier_id") %>%
    dplyr::mutate(
      achievement_description = stringr::str_replace(achievement_description, "XXX", as.character(max_streak)),
      achievement_icon = stringr::str_replace(achievement_icon, "XXX", as.character(tier_id))  # Not strictly necessary here
    ) 
  
  # Replace "XXX" with actual max_streak in description
  achievements <- achievements %>%
    dplyr::mutate(
      achievement_description = paste0("You have an uninterrupted streak of ", max_streak, " victories!"),
      achievement_icon = paste0("on_the_streak_", tolower(achievement_tier), ".png")
    )%>%
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