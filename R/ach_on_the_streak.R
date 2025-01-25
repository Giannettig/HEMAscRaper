#' On The Streak Achievement
#'
#' @description
#' Awards achievements to fighters based on their longest uninterrupted winning streak.
#'
#' @details
#' Tiers are awarded based on consecutive victories:
#' - Epic (4): 50+ consecutive wins
#' - Gold (3): 20+ consecutive wins
#' - Silver (2): 10+ consecutive wins
#' - Bronze (1): 5+ consecutive wins
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
#' achievements <- ach_on_the_streak(tournament_data)
#' }
#'
#' @keywords internal
ach_on_the_streak <- function(data) {
  # Define tiers
  tiers <- tibble::tribble(
    ~achievement_tier, ~tier_id, ~achievement_name, ~achievement_description_template,                       ~achievement_icon,
    "Epic",            4,        "On the Streak!", "You have an uninterrupted streak of {max_streak} victories!", "on_the_streak_epic.png",
    "Gold",            3,        "On the Streak!", "You have an uninterrupted streak of {max_streak} victories!", "on_the_streak_gold.png",
    "Silver",          2,        "On the Streak!", "You have an uninterrupted streak of {max_streak} victories!", "on_the_streak_silver.png",
    "Bronze",          1,        "On the Streak!", "You have an uninterrupted streak of {max_streak} victories!", "on_the_streak_bronze.png"
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
    dplyr::filter(!is.na(tier_id))  # Only fighters with achievements
  
  # Handle case where no achievements exist
  if (nrow(max_streaks) == 0) {
    return(data.frame(
      fighter_id = integer(0), tier_id = integer(0), achieved = logical(0),
      percentile = numeric(0), achievement_tier = character(0),
      achievement_name = character(0), achievement_description = character(0),
      achievement_icon = character(0), stringsAsFactors = FALSE
    ))
  }
  
  # Calculate total fighters and cumulative counts for percentiles
  total_fighters <- dplyr::n_distinct(data$fighter_id)
  tier_counts <- max_streaks %>%
    dplyr::group_by(tier_id) %>%
    dplyr::summarize(tier_count = dplyr::n(), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(tier_id)) %>%
    dplyr::mutate(cumulative_count = cumsum(tier_count))
  
  # Merge percentile and tier details
  achievements <- max_streaks %>%
    dplyr::left_join(tier_counts, by = "tier_id") %>%
    dplyr::left_join(tiers, by = "tier_id") %>%
    dplyr::mutate(
      percentile = (1 - cumulative_count / total_fighters) * 100,  # Correct percentile
      achievement_description = stringr::str_replace(
        achievement_description_template, "\\{max_streak\\}", as.character(max_streak)
      ), achieved = TRUE
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