#' The Revenant Achievement
#'
#' @description
#' Awards achievements to fighters who return to competition after long periods
#' of inactivity.
#'
#' @details
#' Tiers are awarded based on years of inactivity before return:
#' - Epic (4): 10+ years
#' - Gold (3): 7+ years
#' - Silver (2): 5+ years
#' - Bronze (1): 2+ years
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
#' achievements <- ach_revenant(tournament_data)
#' }
#'
#' @keywords internal
ach_revenant <- function(data) {
  # Define all tiers and their conditions
  tiers <- tibble::tribble(
    ~achievement_tier, ~tier_id, ~achievement_name,       ~achievement_description_template,                       ~achievement_icon,
    "Bronze",          1,        "The Revenant",         "You came back to fight after {interval_years} years of inactivity!", "the_revenant_bronze.png",
    "Silver",          2,        "The Revenant",         "You came back to fight after {interval_years} years of inactivity!", "the_revenant_silver.png",
    "Gold",            3,        "The Revenant",         "You came back to fight after {interval_years} years of inactivity!", "the_revenant_gold.png",
    "Epic",            4,        "The Revenant",         "You came back to fight after {interval_years} years of inactivity!", "the_revenant_epic.png"
  )
  
  # Total unique fighters for percentile calculation
  total_fighters <- data %>% dplyr::distinct(fighter_id) %>% nrow()
  
  # Calculate intervals between fights in years
  ach <- data %>%
    dplyr::select(fighter_id, event_date) %>%
    dplyr::distinct() %>%
    dplyr::arrange(fighter_id, event_date) %>%
    dplyr::group_by(fighter_id) %>%
    dplyr::mutate(
      interval_between_fights = as.numeric(event_date - dplyr::lag(event_date), units = "days") / 365
    ) %>%
    dplyr::filter(!is.na(interval_between_fights)) %>%
    dplyr::summarize(max_interval = max(interval_between_fights, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(
      tier_id = dplyr::case_when(
        max_interval >= 10 ~ 4,  # Epic tier (10+ years)
        max_interval >= 7  ~ 3,  # Gold tier (7+ years)
        max_interval >= 5  ~ 2,  # Silver tier (5+ years)
        max_interval >= 2  ~ 1,  # Bronze tier (2+ years)
        TRUE               ~ NA_integer_
      ),
      achieved = !is.na(tier_id)
    ) %>%
    dplyr::filter(achieved)
  
  # Handle case where no achievements are found
  if (nrow(ach) == 0) {
    return(tibble::tibble(
      fighter_id = integer(0),
      tier_id = integer(0),
      achieved = logical(0),
      percentile = numeric(0),
      achievement_tier = character(0),
      achievement_name = character(0),
      achievement_description = character(0),
      achievement_icon = character(0)
    ))
  }
  
  # Calculate cumulative counts for percentiles
  tier_counts <- ach %>%
    dplyr::group_by(tier_id) %>%
    dplyr::summarize(tier_count = dplyr::n(), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(tier_id)) %>%
    dplyr::mutate(cumulative_count = cumsum(tier_count))
  
  # Join cumulative counts back to achievements
  ach <- ach %>%
    dplyr::left_join(tier_counts, by = "tier_id") %>%
    dplyr::mutate(
      percentile = (cumulative_count / total_fighters) * 100 # Percent of fighters with the achievement
    )
  
  # Join tier details and create descriptions
  achievements <- ach %>%
    dplyr::left_join(tiers, by = "tier_id") %>%
    dplyr::mutate(
      achievement_description = stringr::str_replace_all(
        achievement_description_template, 
        "\\{interval_years\\}", 
        as.character(round(max_interval, 1))
      )
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