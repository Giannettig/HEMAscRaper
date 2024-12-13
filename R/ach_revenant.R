
#' The Revenant Achievement
#'
#' @description
#' Internal function to award the "The Revenant" achievement based on returning to fight after periods of inactivity.
#'
#' Tiers:
#' - Epic (tier_id=4): Returned after at least ten years of inactivity
#' - Gold (tier_id=3): Returned after seven years of inactivity
#' - Silver (tier_id=2): Returned after five years of inactivity
#' - Bronze (tier_id=1): Returned after at least two years of inactivity
#'
#' The achievement description dynamically includes the actual interval of inactivity.
#'
#' @param data A data frame with at least the following columns:
#' \describe{
#'   \item{fighter_id}{Identifier of the fighter}
#'   \item{match_id}{Identifier of the match}
#'   \item{event_date}{Date of the event (as Date or character)}
#' }
#'
#' @return A data frame with columns:
#' \describe{
#'   \item{fighter_id}{ID of the fighter}
#'   \item{tier_id}{Numeric tier ID (1-4)}
#'   \item{achieved}{Logical, TRUE if achieved}
#'   \item{percentile}{Proportion of fighters who achieved this tier or higher}
#'   \item{achievement_tier}{"Bronze", "Silver", "Gold", or "Epic"}
#'   \item{achievement_name}{"The Revenant"}
#'   \item{achievement_description}{Includes the actual interval of inactivity in years}
#'   \item{achievement_icon}{e.g., "the_revenant_bronze.png"}
#' }
#'
#' @importFrom dplyr group_by summarize mutate filter ungroup left_join slice_max select arrange
#' @importFrom tibble tribble
#' @importFrom stringr str_replace
#' @keywords internal
ach_revenant <- function(data) {
  # Define all tiers and their conditions
  tiers <- tribble(
    ~achievement_tier, ~tier_id, ~achievement_name,       ~achievement_description,                                             ~achievement_icon,
    "Bronze",           1,        "The Revenant",       "You come back to fight after at least two years of inactivity",       "bronze_revenant.png",
    "Epic",             4,        "The Revenant",       "You come back to fight after ten years of inactivity",                "epic_revenant.png"
  )
  
  total_fighters <- data %>% distinct(fighter_id) %>% nrow() # Total unique fighters
  
  ach <- data %>%
    select(fighter_id, event_date) %>%
    distinct() %>%
    arrange(fighter_id, event_date) %>%
    group_by(fighter_id) %>%
    mutate(interval_between_fights = as.numeric(event_date - lag(event_date), units = "days")) %>%
    filter(!is.na(interval_between_fights)) %>%
    summarize(max_interval = max(interval_between_fights, na.rm = TRUE)) %>%
    mutate(
      tier_id = case_when(
        max_interval >= 3650 ~ 4,  # Epic tier (10 years)
        max_interval >= 730 ~ 1,   # Bronze tier (2 years)
        TRUE ~ NA_integer_
      ),
      achieved = !is.na(tier_id)
    ) %>%
    filter(achieved) %>%
    mutate(percentile = n() / total_fighters) %>% # Percentile calculation
    left_join(tiers, by = "tier_id")%>%select(-max_interval)
  
  return(ach)
}
