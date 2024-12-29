#' Veni Vidi Vici Achievement
#'
#' @description
#' Internal function to award the "Veni Vidi Vici" achievement to fighters who won a final match
#' in their very first tournament.
#'
#' Tiers:
#' - Unique (tier_id=1): Successfully won a final match in the very first tournament.
#'
#' The achievement description dynamically includes the name of the tournament where the fighter achieved this.
#'
#' @param data A data frame with at least the following columns:
#' \describe{
#'   \item{fighter_id}{Identifier of the fighter}
#'   \item{match_id}{Identifier of the match}
#'   \item{event_date}{Date of the event (as Date or character)}
#'   \item{result}{Result of the match ("WIN" or "LOSS")}
#'   \item{tournament_name}{Name of the tournament}
#' }
#'
#' @return A data frame with columns:
#' \describe{
#'   \item{fighter_id}{ID of the fighter}
#'   \item{tier_id}{Numeric tier ID (1)}
#'   \item{achieved}{Logical, TRUE if achieved}
#'   \item{percentile}{Proportion of fighters who achieved this tier}
#'   \item{achievement_tier}{"Unique"}
#'   \item{achievement_name}{"Veni Vidi Vici"}
#'   \item{achievement_description}{Includes the name of the first tournament where the achievement was made}
#'   \item{achievement_icon}{"veni_vidi_vici.png"}
#' }
#'
#' @importFrom dplyr filter group_by slice_min summarize mutate left_join select
#' @importFrom tibble tribble
#' @importFrom stringr str_detect
#' @keywords internal
ach_veni_vidi_vici <- function(data) {
  
  # Define tiers
  tiers <- tibble::tribble(
    ~achievement_tier, ~tier_id, ~achievement_name, ~achievement_description, ~achievement_icon,
    "Unique",           1,        "Veni Vidi Vici", "You won a final match in your very first tournament.", "veni_vidi_vici.png"
  )
  
 
  # Identify final winners in their very first tournament
  final_winners <- data %>%
    # Step 1: Identify fighters with a WIN result in a top-level final
    dplyr::filter(
      .data$is_final==TRUE, # Matches "final" stage based on your pattern
      result == "WIN"                           # Only consider winners
    ) %>%
    # Step 2: Find the earliest tournament date for each fighter
    dplyr::group_by(fighter_id) %>%
    dplyr::mutate(first_tournament_date = min(event_date)) %>%
    # Step 3: Filter where the event_date matches the fighter's earliest tournament date
    dplyr::filter(event_date == .data$first_tournament_date) %>%
    # Step 4: Select only the earliest win if there are ties on the event_date
    dplyr::slice_min(order_by = event_date, with_ties = FALSE) %>%
    dplyr::ungroup()
  # Compute total fighters
  total_fighters <- dplyr::n_distinct(data$fighter_id)
  
  # Calculate percentile
  percentile <- nrow(final_winners) / total_fighters
  
  # Prepare achievements data frame
  achievements <- final_winners %>%
    dplyr::mutate(
      tier_id = 1,
      achieved = TRUE,
      percentile = percentile,
      achievement_tier = "Unique",
      achievement_name = "Veni Vidi Vici",
      achievement_description = paste0(
        "You won a final match in your very first tournament in Hema Ratings (",
        tournament_name, ")."
      ),
      achievement_icon = "veni_vidi_vici.png"
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
  
  # Join with tiers to ensure consistency
  achievements <- dplyr::left_join(achievements, tiers, by = c("achievement_tier", "tier_id", "achievement_name", "achievement_description", "achievement_icon"))
  
  return(achievements)
}