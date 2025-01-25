#' Veni Vidi Vici Achievement
#'
#' @description
#' Awards achievements to fighters who win a tournament final in their very first
#' tournament.
#'
#' @details
#' This is a unique tier achievement awarded to fighters who achieve victory in a
#' tournament final during their first-ever tournament appearance.
#'
#' @param data A data frame containing HEMA tournament match data
#'
#' @return A data frame of achievements with columns:
#' \itemize{
#'   \item fighter_id: Unique fighter identifier
#'   \item tier_id: Achievement tier level (always 1)
#'   \item achieved: Logical indicating if achievement earned
#'   \item percentile: Fighter's percentile for this achievement
#'   \item achievement_tier: Text description of tier (always "Unique")
#'   \item achievement_name: Name of the achievement
#'   \item achievement_description: Description of what was achieved
#'   \item achievement_icon: Icon file name for the achievement
#' }
#'
#' @examples
#' \dontrun{
#' achievements <- ach_veni_vidi_vici(tournament_data)
#' }
#'
#' @keywords internal
ach_veni_vidi_vici <- function(data) {
  # Define tiers
  tiers <- tibble::tribble(
    ~achievement_tier, ~tier_id, ~achievement_name, ~achievement_description_template, ~achievement_icon,
    "Unique",           1,        "Veni Vidi Vici", "You won a final match in your very first tournament ({tournament_name}).", "veni_vidi_vici.png"
  )
  
  # Identify final winners in their very first tournament
  final_winners <- data %>%
    # Step 1: Identify fighters with a WIN result in a top-level final
    dplyr::filter(
      is_final == TRUE, # Matches "final" stage based on your pattern
      result == "WIN"   # Only consider winners
    ) %>%
    # Step 2: Find the earliest tournament date for each fighter
    dplyr::group_by(fighter_id) %>%
    dplyr::mutate(first_tournament_date = min(event_date, na.rm = TRUE)) %>%
    # Step 3: Filter where the event_date matches the fighter's earliest tournament date
    dplyr::filter(event_date == first_tournament_date) %>%
    # Step 4: Select only the earliest win if there are ties on the event_date
    dplyr::slice_min(order_by = event_date, with_ties = FALSE) %>%
    dplyr::ungroup()
  
  # Compute total fighters
  total_fighters <- dplyr::n_distinct(data$fighter_id)
  
  # Ensure there are winners; otherwise, return an empty result
  if (nrow(final_winners) == 0) {
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
  
  # Calculate percentile for each fighter
  final_winners <- final_winners %>%
    dplyr::mutate(percentile = (nrow(final_winners) / total_fighters) * 100)
  
  # Prepare achievements data frame
  achievements <- final_winners %>%
    dplyr::mutate(
      tier_id = 1,
      achieved = TRUE,
      achievement_tier = "Unique",
      achievement_name = "Veni Vidi Vici",
      achievement_description = stringr::str_replace_all(
        tiers$achievement_description_template[1],
        "\\{tournament_name\\}",
        as.character(tournament_name)
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
  
  # Ensure consistency with tiers
  achievements <- dplyr::left_join(
    achievements, 
    tiers %>% dplyr::select(-achievement_description_template), 
    by = c("achievement_tier", "tier_id", "achievement_name", "achievement_icon")
  )
  
  return(achievements)
}