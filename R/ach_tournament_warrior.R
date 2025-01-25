#' Tournament Warrior Achievement
#'
#' @description
#' Awards achievements to fighters based on the total number of tournaments
#' they've participated in.
#'
#' @details
#' Tiers are awarded based on number of tournaments:
#' - Epic (4): 50+ tournaments
#' - Gold (3): 20+ tournaments
#' - Silver (2): 10+ tournaments
#' - Bronze (1): 5+ tournaments
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
#' achievements <- ach_tournament_warrior(tournament_data)
#' }
#'
#' @keywords internal
ach_tournament_warrior <- function(data) {
  
  # Define tiers
  tier_details <- tibble::tribble(
    ~achievement_tier, ~achievement_name_template,                 ~achievement_description_template,                       ~achievement_icon,
    "Bronze",          "Tournament Warrior - Bronze",             "You fought in {tournament_count} tournaments!",        "tournament_bronze.png",
    "Silver",          "Tournament Warrior - Silver",             "You fought in {tournament_count} tournaments!",        "tournament_silver.png",
    "Gold",            "Tournament Warrior - Gold",               "You fought in {tournament_count} tournaments!",        "tournament_gold.png",
    "Epic",            "Tournament Warrior - Epic",               "You fought in {tournament_count} tournaments!",        "tournament_epic.png"
  )
  
  # Calculate the number of tournaments per fighter
  tournament_counts <- data %>%
    dplyr::group_by(fighter_id) %>%
    dplyr::summarize(tournament_count = dplyr::n_distinct(tournament_id), .groups = "drop")
  
  # Assign tiers based on tournament_count
  tournament_counts <- tournament_counts %>%
    dplyr::mutate(
      achievement_tier = dplyr::case_when(
        tournament_count > 50  ~ "Epic",
        tournament_count >= 20 ~ "Gold",
        tournament_count >= 10 ~ "Silver",
        tournament_count >= 5  ~ "Bronze",
        TRUE                   ~ NA_character_
      )
    ) %>%
    dplyr::filter(!is.na(achievement_tier))
  
  # Total fighters for percentile calculation
  total_fighters <- dplyr::n_distinct(data$fighter_id)
  
  # Calculate cumulative counts for percentiles
  tier_counts <- tournament_counts %>%
    dplyr::group_by(achievement_tier) %>%
    dplyr::summarize(tier_count = dplyr::n(), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(achievement_tier)) %>%
    dplyr::mutate(cumulative_count = cumsum(tier_count))
  
  # Join cumulative counts back to tournament_counts
  tournament_counts <- tournament_counts %>%
    dplyr::left_join(tier_counts, by = "achievement_tier") %>%
    dplyr::mutate(
      percentile = (cumulative_count / total_fighters) * 100 # Percent of fighters with the achievement
    )
  
  # Join tier details and create descriptions
  achievements <- tournament_counts %>%
    dplyr::left_join(tier_details, by = "achievement_tier") %>%
    dplyr::mutate(
      achievement_name = achievement_name_template,
      achievement_description = stringr::str_replace_all(
        achievement_description_template,
        "\\{tournament_count\\}", as.character(tournament_count)
      )
    ) %>%
    dplyr::mutate(
      tier_id = dplyr::case_when(
        achievement_tier == "Bronze" ~ 1,
        achievement_tier == "Silver" ~ 2,
        achievement_tier == "Gold"   ~ 3,
        achievement_tier == "Epic"   ~ 4,
        TRUE                         ~ NA_integer_
      ),
      achieved = TRUE
    ) %>%
    # Select the required columns in the correct order
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