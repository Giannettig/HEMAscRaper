#' Tournaments Fought Achievement
#'
#' @description
#' Internal function to award achievements based on the number of tournaments a fighter has participated in.
#'
#' Tiers:
#' - Epic (tier_id=4): Fought in more than 50 tournaments
#' - Gold (tier_id=3): Fought in 20 or more tournaments
#' - Silver (tier_id=2): Fought in 10 or more tournaments
#' - Bronze (tier_id=1): Fought in 5 or more tournaments
#'
#' The achievement description dynamically includes the number of tournaments fought.
#'
#' @param data A data frame with at least the following columns:
#' \describe{
#'   \item{fighter_id}{Identifier of the fighter}
#'   \item{tournament_id}{Identifier of the tournament}
#' }
#'
#' @return A data frame with columns:
#' \describe{
#'   \item{fighter_id}{ID of the fighter}
#'   \item{tier_id}{Numeric tier ID (1-4)}
#'   \item{achieved}{Logical, TRUE if achieved}
#'   \item{percentile}{Proportion of fighters who achieved this tier or higher}
#'   \item{achievement_tier}{"Bronze", "Silver", "Gold", or "Epic"}
#'   \item{achievement_name}{"Tournament Warrior - <Tier>"}
#'   \item{achievement_description}{Includes the actual number of tournaments fought}
#'   \item{achievement_icon}{e.g., "tournament_bronze.png"}
#' }
#'
#' @importFrom dplyr group_by summarize mutate filter ungroup left_join select arrange
#' @importFrom tibble tribble
#' @importFrom stringr str_replace_all
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
        tournament_count > 50 ~ "Epic",
        tournament_count >= 20 ~ "Gold",
        tournament_count >= 10 ~ "Silver",
        tournament_count >= 5 ~ "Bronze",
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::filter(!is.na(achievement_tier))
  
  # Calculate percentiles
  total_fighters <- dplyr::n_distinct(data$fighter_id)
  tournament_counts <- tournament_counts %>%
    dplyr::group_by(achievement_tier) %>%
    dplyr::mutate(
      percentile = n() / total_fighters
    ) %>%
    dplyr::ungroup()
  
  # Join tier details and create descriptions
  achievements <- tournament_counts %>%
    dplyr::left_join(tier_details, by = "achievement_tier") %>%
    dplyr::mutate(
      achievement_name = achievement_name_template,
      achievement_description = stringr::str_replace_all(achievement_description_template, "\\{tournament_count\\}", as.character(tournament_count))
    ) %>%
    dplyr::mutate(
      tier_id = dplyr::case_when(
        achievement_tier == "Bronze" ~ 1,
        achievement_tier == "Silver" ~ 2,
        achievement_tier == "Gold" ~ 3,
        achievement_tier == "Epic" ~ 4,
        TRUE ~ NA_integer_
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
