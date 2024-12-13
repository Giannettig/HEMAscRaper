#' Tryhard Achievement
#'
#' @description
#' Internal function to award the "Tryhard" achievement based on the number of matches fought in a year
#' with a win rate under 30%. Tiers are assigned as follows:
#' - Bronze (tier_id=1): Fought in 100 or more matches
#' - Silver (tier_id=2): Fought in 200 or more matches
#' - Gold (tier_id=3): Fought in 300 or more matches
#' - Epic (tier_id=4): Fought the most matches in a year among those meeting the criteria
#'
#' @param data A data frame with at least the following columns:
#' \describe{
#'   \item{fighter_id}{Identifier of the fighter}
#'   \item{event_year}{Year of the event}
#'   \item{match_id}{Identifier of the match}
#'   \item{result}{Match result, e.g., "WIN", "LOSS"}
#' }
#'
#' @return A data frame with columns:
#' \describe{
#'   \item{fighter_id}{ID of the fighter}
#'   \item{tier_id}{Numeric tier ID (1-4)}
#'   \item{achieved}{Logical, TRUE if achieved}
#'   \item{percentile}{Proportion of fighters who achieved this tier or higher in that year}
#'   \item{achievement_tier}{"Bronze", "Silver", "Gold", or "Epic"}
#'   \item{achievement_name}{"Tryhard - <Tier>"}
#'   \item{achievement_description}{Includes the actual number of matches fought, win rate, and year}
#'   \item{achievement_icon}{e.g., "tryhard_bronze.png"}
#' }
#'
#' @importFrom dplyr group_by summarize mutate filter ungroup left_join select arrange slice_max
#' @importFrom tibble tribble
#' @importFrom stringr str_replace_all str_squish
#' @keywords internal
ach_tryhard <- function(data) {
  
  # Define tier details
  tier_details <- tibble::tribble(
    ~achievement_tier, ~achievement_name_template, ~achievement_description_template,                                         ~achievement_icon,
    "Bronze",          "Tryhard - Bronze",        "You fought {match_count} matches with a win rate of {win_rate}% in {event_year}!", "tryhard_bronze.png",
    "Silver",          "Tryhard - Silver",        "You fought {match_count} matches with a win rate of {win_rate}% in {event_year}!", "tryhard_silver.png",
    "Gold",            "Tryhard - Gold",          "You fought {match_count} matches with a win rate of {win_rate}% in {event_year}!", "tryhard_gold.png",
    "Epic",            "Tryhard - Epic",          "You fought the most matches with a win rate of {win_rate}% in {event_year}!",    "tryhard_epic.png"
  )
  
  # Calculate match_count and win_rate per fighter per year
  yearly_stats <- data %>%
    dplyr::group_by(fighter_id, event_year) %>%
    dplyr::summarize(
      match_count = dplyr::n_distinct(match_id),
      win_rate = round(mean(result == "WIN", na.rm = TRUE) * 100, 1), # Calculate win rate as a percentage
      .groups = "drop"
    ) %>%
    dplyr::filter(match_count >= 100, win_rate < 30)
  
  # Assign Epic tier: fighters with the most matches in their year
  epic_tiers <- yearly_stats %>%
    dplyr::group_by(event_year) %>%
    dplyr::slice_max(match_count, with_ties = TRUE) %>%
    dplyr::mutate(
      achievement_tier = "Epic"
    ) %>%
    dplyr::ungroup()
  
  # Assign Bronze, Silver, Gold tiers
  other_tiers <- yearly_stats %>%
    dplyr::anti_join(epic_tiers, by = c("fighter_id", "event_year")) %>%
    dplyr::mutate(
      achievement_tier = dplyr::case_when(
        match_count >= 300 ~ "Gold",
        match_count >= 200 ~ "Silver",
        match_count >= 100 ~ "Bronze",
        TRUE               ~ NA_character_
      )
    ) %>%
    dplyr::filter(!is.na(achievement_tier))
  
  # Combine Epic and other tiers
  combined_achievements <- dplyr::bind_rows(epic_tiers, other_tiers)
  
  # Total fighters per event_year for percentile calculation
  total_fighters_year <- data %>%
    dplyr::group_by(event_year) %>%
    dplyr::summarize(total_fighters = dplyr::n_distinct(fighter_id), .groups = "drop")
  
  # Join tier details and create descriptions
  achievements <- combined_achievements %>%
    dplyr::left_join(tier_details, by = "achievement_tier") %>%
    dplyr::left_join(total_fighters_year, by = "event_year") %>%
    dplyr::mutate(
      achievement_name = stringr::str_replace_all(achievement_name_template, "\\{Tier\\}", achievement_tier),
      achievement_description = stringr::str_replace_all(
        stringr::str_replace_all(stringr::str_replace_all(achievement_description_template, "\\{match_count\\}", as.character(match_count)), 
                                 "\\{win_rate\\}", as.character(win_rate)),
        "\\{event_year\\}", as.character(event_year)
      ),
      percentile = dplyr::case_when(
        achievement_tier == "Epic" ~ 1 / total_fighters,
        TRUE ~ match_count / total_fighters
      ),
      tier_id = dplyr::case_when(
        achievement_tier == "Bronze" ~ 1,
        achievement_tier == "Silver" ~ 2,
        achievement_tier == "Gold"   ~ 3,
        achievement_tier == "Epic"   ~ 4,
        TRUE                         ~ NA_integer_
      ),
      achieved = TRUE,
      achievement_icon = stringr::str_replace_all(achievement_icon, "\\{tier\\}", tolower(achievement_tier))
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