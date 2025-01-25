#' Tryhard Achievement
#'
#' @description
#' Awards achievements to fighters who participate in a high number of matches
#' while maintaining a low win rate (below 30%) in a given year.
#'
#' @details
#' Tiers are awarded based on number of matches (minimum 100 matches required):
#' - Epic (4): Most matches in the year
#' - Gold (3): 300+ matches
#' - Silver (2): 200+ matches
#' - Bronze (1): 100+ matches
#'
#' All tiers require a win rate below 30%.
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
#' achievements <- ach_tryhard(tournament_data)
#' }
#'
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
    dplyr::filter(match_count >= 100, win_rate < 30) # Minimum match count and win rate threshold
  
  # Assign Epic tier: fighters with the most matches in their year
  epic_tiers <- yearly_stats %>%
    dplyr::group_by(event_year) %>%
    dplyr::slice_max(match_count, with_ties = TRUE) %>%
    dplyr::mutate(achievement_tier = "Epic") %>%
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
  
  # Calculate the number of fighters who achieved each tier or higher in each year
  tier_counts <- combined_achievements %>%
    dplyr::group_by(event_year, achievement_tier) %>%
    dplyr::summarize(tier_count = dplyr::n(), .groups = "drop") %>%
    dplyr::arrange(event_year, dplyr::desc(achievement_tier)) %>%
    dplyr::group_by(event_year) %>%
    dplyr::mutate(cumulative_count = cumsum(tier_count)) %>%
    dplyr::ungroup()
  
  # Join tier details and create descriptions
  achievements <- combined_achievements %>%
    dplyr::left_join(tier_details, by = "achievement_tier") %>%
    dplyr::left_join(total_fighters_year, by = "event_year") %>%
    dplyr::left_join(tier_counts, by = c("event_year", "achievement_tier")) %>%
    dplyr::mutate(
      achievement_name = achievement_name_template,
      achievement_description = stringr::str_replace_all(
        stringr::str_replace_all(
          stringr::str_replace_all(achievement_description_template, "\\{match_count\\}", as.character(match_count)),
          "\\{win_rate\\}", as.character(win_rate)
        ),
        "\\{event_year\\}", as.character(event_year)
      ),
      percentile = (cumulative_count / total_fighters) * 100, # Percent of fighters with the achievement
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