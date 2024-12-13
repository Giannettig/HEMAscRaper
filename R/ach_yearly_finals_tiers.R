#' Yearly Finals Tiers Achievement
#'
#' @description
#' Awards yearly achievements based on the number of final matches a fighter participates in each year.
#'
#' ## Tiers
#' - **Epic** (tier_id=4): Fought in 10 or more finals in a year.
#' - **Gold** (tier_id=3): Fought in 5 or more finals in a year.
#' - **Silver** (tier_id=2): Fought in 3 or more finals in a year.
#' - **Bronze** (tier_id=1): Fought in at least 1 final in a year.
#'
#' The achievement description dynamically includes the number of finals participated in that year.
#'
#' @param data A data frame containing HEMA event data. Required columns:
#' - `fighter_id`: Unique identifier for each fighter.
#' - `match_id`: Unique identifier for each match.
#' - `event_year`: Year of the event.
#' - `stage`: Stage of the match (e.g., final, gold match).
#'
#' @return A data frame with the following columns:
#' - `fighter_id`: Identifier of the fighter.
#' - `tier_id`: Numeric tier level (1 = Bronze, 2 = Silver, 3 = Gold, 4 = Epic).
#' - `achieved`: Logical, `TRUE` if the fighter achieved this tier.
#' - `percentile`: Proportion of fighters achieving this tier or higher in the dataset.
#' - `achievement_tier`: Achievement tier ("Bronze", "Silver", "Gold", "Epic").
#' - `achievement_name`: Name of the achievement (e.g., "There Can Be Only One 2024 - Gold").
#' - `achievement_description`: Description of the achievement with dynamic details.
#' - `achievement_icon`: Icon file for the tier (e.g., "bronze_medal.png").
#'
#' @importFrom dplyr group_by summarize mutate filter ungroup left_join select arrange
#' @importFrom tibble tribble
#' @importFrom stringr str_detect str_replace_all str_squish
#' @keywords internal
#' @examples
#' # Example usage:
#' # achievements <- ach_yearly_finals_tiers(data)
#' # head(achievements)
ach_yearly_finals_tiers <- function(data) {
  
  # Define tiers
  tier_details <- tibble::tribble(
    ~achievement_tier, ~achievement_name_template,                     ~achievement_description_template,                   ~achievement_icon,
    "Bronze",          "There Can Be Only One {event_year} - Bronze",  "You fought in {final_matches} final(s) this year!",  "bronze_medal.png",
    "Silver",          "There Can Be Only One {event_year} - Silver",  "You fought in {final_matches} finals this year!",    "silver_medal.png",
    "Gold",            "There Can Be Only One {event_year} - Gold",    "You fought in {final_matches} finals this year!",    "gold_medal.png",
    "Epic",            "There Can Be Only One {event_year} - Epic",    "You fought in {final_matches} finals this year!",    "epic_medal.png"
  )
  
  # Normalization function
  normalize_stage <- function(x) {
    stringr::str_squish(
      tolower(gsub("[[:punct:]]", " ", x))
    )
  }
  
  # Define regex pattern for qualifying stages
  pattern <- "(?i)(gold|final(?!.*(eight|octo|quarter|semi|1/8|1/16|1/4|top|pool)))"
  
  # Normalize data
  data_clean <- data %>%
    dplyr::mutate(stage_norm = normalize_stage(stage))
  
  # Filter qualifying records
  data_finalists <- data_clean %>%
    dplyr::filter(stringr::str_detect(stage_norm, pattern))
  
  # Count number of qualifying matches per fighter per event_year
  yearly_counts <- data_finalists %>%
    dplyr::group_by(event_year, fighter_id) %>%
    dplyr::summarise(final_matches = dplyr::n_distinct(match_id), .groups = "drop")
  
  # Assign tiers based on final_matches
  yearly_counts <- yearly_counts %>%
    dplyr::mutate(
      achievement_tier = dplyr::case_when(
        final_matches >= 10 ~ "Epic",
        final_matches >= 5  ~ "Gold",
        final_matches >= 3  ~ "Silver",
        final_matches >= 1  ~ "Bronze",
        TRUE                ~ NA_character_
      )
    ) %>%
    dplyr::filter(!is.na(achievement_tier))
  
  # Total fighters per event_year for percentile calculation
  total_fighters <- data_clean %>%
    dplyr::group_by(event_year) %>%
    dplyr::summarise(total_fighters_in_year = dplyr::n_distinct(fighter_id), .groups = "drop")
  
  # Join details and compute percentile
  achievements <- yearly_counts %>%
    dplyr::left_join(tier_details, by = "achievement_tier") %>%
    dplyr::left_join(total_fighters, by = "event_year") %>%
    dplyr::group_by(event_year) %>%
    dplyr::mutate(percentile = final_matches / total_fighters_in_year) %>%
    dplyr::ungroup() %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      achievement_name = stringr::str_replace_all(achievement_name_template, "\\{event_year\\}", as.character(event_year)),
      achievement_description = stringr::str_replace_all(achievement_description_template, "\\{final_matches\\}", as.character(final_matches))
    ) %>%
    dplyr::ungroup() %>%
    # Add tier_id and achieved columns
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
    # Select only the required columns in the correct order
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