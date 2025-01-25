#' Yearly Finals Achievement
#'
#' @description
#' Awards achievements to fighters based on the number of tournament finals
#' they've reached in a given year.
#'
#' @details
#' Tiers are awarded based on number of finals reached in a year:
#' - Epic (4): 10+ finals
#' - Gold (3): 5+ finals
#' - Silver (2): 3+ finals
#' - Bronze (1): 1+ finals
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
#' achievements <- ach_yearly_finals_tiers(tournament_data)
#' }
#'
#' @keywords internal

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
  pattern <- "(?i)(gold|final(?!.*(eight|octo|quarter|semi|semi-finals|1/8|1/16|1/4|top|pool)))"
  
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
  
  # Join total_fighters to yearly_counts
  yearly_counts <- yearly_counts %>%
    dplyr::left_join(total_fighters, by = "event_year")
  
  # Handle cases where total_fighters_in_year is missing
  if (any(is.na(yearly_counts$total_fighters_in_year))) {
    stop("Missing total_fighters_in_year for some event_years. Check your data and joins.")
  }
  
  # Calculate rank and percentile
  yearly_counts <- yearly_counts %>%
    dplyr::group_by(event_year) %>%
    dplyr::arrange(dplyr::desc(final_matches)) %>%
    dplyr::mutate(
      rank = dplyr::row_number(),
      percentile = (rank / total_fighters_in_year) * 100  # Percentile reflects percentage of people with the achievement
    ) %>%
    dplyr::ungroup()
  
  # Join details and format output
  achievements <- yearly_counts %>%
    dplyr::left_join(tier_details, by = "achievement_tier") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      achievement_name = stringr::str_replace_all(
        achievement_name_template, 
        "\\{event_year\\}", 
        as.character(event_year)
      ),
      achievement_description = stringr::str_replace_all(
        achievement_description_template, 
        "\\{final_matches\\}", 
        as.character(final_matches)
      )
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