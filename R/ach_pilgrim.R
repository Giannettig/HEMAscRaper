#' Pilgrim Achievement
#'
#' @description
#' Awards achievements to fighters based on the number of different HEMA scenes
#' they've competed in during a given year.
#'
#' @details
#' Tiers are awarded based on number of distinct HEMA scenes per year:
#' - Epic (4): 5+ scenes
#' - Gold (3): 4 scenes
#' - Silver (2): 3 scenes
#' - Bronze (1): 2 scenes
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
#' achievements <- ach_pilgrim(tournament_data)
#' }
#'
#' @keywords internal

ach_pilgrim <- function(data) {
  # Define tiers
  tiers <- tibble::tribble(
    ~achievement_tier, ~tier_id, ~achievement_name, ~achievement_description,                                         ~achievement_icon,
    "Bronze",           1,        "The Pilgrim {event_year}",     "You fought in tournaments in {distinct_countries} countries in {event_year}!",      "pilgrim_bronze.png",
    "Silver",           2,        "The Pilgrim {event_year}",     "You fought in tournaments in {distinct_countries} countries in {event_year}!",      "pilgrim_silver.png",
    "Gold",             3,        "The Pilgrim {event_year}",     "You fought in tournaments in {distinct_countries} countries in {event_year}!",     "pilgrim_gold.png",
    "Epic",             4,        "The Pilgrim {event_year}",     "You fought in the most countries ({distinct_countries}) in {event_year}!",              "pilgrim_epic.png"
  )
  
  # Filter out rows with missing event_country
  data <- data %>%
    dplyr::filter(!is.na(event_country))
  
  # Calculate the number of countries each fighter participated in per year
  yearly_countries <- data %>%
    dplyr::group_by(event_year, fighter_id) %>%
    dplyr::summarize(
      distinct_countries = dplyr::n_distinct(event_country),
      .groups = "drop"
    )
  
  # Identify the maximum number of countries per year for the Epic tier
  max_countries_per_year <- yearly_countries %>%
    dplyr::group_by(event_year) %>%
    dplyr::summarize(
      max_countries = max(distinct_countries, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::filter(max_countries >= 2)  # Exclude years where the maximum is less than 2
  
  # Assign tiers based on the number of countries
  yearly_tiers <- yearly_countries %>%
    dplyr::left_join(max_countries_per_year, by = "event_year", multiple = "all") %>%
    dplyr::mutate(
      tier_id = dplyr::case_when(
        distinct_countries == max_countries & distinct_countries >= 3 ~ 4,  # Epic tier for the most countries, minimum 3
        distinct_countries >= 7                                       ~ 3,  # Gold tier
        distinct_countries >= 5                                       ~ 2,  # Silver tier
        distinct_countries >= 3                                       ~ 1,  # Bronze tier
        TRUE                                                          ~ NA_integer_
      )
    ) %>%
    dplyr::filter(!is.na(tier_id))  # Only keep fighters who achieved a tier
  
  # Calculate total fighters per year for percentile calculation
  total_fighters_per_year <- data %>%
    dplyr::group_by(event_year) %>%
    dplyr::summarize(
      total_fighters = dplyr::n_distinct(fighter_id),
      .groups = "drop"
    )
  
  # Calculate cumulative counts for percentiles
  tier_counts <- yearly_tiers %>%
    dplyr::group_by(event_year, tier_id) %>%
    dplyr::summarize(
      tier_count = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::arrange(event_year, dplyr::desc(tier_id)) %>%
    dplyr::group_by(event_year) %>%
    dplyr::mutate(
      cumulative_count = cumsum(tier_count)
    ) %>%
    dplyr::ungroup()
  
  # Join tier details and calculate dynamic descriptions
  achievements <- yearly_tiers %>%
    dplyr::left_join(tiers, by = "tier_id") %>%
    dplyr::left_join(total_fighters_per_year, by = "event_year") %>%
    dplyr::left_join(tier_counts, by = c("event_year", "tier_id")) %>%
    dplyr::mutate(
      achievement_name = stringr::str_replace(achievement_name, "\\{event_year\\}", as.character(event_year)),
      achievement_description = stringr::str_replace(
        stringr::str_replace(achievement_description, "\\{event_year\\}", as.character(event_year)),
        "\\{distinct_countries\\}", as.character(distinct_countries)
      ),
      achieved = TRUE,
      percentile = (cumulative_count / total_fighters) * 100  # Correct percentile calculation
    ) %>%
    dplyr::select(
      fighter_id, tier_id, achieved, percentile, 
      achievement_tier, achievement_name, 
      achievement_description, achievement_icon
    )
  
  return(achievements)
}