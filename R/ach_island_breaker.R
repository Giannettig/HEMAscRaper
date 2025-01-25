#' Island Breaker Achievement
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
#' achievements <- ach_island_breaker(tournament_data)
#' }
#'
#' @keywords internal
ach_island_breaker <- function(data) {
  # Define tiers
  tiers <- tibble::tribble(
    ~achievement_tier, ~tier_id, ~achievement_name,         ~achievement_description_template,                                              ~achievement_icon,
    "Bronze",           1,        "Island Breaker {event_year}",         "You fought in {distinct_scenes} different HEMA scenes in {event_year}!",              "island_breaker_bronze.png",
    "Silver",           2,        "Island Breaker {event_year}",         "You fought in {distinct_scenes} different HEMA scenes in {event_year}!",              "island_breaker_silver.png",
    "Gold",             3,        "Island Breaker {event_year}",         "You fought in {distinct_scenes} different HEMA scenes in {event_year}!",              "island_breaker_gold.png",
    "Epic",             4,        "Island Breaker {event_year}",         "You fought in {distinct_scenes} different HEMA scenes in {event_year}!",      "island_breaker_epic.png"
  )
  
  # Ensure required columns are present
  required_cols <- c("event_community", "event_year", "fighter_id")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  
  # Filter out rows with missing event_community
  data <- data %>% dplyr::filter(!is.na(event_community))
  
  # Calculate the number of scenes each fighter participated in per year
  yearly_scenes <- data %>%
    dplyr::group_by(event_year, fighter_id) %>%
    dplyr::summarize(
      distinct_scenes = n_distinct(event_community),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      tier_id = dplyr::case_when(
        distinct_scenes >= 5 ~ 4,  # Epic
        distinct_scenes >= 4 ~ 3,  # Gold
        distinct_scenes >= 3 ~ 2,  # Silver
        distinct_scenes >= 2 ~ 1,  # Bronze
        TRUE                 ~ NA_integer_
      )
    ) %>%
    dplyr::filter(!is.na(tier_id))  # Only keep fighters who achieved a tier
  
  # Handle case where no fighters meet the criteria
  if (nrow(yearly_scenes) == 0) {
    return(data.frame(
      fighter_id = integer(0), tier_id = integer(0), achieved = logical(0),
      percentile = numeric(0), achievement_tier = character(0),
      achievement_name = character(0), achievement_description = character(0),
      achievement_icon = character(0), stringsAsFactors = FALSE
    ))
  }
  
  # Total fighters per year for percentile calculation
  total_fighters <- data %>%
    dplyr::group_by(event_year) %>%
    dplyr::summarize(
      total_fighters = n_distinct(fighter_id),
      .groups = "drop"
    )
  
  # Calculate cumulative counts for percentiles
  tier_counts <- yearly_scenes %>%
    dplyr::group_by(event_year, tier_id) %>%
    dplyr::summarize(
      tier_count = n(),
      .groups = "drop"
    ) %>%
    dplyr::arrange(event_year, dplyr::desc(tier_id)) %>%
    dplyr::group_by(event_year) %>%
    dplyr::mutate(
      cumulative_count = cumsum(tier_count)
    ) %>%
    dplyr::ungroup()
  
  # Join tier details and calculate dynamic descriptions
  achievements <- yearly_scenes %>%
    dplyr::left_join(tiers, by = "tier_id") %>%
    dplyr::left_join(total_fighters, by = "event_year") %>%
    dplyr::left_join(tier_counts, by = c("event_year", "tier_id")) %>%
    dplyr::mutate(
      achievement_name = stringr::str_replace(
        achievement_name,
        "\\{event_year\\}",as.character(event_year)
      ),
      achievement_description = stringr::str_replace(
        achievement_description_template,
        "\\{distinct_scenes\\}", as.character(distinct_scenes))%>%
        stringr::str_replace("\\{event_year\\}", as.character(event_year)
      ),
      achieved = TRUE,
      percentile = 1 - (cumulative_count / total_fighters)  # Correct percentile calculation
    ) %>%
    dplyr::select(
      fighter_id, tier_id, achieved, percentile, 
      achievement_tier, achievement_name, 
      achievement_description, achievement_icon
    )
  
  return(achievements)
}