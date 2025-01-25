#' Conquerer of the Continents Achievement
#'
#' @description
#' Awards achievements to fighters based on the number of different continents
#' they've competed in.
#'
#' @details
#' Tiers are awarded based on number of distinct continents:
#' - Epic (4): 5+ continents
#' - Gold (3): 4 continents
#' - Silver (2): 3 continents
#' - Bronze (1): 2 continents
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
#' achievements <- ach_conquerer_of_the_continents(tournament_data)
#' }
#'
#' @keywords internal
ach_conquerer_of_the_continents <- function(data) {
  # Define tiers
  tiers <- tibble::tribble(
    ~achievement_tier, ~tier_id, ~achievement_name,                  ~achievement_description_template,                                           ~achievement_icon,
    "Epic",    4, "The Conquerer of the continents", "Your steel tasted opponents on every continent in the world!",  "the_conquerer_of_the_continents_epic.png",
    "Gold",    3, "The Conquerer of the continents", "You fought in {visited_regions} continents!",                                     "the_conquerer_of_the_continents_gold.png",
    "Silver",  2, "The Conquerer of the continents", "You fought in {visited_regions} continents!",                                    "the_conquerer_of_the_continents_silver.png",
    "Bronze",  1, "The Conquerer of the continents", "You fought in {visited_regions} continents!",                              "the_conquerer_of_the_continents_bronze.png"
  )
  
  # Filter out rows with missing event_region
  data <- data %>% dplyr::filter(!is.na(event_region))
  
  # Compute visited regions per fighter
  regions_per_fighter <- data %>%
    dplyr::group_by(fighter_id) %>%
    dplyr::summarize(visited_regions = dplyr::n_distinct(event_region, na.rm = TRUE), .groups = "drop")
  
  # Handle case where no fighters meet the criteria
  if (nrow(regions_per_fighter) == 0) {
    return(data.frame(
      fighter_id = integer(0), tier_id = integer(0), achieved = logical(0),
      percentile = numeric(0), achievement_tier = character(0),
      achievement_name = character(0), achievement_description = character(0),
      achievement_icon = character(0), stringsAsFactors = FALSE
    ))
  }
  
  # Assign tiers based on visited_regions
  regions_per_fighter <- regions_per_fighter %>%
    dplyr::mutate(
      tier_id = dplyr::case_when(
        visited_regions >= 5 ~ 4,  # Epic
        visited_regions >= 4 ~ 3,  # Gold
        visited_regions >= 3 ~ 2,  # Silver
        visited_regions >= 2 ~ 1,  # Bronze
        TRUE                 ~ NA_integer_
      )
    ) %>%
    dplyr::filter(!is.na(tier_id))  # Only keep fighters who achieved a tier
  
  # Handle case where no fighters meet the criteria
  if (nrow(regions_per_fighter) == 0) {
    return(data.frame(
      fighter_id = integer(0), tier_id = integer(0), achieved = logical(0),
      percentile = numeric(0), achievement_tier = character(0),
      achievement_name = character(0), achievement_description = character(0),
      achievement_icon = character(0), stringsAsFactors = FALSE
    ))
  }
  
  # Total fighters for percentile calculation
  total_fighters <- dplyr::n_distinct(data$fighter_id)
  
  # Calculate cumulative counts for percentiles
  tier_counts <- regions_per_fighter %>%
    dplyr::group_by(tier_id) %>%
    dplyr::summarize(tier_count = dplyr::n(), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(tier_id)) %>%
    dplyr::mutate(cumulative_count = cumsum(tier_count))
  
  # Join tier details and calculate dynamic descriptions
  achievements <- regions_per_fighter %>%
    dplyr::left_join(tiers, by = "tier_id") %>%
    dplyr::left_join(tier_counts, by = "tier_id")
  
  achievements$achievement_description<- stringr::str_replace_all(
  achievements$achievement_description_template,"\\{visited_regions\\}", as.character(achievements$visited_regions))
  
  achievements<-achievements%>%
    mutate(
      achieved = TRUE,
      percentile = (1 - (cumulative_count / total_fighters)) * 100  # Correct percentile calculation as a percentage
    ) %>%
    dplyr::select(
      fighter_id, tier_id, achieved, percentile,
      achievement_tier, achievement_name,
      achievement_description, achievement_icon
    )
  
  return(achievements)
}