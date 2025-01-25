#' Animal Lover Achievement
#'
#' @description
#' Awards achievements to fighters who participate in tournaments with animal-themed
#' names or mascots (e.g., tournaments containing words like "penguin", "lion",
#' "bear", etc.).
#'
#' @details
#' Tiers are awarded based on number of distinct animal-themed tournaments:
#' - Epic (4): 8+ tournaments
#' - Gold (3): 5+ tournaments
#' - Silver (2): 3+ tournaments
#' - Bronze (1): 1+ tournaments
#'
#' The achievement considers both explicit animal names in tournament titles and
#' specific tournaments known to have animal mascots (e.g., "bohema",
#' "helsinki longsword open").
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
#' achievements <- ach_animal_lover(tournament_data)
#' }
#'
#' @keywords internal
ach_animal_lover <- function(data) {
  # Define all tiers and their conditions in a single data frame
  tiers <- tibble::tribble(
    ~achievement_tier, ~tier_id, ~achievement_name, ~achievement_description_template, ~achievement_icon,
    "Epic",            4,        "Animal Lover",    "You fought in {animal_tournaments} tournaments with an animal mascot!", "animal_lover_epic.png",
    "Gold",            3,        "Animal Lover",    "You fought in {animal_tournaments} tournaments with an animal mascot!", "animal_lover_gold.png",
    "Silver",          2,        "Animal Lover",    "You fought in {animal_tournaments} tournaments with an animal mascot!", "animal_lover_silver.png",
    "Bronze",          1,        "Animal Lover",    "You fought in {animal_tournaments} tournaments with an animal mascot!", "animal_lover_bronze.png"
  )
  
  # Filter out rows with missing event_brand or event_id
  data <- data %>% dplyr::filter(!is.na(event_brand) & !is.na(event_id))
  
  # Normalize event names
  normalized_data <- data %>%
    dplyr::mutate(
      event_name_normalized = tolower(event_brand) %>%
        stringr::str_replace_all("[^a-z\\s]", "") %>%
        stringr::str_squish()
    )
  
  # Updated animal-related regex to enforce matches at the start of words
  pattern <- "\\b(penguin|lion|bear|boar|fish|wolf|goose|swordfish|unicorn|horn|beast|falcon|hawk|eagle|serpent|dragon|owl|fox|rabbit|bison)"
  
  # Events explicitly featuring animals
  explicit_animal_events <- c("bohema", "helsinki longsword open")
  
  # Filter events matching the pattern or explicitly listed
  animal_related_events <- normalized_data %>%
    dplyr::filter(
      stringr::str_detect(event_name_normalized, pattern) |
        event_brand %in% explicit_animal_events
    ) %>%
    dplyr::select(event_id) %>%
    dplyr::distinct()
  
  # Handle case where no animal-related events exist
  if (nrow(animal_related_events) == 0) {
    return(data.frame(
      fighter_id = integer(0), tier_id = integer(0), achieved = logical(0),
      percentile = numeric(0), achievement_tier = character(0),
      achievement_name = character(0), achievement_description = character(0),
      achievement_icon = character(0), stringsAsFactors = FALSE
    ))
  }
  
  # Count distinct animal tournaments per fighter
  achievement_data <- data %>%
    dplyr::semi_join(animal_related_events, by = "event_id") %>%
    dplyr::group_by(fighter_id) %>%
    dplyr::summarize(animal_tournaments = n_distinct(event_brand), .groups = "drop") %>%
    dplyr::mutate(
      tier_id = dplyr::case_when(
        animal_tournaments >= 8 ~ 4,  # Epic
        animal_tournaments >= 5 ~ 3,  # Gold
        animal_tournaments >= 3 ~ 2,  # Silver
        animal_tournaments >= 1 ~ 1,  # Bronze
        TRUE ~ NA_integer_
      ),
      achieved = !is.na(tier_id)
    ) %>%
    dplyr::filter(achieved)
  
  # Handle case where no fighters meet the criteria
  if (nrow(achievement_data) == 0) {
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
  tier_counts <- achievement_data %>%
    dplyr::group_by(tier_id) %>%
    dplyr::summarize(tier_count = dplyr::n(), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(tier_id)) %>%
    dplyr::mutate(cumulative_count = cumsum(tier_count))
  
  # Join tier details and calculate dynamic descriptions
  achievements <- achievement_data %>%
    dplyr::left_join(tiers, by = "tier_id") %>%
    dplyr::left_join(tier_counts, by = "tier_id") %>%
    dplyr::mutate(
      achievement_description = achievement_description_template,
      achieved = TRUE,
      percentile = (1 - (cumulative_count / total_fighters)) * 100  # Correct percentile calculation
    )
  
  achievements$achievement_description<-stringr::str_replace_all(achievements$achievement_description,"\\{animal_tournaments\\}", as.character(achievements$animal_tournaments) )
  achievements<-achievements%>%
    dplyr::select(
      fighter_id, tier_id, achieved, percentile,
      achievement_tier, achievement_name,
      achievement_description, achievement_icon
    )
  
  return(achievements)
}