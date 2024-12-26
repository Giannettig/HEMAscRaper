#' Animal Lover Achievement
#'
#' Identifies fighters who participated in tournaments featuring an animal mascot. Awards achievements based on the number of distinct tournaments attended in a given year.
#'
#' @param data A data frame containing HEMA tournament information. Required columns include:
#'   - `fighter_id`: Unique identifier for each fighter.
#'   - `event_year`: Year of the event.
#'   - `event_id`: Unique identifier for events.
#'   - `tournament_id`: Unique identifier for tournaments.
#'   - `event_brand`: Event name or branding information.
#' @return A data frame with the following columns:
#'   - `fighter_id`: Unique identifier for each fighter.
#'   - `tier_id`: Achievement tier level.
#'   - `achieved`: Logical, indicating if the achievement was earned.
#'   - `percentile`: Proportion of fighters achieving this tier in the dataset.
#'   - `achievement_tier`: Name of the tier (e.g., Bronze).
#'   - `achievement_name`: Name of the achievement.
#'   - `achievement_description`: Description of the achievement with dynamic details.
#'   - `achievement_icon`: Icon file associated with the achievement.
#' @keywords internal
#' @examples
#' # Example usage:
#' # ach_animal_lover(data)
ach_animal_lover <- function(data) {
  # Define all tiers and their conditions in a single data frame
  tiers <- tibble::tribble(
    ~achievement_tier, ~tier_id, ~achievement_name, ~achievement_description, ~achievement_icon,
    "Epic",            4,        "Animal Lover",    "You fought in {animal_tournaments} tournaments with an animal mascot!", "animal_lover_epic.png",
    "Gold",            3,        "Animal Lover",    "You fought in {animal_tournaments} tournaments with an animal mascot!", "animal_lover_gold.png",
    "Silver",          2,        "Animal Lover",    "You fought in {animal_tournaments} tournaments with an animal mascot!", "animal_lover_silver.png",
    "Bronze",          1,        "Animal Lover",    "You fought in {animal_tournaments} tournaments with an animal mascot!", "animal_lover_bronze.png"
  )
  
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
  
  # Count distinct animal tournaments per fighter per year
  achievement_data <- data %>%
    dplyr::semi_join(animal_related_events, by = "event_id") %>%
    dplyr::group_by(fighter_id) %>%
    dplyr::summarise(animal_tournaments = n_distinct(event_brand), .groups = "drop") %>%
    dplyr::mutate(
      tier_id = dplyr::case_when(
        animal_tournaments >= 8 ~ 4,
        animal_tournaments >= 5 ~ 3,
        animal_tournaments >= 3 ~ 2,
        animal_tournaments >= 1 ~ 1,
        TRUE ~ NA_integer_
      ),
      achieved = !is.na(tier_id)
    ) %>%
    dplyr::filter(achieved) %>%
    dplyr::mutate(percentile = n() / base::nrow(data %>% dplyr::distinct(fighter_id))) %>%
    dplyr::left_join(tiers, by = "tier_id") %>%
    dplyr::mutate(
      achievement_description = stringr::str_replace_all(
        achievement_description,
        "\\{animal_tournaments\\}",
        as.character(animal_tournaments)
      )
    ) %>%
    dplyr::select(
      fighter_id, tier_id, achieved, percentile, 
      achievement_tier, achievement_name, 
      achievement_description, achievement_icon
    ) %>%
    dplyr::filter(!is.na(achievement_name)) # Ensure valid achievements only
  
  return(achievement_data)
}
