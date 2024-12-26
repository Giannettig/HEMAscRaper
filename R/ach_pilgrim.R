#' The Pilgrim Achievement
#'
#' @description
#' Internal function to award the "The Pilgrim" achievement to fighters who traveled to multiple countries to fight in tournaments in a given year.
#' - Bronze: Participated in tournaments in 2 countries.
#' - Silver: Participated in tournaments in 5 countries.
#' - Gold: Participated in tournaments in 15 countries.
#' - Epic: Participated in the most countries in the year.
#'
#' @param data A data frame with at least the following columns:
#' \describe{
#'   \item{fighter_id}{Identifier of the fighter.}
#'   \item{event_year}{Year of the event.}
#'   \item{event_country}{Country where the event was held.}
#' }
#'
#' @return A data frame with columns:
#' \describe{
#'   \item{fighter_id}{ID of the fighter.}
#'   \item{tier_id}{Numeric tier ID.}
#'   \item{achieved}{Logical, TRUE if achieved.}
#'   \item{percentile}{Proportion of fighters who achieved this tier.}
#'   \item{achievement_tier}{Tier name (Bronze, Silver, Gold, Epic).}
#'   \item{achievement_name}{Name of the achievement.}
#'   \item{achievement_description}{Description of the achievement.}
#'   \item{achievement_icon}{Filename of the achievement icon.}
#' }
#'
#' @keywords internal
ach_pilgrim <- function(data) {
  # Define tiers
  tiers <- tibble::tribble(
    ~achievement_tier, ~tier_id, ~achievement_name, ~achievement_description,                                         ~achievement_icon,
    "Bronze",           1,        "The Pilgrim {event_year}",     "You fought in tournaments in 2 countries in {event_year}!",      "pilgrim_bronze.png",
    "Silver",           2,        "The Pilgrim {event_year}",     "You fought in tournaments in 5 countries in {event_year}!",      "pilgrim_silver.png",
    "Gold",             3,        "The Pilgrim {event_year}",     "You fought in tournaments in 15 countries in {event_year}!",     "pilgrim_gold.png",
    "Epic",             4,        "The Pilgrim {event_year}",     "You fought in the most countries in {event_year}!",              "pilgrim_epic.png"
  )
  
  # Calculate the number of countries each fighter participated in per year
  yearly_countries <- data %>%
    dplyr::filter(!is.na(event_country)) %>%
    dplyr::group_by(event_year, fighter_id) %>%
    dplyr::summarise(
      distinct_countries = n_distinct(event_country),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      tier_id = dplyr::case_when(
        distinct_countries == max(distinct_countries) ~ 4,  # Epic tier for the most countries
        distinct_countries >= 15                     ~ 3,  # Gold tier
        distinct_countries >= 5                      ~ 2,  # Silver tier
        distinct_countries >= 2                      ~ 1,  # Bronze tier
        TRUE                                         ~ NA_integer_
      )
    ) %>%
    dplyr::filter(!is.na(tier_id))  # Only keep fighters who achieved a tier
  
  # Calculate percentile
  total_fighters <- data %>%
    dplyr::group_by(event_year) %>%
    dplyr::summarise(total_fighters_in_year = n_distinct(fighter_id), .groups = "drop")
  
  # Join tier details and calculate dynamic descriptions
  achievements <- yearly_countries %>%
    dplyr::left_join(tiers, by = "tier_id") %>%
    dplyr::left_join(total_fighters, by = "event_year") %>%
    dplyr::mutate(
      achievement_name = stringr::str_replace_all(achievement_name, "\\{event_year\\}", as.character(event_year)),
      achievement_description = stringr::str_replace_all(
        achievement_description,
        "\\{event_year\\}",
        as.character(event_year)
      ),
      achieved = TRUE,
      percentile = n() / total_fighters_in_year
    ) %>%
    dplyr::select(
      fighter_id, tier_id, achieved, percentile, 
      achievement_tier, achievement_name, 
      achievement_description, achievement_icon
    )
  
  return(achievements)
}