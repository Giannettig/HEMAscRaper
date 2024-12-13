#' King Midas Achievement
#'
#' @description
#' Internal function to award the "King Midas" achievement to the fighter who won the most gold medals in a given year.
#' - Epic (tier_id=4): Awarded to the fighter with the highest number of gold medals in a year.
#'
#' @param data A data frame with at least the following columns:
#' \describe{
#'   \item{fighter_id}{Identifier of the fighter}
#'   \item{event_year}{Year of the event}
#'   \item{stage}{Stage of the match (e.g., "Gold Final", "Winner's Gold Match")}
#'   \item{result}{Match result, e.g., "WIN", "LOSS"}
#' }
#'
#' @return A data frame with columns:
#' \describe{
#'   \item{fighter_id}{ID of the fighter}
#'   \item{tier_id}{Numeric tier ID (4 for Epic)}
#'   \item{achieved}{Logical, TRUE if achieved}
#'   \item{percentile}{Proportion of fighters who achieved this tier in that year}
#'   \item{achievement_tier}{"Epic"}
#'   \item{achievement_name}{"King Midas"}
#'   \item{achievement_description}{Dynamic description mentioning the number of gold medals and the year}
#'   \item{achievement_icon}{"king_midas_epic.png"}
#' }
#'
#' @importFrom dplyr group_by summarize mutate filter ungroup slice_max
#' @importFrom tibble tribble
#' @importFrom stringr str_replace_all
#' @keywords internal
ach_king_midas <- function(data) {
  # Define tier details
  tier_details <- tibble::tribble(
    ~achievement_tier, ~tier_id, ~achievement_name,  ~achievement_description_template,                           ~achievement_icon,
    "Epic",            4,        "King Midas",      "You won the most gold medals ({gold_medals}) in {event_year}!", "king_midas_epic.png"
  )
  
  # Filter data for gold medal matches
  pattern <- "(?i)(gold|final(?!.*(eight|octo|quarter|semi|1/8|1/16|1/4|top|pool)))"
  gold_medal_matches <- data %>%
    dplyr::mutate(stage_norm = tolower(gsub("[[:punct:]]", " ", stage))) %>%
    dplyr::filter(stringr::str_detect(stage_norm, pattern), result == "WIN")
  
  # Calculate the number of gold medals per fighter per year
  yearly_gold_counts <- gold_medal_matches %>%
    dplyr::group_by(event_year, fighter_id) %>%
    dplyr::summarize(gold_medals = dplyr::n_distinct(match_id), .groups = "drop")
  
  # Identify the fighter with the most gold medals for each year
  yearly_midas <- yearly_gold_counts %>%
    dplyr::group_by(event_year) %>%
    dplyr::slice_max(gold_medals, with_ties = FALSE) %>%
    dplyr::mutate(
      tier_id = 4,
      achieved = TRUE
    )
  
  # Total fighters per year for percentile calculation
  total_fighters <- data %>%
    dplyr::group_by(event_year) %>%
    dplyr::summarize(total_fighters_in_year = dplyr::n_distinct(fighter_id), .groups = "drop")
  
  # Join tier details and compute the achievement
  achievements <- yearly_midas %>%
    dplyr::left_join(tier_details, by = "tier_id") %>%
    dplyr::left_join(total_fighters, by = "event_year") %>%
    dplyr::mutate(
      percentile = 1 / total_fighters_in_year,
      achievement_description = ifelse(
        is.na(achievement_description_template),
        NA,  # Keep NA if the template itself is NA
        stringr::str_replace_all(
          achievement_description_template,
          c(
            "\\{gold_medals\\}" = as.character(gold_medals),
            "\\{event_year\\}" = as.character(event_year)
          )
        )
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      fighter_id,
      tier_id,
      achieved,
      percentile,
      achievement_tier,
      achievement_name,
      achievement_description,
      achievement_icon
    ) %>%
    dplyr::mutate(
      fighter_id = as.double(fighter_id),  # Ensure correct type
      tier_id = as.double(tier_id)        # Ensure correct type
    )
  
  # Handle case where no achievements exist
  if (nrow(achievements) == 0) {
    return(data.frame(
      fighter_id = double(0), tier_id = double(0), achieved = logical(0),
      percentile = numeric(0), achievement_tier = character(0),
      achievement_name = character(0), achievement_description = character(0),
      achievement_icon = character(0), stringsAsFactors = FALSE
    ))
  }
  
  return(achievements)
}