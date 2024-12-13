#' I like it Cozy Achievement
#'
#' @description
#' Internal function to award the "I like it Cozy" achievement to fighters who have never fought outside their home country
#' and have fought more than 100 matches.
#'
#' @param data A data frame with at least the following columns:
#' \describe{
#'   \item{fighter_id}{Identifier of the fighter}
#'   \item{club_country}{Country of the fighter's club}
#'   \item{event_country}{Country where the event took place}
#'   \item{match_id}{Identifier of the match}
#' }
#'
#' @return A data frame with columns:
#' \describe{
#'   \item{fighter_id}{ID of the fighter}
#'   \item{tier_id}{Numeric tier ID (1)}
#'   \item{achieved}{Logical, always TRUE for achievers}
#'   \item{percentile}{Proportion of fighters who achieved this}
#'   \item{achievement_tier}{"Unique"}
#'   \item{achievement_name}{"I like it Cozy"}
#'   \item{achievement_description}{Includes the total number of home matches}
#'   \item{achievement_icon}{"i_like_it_cozy_unique.png"}
#' }
#'
#' @importFrom dplyr mutate group_by summarize filter ungroup left_join select
#' @importFrom tibble tribble
#' @keywords internal
ach_like_cozy <- function(data) {
  
  # Define tiers
  tiers <- tibble::tribble(
    ~achievement_tier, ~tier_id, ~achievement_name,  ~achievement_description,                                            ~achievement_icon,
    "Unique",           1,        "I like it Cozy",  "You fought a hundred times without leaving your home country",     "i_like_it_cozy_unique.png"
  )
  
  # Determine if each match was fought outside the home country
  # is_home = TRUE if the match was fought in the home country
  ach <- data %>%
    dplyr::mutate(
      is_home = (club_country == event_country) & (!is.na(club_country) | !is.na(event_country))
    ) %>%
    dplyr::group_by(fighter_id) %>%
    dplyr::summarize(
      total_matches = dplyr::n_distinct(match_id, na.rm = TRUE),
      matches_home = sum(is_home, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::filter(total_matches > 100 & matches_home == total_matches) %>%
    dplyr::mutate(
      tier_id = 1,
      achieved = TRUE,
      percentile = sum(achieved, na.rm = TRUE) / dplyr::n_distinct(data$fighter_id)
    ) %>%
    dplyr::left_join(tiers, by = "tier_id") %>%
    dplyr::mutate(
      achievement_description = paste0("You fought a total of ", matches_home, " matches in your home country without ever leaving.")
    ) %>%
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
  
  return(ach)
}