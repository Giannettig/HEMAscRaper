#' Conquerer of the Continents Achievement
#'
#' @description
#' Internal function to award "The Conquerer of the continents" achievement based on how many distinct continents (event_region)
#' a fighter has fought in.
#'
#' Tiers:
#' - Epic (tier_id=4): visited >=5 continents
#' - Gold (tier_id=3): visited >=4 continents
#' - Silver (tier_id=2): visited >=3 continents
#' - Bronze (tier_id=1): visited >=2 continents
#'
#' The description and icon are adjusted dynamically. The final description will include the actual number of continents visited.
#'
#' @param data A data frame with at least `fighter_id` and `event_region`.
#'
#' @return A data frame with columns:
#' \describe{
#'   \item{fighter_id}{ID of the fighter}
#'   \item{tier_id}{Numeric tier ID (1-4)}
#'   \item{achieved}{Logical, TRUE if achieved}
#'   \item{percentile}{Proportion of fighters who achieved this tier or higher}
#'   \item{achievement_tier}{"Bronze", "Silver", "Gold", or "Epic"}
#'   \item{achievement_name}{"The Conquerer of the continents"}
#'   \item{achievement_description}{Includes the actual number of continents visited}
#'   \item{achievement_icon}{e.g. "the_conquerer_of_the_continents_bronze.png"}
#' }
#'
#' @importFrom dplyr group_by summarize n_distinct mutate filter ungroup left_join slice_max select
#' @importFrom tibble tribble
#' @keywords internal
ach_conquerer_of_the_continents <- function(data) {
  
  # Define tiers
  tiers <- tibble::tribble(
    ~achievement_tier, ~tier_id, ~achievement_name,                  ~achievement_description,                                           ~achievement_icon,
    "Epic",    4, "The Conquerer of the continents", "Your steel tasted opponents on every continent in the world!",  "the_conquerer_of_the_continents_epic.png",
    "Gold",    3, "The Conquerer of the continents", "You fought in 4 continents",                                     "the_conquerer_of_the_continents_gold.png",
    "Silver",  2, "The Conquerer of the continents", "You fought in 3 continents!",                                    "the_conquerer_of_the_continents_silver.png",
    "Bronze",  1, "The Conquerer of the continents", "You have set foot on 2 continents!",                              "the_conquerer_of_the_continents_bronze.png"
  )
  
  # Compute visited regions per fighter
  regions_per_fighter <- data %>%
    dplyr::group_by(fighter_id) %>%
    dplyr::summarize(visited_regions = dplyr::n_distinct(event_region, na.rm = TRUE), .groups = "drop")
  
  if (nrow(regions_per_fighter) == 0) {
    # No fighters or no event_region data
    return(data.frame(
      fighter_id = integer(0), tier_id = integer(0), achieved = logical(0),
      percentile = numeric(0), achievement_tier = character(0),
      achievement_name = character(0), achievement_description = character(0),
      achievement_icon = character(0), stringsAsFactors = FALSE
    ))
  }
  
  # Assign tiers based on visited_regions
  assign_tier_id <- function(vr) {
    if (vr >= 5) return(4)  # Epic
    else if (vr >= 4) return(3) # Gold
    else if (vr >= 3) return(2) # Silver
    else if (vr >= 2) return(1) # Bronze
    else return(NA)
  }
  
  regions_per_fighter$tier_id <- sapply(regions_per_fighter$visited_regions, assign_tier_id)
  
  achieved_idx <- !is.na(regions_per_fighter$tier_id)
  if (!any(achieved_idx)) {
    # No achievements
    return(data.frame(
      fighter_id = integer(0), tier_id = integer(0), achieved = logical(0),
      percentile = numeric(0), achievement_tier = character(0),
      achievement_name = character(0), achievement_description = character(0),
      achievement_icon = character(0), stringsAsFactors = FALSE
    ))
  }
  
  achievers <- regions_per_fighter[achieved_idx, , drop = FALSE]
  
  # If multiple tiers possible, select highest tier per fighter
  achievers <- achievers %>%
    dplyr::group_by(fighter_id) %>%
    dplyr::slice_max(tier_id, with_ties = FALSE) %>%
    dplyr::ungroup()
  
  total_fighters <- length(unique(data$fighter_id))
  percentile <- nrow(achievers) / total_fighters
  
  # Join tiers
  achievements <- dplyr::left_join(achievers, tiers, by = "tier_id")
  
  # Incorporate dynamic detail: number of continents visited
  # Overwrite achievement_description to include visited_regions
  achievements <- achievements %>%
    dplyr::mutate(
      achieved = TRUE,
      percentile = percentile,
      achievement_description = paste0(
        "You fought in ", visited_regions, 
        ifelse(visited_regions > 1, " continents!", " continent!")
      ),
      # Recompute icon based on tier
      achievement_icon = dplyr::case_when(
        achievement_tier == "Epic" ~ "the_conquerer_of_the_continents_epic.png",
        achievement_tier == "Gold" ~ "the_conquerer_of_the_continents_gold.png",
        achievement_tier == "Silver" ~ "the_conquerer_of_the_continents_silver.png",
        achievement_tier == "Bronze" ~ "the_conquerer_of_the_continents_bronze.png"
      )
    ) %>%
    dplyr::select(
      fighter_id, tier_id, achieved, percentile,
      achievement_tier, achievement_name,
      achievement_description, achievement_icon
    )
  
  return(achievements)
}
