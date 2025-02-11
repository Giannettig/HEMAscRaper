#' Calculate Elo Ratings for HEMA Matches
#'
#' This function calculates Elo ratings for HEMA matches, along with win probabilities and updated ratings for each match.
#' Matches are processed in sequence, ordered by event date and tournament ID.
#'
#' @param hema_fights A data frame containing HEMA match data. Must include the following columns:
#'   - `event_date`: Date of the event.
#'   - `tournament_id`: ID of the tournament.
#'   - `stage`: Stage of the match.
#'   - `fighter_1`: Name or ID of Fighter 1.
#'   - `fighter_2`: Name or ID of Fighter 2.
#'   - `fighter_1_result`: Result for Fighter 1 ("WIN", "LOSS", "DRAW", etc.).
#'   - `fighter_2_result`: Result for Fighter 2 ("WIN", "LOSS", "DRAW", etc.).
#' @param initial_rating Numeric, the initial Elo rating for all fighters. Default is 1200.
#' @param k Numeric, the K-factor for Elo updates. Default is 32.
#'
#' @return A data frame with match details, including Elo ratings, win probabilities, and updates for each match.
#'
#' @import dplyr
#' @import tidyr
#' @import purrr
#' @importFrom elo elo.run
#'
#'
#' @export
generate_ratings <- function(hema_fights, initial_rating = 800, k = 32) {
  # Convert match results to numeric values
  matches <- hema_fights%>%select(1:16) %>%
    dplyr::mutate(
      fighter1_score = dplyr::case_when(
        fighter_1_result == "WIN" ~ 1,
        fighter_1_result == "LOSS" ~ 0,
        fighter_1_result == "DRAW" ~ 0.5,
        fighter_1_result == "DOUBLE LOSS" ~ 0, # Both fighters get 0
        fighter_1_result %in% c("NO DATA", "UNRATED WIN", "UNRATED LOSS") ~ NA_real_,
        TRUE ~ NA_real_
      ),
      fighter2_score = dplyr::case_when(
        fighter_2_result == "WIN" ~ 1,
        fighter_2_result == "LOSS" ~ 0,
        fighter_2_result == "DRAW" ~ 0.5,
        fighter_2_result == "DOUBLE LOSS" ~ 0, # Both fighters get 0
        fighter_2_result %in% c("NO DATA", "UNRATED WIN", "UNRATED LOSS") ~ NA_real_,
        TRUE ~ NA_real_
      ),
      tournament_category = case_when(
        str_detect(tournament_category, "Mixed|Men's") ~ "Mixed & Men's",
        TRUE ~ tournament_category  # Retain the original category for other cases
        
      )
    ) %>%
    dplyr::filter(fighter1_score %in% c(0, 1, 0.5) & !is.na(fighter_1) & !is.na(fighter_2)) %>%
    dplyr::arrange(event_date, event_id)
  
  # Extract unique tournament categories and weapons
  categories <- matches %>%
    dplyr::select(tournament_category, tournament_weapon) %>%
    dplyr::distinct()
  
  # Process each category separately
  category_matches <- purrr::map2(
    categories$tournament_category,
    categories$tournament_weapon,
    ~ matches %>% dplyr::filter(tournament_category == .x, tournament_weapon == .y)
  )
  
  ordered_matches<-map_df(category_matches, as_tibble)%>%select(1:16)
  
  # Calculate Elo ratings for each category
  elo_matches <- purrr::map_df(category_matches, function(x) {
    elo::elo.run(
      fighter1_score ~ fighter_1 + fighter_2,
      data = x,
      initial.elos = initial_rating,
      k = k
    ) %>%
      dplyr::as_tibble()
  }) %>%
    dplyr::select(-1, -2, -wins.A) %>%
    dplyr::bind_cols(ordered_matches %>% dplyr::select(1), .) %>%
    dplyr::mutate(win_chance_2 = 1 - p.A) %>%
    dplyr::rename(
      win_chance_1 = p.A,
      update_1 = update.A,
      update_2 = update.B,
      elo_1 = elo.A,
      elo_2 = elo.B
    ) %>%
    dplyr::select(1, 2, 7, 3:6)
  
  # Merge Elo results back with original data
  ranked_matches <- ordered_matches %>%
    dplyr::left_join(elo_matches, by = dplyr::join_by(match_id))
  
  return(ranked_matches)
  
}
