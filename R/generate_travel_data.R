#' @title Generate Travel Data
#' @description This is an internal function to generate travel data from HEMA-related datasets.
#' @param year A numeric value, vector of numeric values, or NULL. Specifies the event years to filter.
#' @param tournament_weapon A character vector or NULL. Specifies the tournament weapons to filter.
#' @param hema_clubs A dataset containing HEMA clubs. Default: HEMAscRaper::hema_clubs.
#' @param hema_events A dataset containing HEMA events. Default: HEMAscRaper::hema_events.
#' @param hema_match_results A dataset containing HEMA match results. Default: HEMAscRaper::hema_match_results.
#' @return A list containing two data frames: `nodes` and `edges`.
#' @import dplyr purrr stringr HEMAscRaper
#' @keywords internal
#' 
generate_travel_data <- function(year = NULL, 
                                 tournament_weapon = NULL, 
                                 hema_clubs = HEMAscRaper::hema_clubs, 
                                 hema_events = HEMAscRaper::hema_events, 
                                 hema_match_results = HEMAscRaper::hema_match_results) {
  
  # Convert year argument to numeric if it's provided as a string
  if (!is.null(year)) {
    if (base::is.character(year)) {
      year <- base::as.numeric(year)
    }
    
    if (!base::is.numeric(year) && !base::is.vector(year)) {
      base::stop("Argument 'year' must be a numeric value, vector of numeric values, or NULL.")
    }
  }
  
  # Validate tournament_weapon argument
  valid_weapons <- dplyr::pull(dplyr::distinct(hema_match_results, tournament_weapon))
  
  if (!is.null(tournament_weapon)) {
    if (!base::is.character(tournament_weapon) || 
        !base::all(tournament_weapon %in% valid_weapons)) {
      base::stop("Argument 'tournament_weapon' must be NULL or a character vector containing valid weapons.")
    }
  }
  
  # Start with the full dataset
  filtered_results <- hema_match_results%>%
    dplyr::left_join(hema_clubs, by = "club_id") %>%
    dplyr::left_join(hema_events, by = "event_id")
  
  # Apply year filter if not NULL
  if (!is.null(year)) {
    filtered_results <- dplyr::filter(
      filtered_results, 
      event_year %in% year & !base::is.na(event_year)
    )
  }
  
  # Apply tournament weapon filter if not NULL
  if (!is.null(tournament_weapon)) {
    filtered_results <- dplyr::filter(
      filtered_results, 
      tournament_weapon %in% tournament_weapon & !base::is.na(tournament_weapon)
    )
  }
  
  # Create Nodes File
  nodes <- filtered_results %>%
    dplyr::group_by(club_country) %>%
    dplyr::summarise(
      Population = dplyr::n_distinct(fighter_id), 
      .groups = "drop"
    ) %>%
    dplyr::left_join(hema_countries, by = c("club_country" = "name")) %>%
    dplyr::mutate(Id = dplyr::row_number()) %>%
    dplyr::rename(
      Label = club_country,
      Region = region,
      Sub_Region = `sub_region`
    ) %>%
    dplyr::select(Id, Label, Region, Sub_Region, Population) %>%
    dplyr::filter(!base::is.na(Label))
  
  # Create Undirected Edges File - will merge directions
  edges <- filtered_results %>%
    dplyr::filter(club_country != event_country) %>%
    dplyr::mutate(
      Target = purrr::map2_chr(club_country, event_country, function(x, y) {
        base::paste(base::sort(c(x, y)), collapse = " | ")
      })
    ) %>%
    dplyr::group_by(Target) %>%
    dplyr::summarise(Weight = dplyr::n_distinct(fighter_id), .groups = "drop") %>%
    dplyr::mutate(
      Source_Label = stringr::str_squish(stringr::str_split_i(Target, "\\|", 1)),
      Target_Label = stringr::str_squish(stringr::str_split_i(Target, "\\|", 2))
    ) %>%
    dplyr::select(-Target) %>%
    dplyr::left_join(nodes, by = c("Source_Label" = "Label")) %>%
    dplyr::rename(Source = Id) %>%
    dplyr::left_join(nodes, by = c("Target_Label" = "Label")) %>%
    dplyr::rename(Target = Id) %>%
    dplyr::select(Source, Target, Source_Label, Target_Label, Weight)
  
  # Return the results as a list
  return(list(nodes = nodes, edges = edges))
}
