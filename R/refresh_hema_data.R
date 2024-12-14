#' Refresh HEMA Ratings Data
#'
#' Downloads and processes HEMA ratings data, performing incremental or full updates, and saves the results as CSV files.
#' Infividual fencer achievements are calculated and tored in path. 
#'
#' @param incremental Logical. If `TRUE`, performs an incremental update of the fights dataset. If `FALSE`, downloads the entire fights dataset. Defaults to `TRUE`.
#' @param path Character. The directory where the processed CSV files will be saved. Defaults to `"./hema_ratings"`.
#'
#' @return None. This function saves the processed data to the specified path as CSV files.
#'
#' @importFrom dplyr bind_rows group_by ungroup mutate select filter summarize rename arrange cur_group_id
#' @importFrom purrr walk
#' @importFrom readr read_csv write_csv
#' @importFrom stringr str_extract
#' @importFrom utils data
#' @examples
#' \dontrun{
#' # Perform a full update
#' refresh_hema_data(incremental = FALSE, path = "./hema_ratings")
#'
#' # Perform an incremental update
#' refresh_hema_data(incremental = TRUE, path = "./hema_ratings")
#' }
#' @export
refresh_hema_data <- function(incremental = TRUE, path = "./hema_ratings") {
  # Start by downloading non-incremental data
  hema_clubs <- get_clubs()
  hema_fighters <- get_fighters()
  hema_events <- get_events()
  
  # Incremental update of fights
  if (!incremental) {
    hema_fights <- get_fights()
  } else if (file.exists(file.path(path, "hema_fights.csv"))) {
    hema_fights <- readr::read_csv(file.path(path, "hema_fights.csv"))
  } else {
    hema_fights<-hema_fights
  }
  
  # Find last year in fight data
  last_year <- hema_fights$event_name %>%
    unique() %>%
    stringr::str_extract("\\b\\d{4}\\b") %>%
    unique() %>%
    as.integer() %>%
    max(na.rm = TRUE)
  
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  
  # Get missing years of data
  new_fights <- get_fights(last_year:current_year)
  old_fights <- dplyr::filter(hema_fights, !(event_id %in% new_fights$event_id))
  
  # Combine new and old fight data
  hema_fights <- dplyr::bind_rows(new_fights, old_fights) %>%
    dplyr::group_by(event_id, tournament_name) %>%
    dplyr::mutate(tournament_id = dplyr::cur_group_id()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(match_id = dplyr::row_number()) %>%
    dplyr::select(match_id, tournament_id, event_id, dplyr::everything())
  
  message("Refining HEMA Matches")
  
  # Match results
  hema_match_results <- hema_fights %>%
    { result <- .; dplyr::bind_rows(
      dplyr::rename(result, fighter_name = fighter_1, opponent_name = fighter_2, result = fighter_1_result) %>%
        dplyr::select(-fighter_2_result),
      dplyr::rename(result, fighter_id = opponent_id, opponent_id = fighter_id, fighter_name = fighter_2,
                    opponent_name = fighter_1, result = fighter_2_result) %>%
        dplyr::select(-fighter_1_result)
    ) } %>%
    dplyr::mutate(match_id = dplyr::row_number()) %>%
    dplyr::left_join(dplyr::select(hema_fighters, fighter_id, fighter_club_id), by = dplyr::join_by(fighter_id)) %>%
    dplyr::rename(club_id = fighter_club_id) %>%
    dplyr::group_by(fighter_id, fighter_name) %>%
    dplyr::arrange(fighter_id, event_id) %>%
    dplyr::mutate(debut_fight = dplyr::row_number() == 1) %>%
    dplyr::ungroup() %>%
    dplyr::select(1:9, club_id, dplyr::everything())
  
  # Tournaments
  message("Refining HEMA Tournaments")
  hema_tournaments <- hema_match_results %>%
    dplyr::group_by(tournament_id, tournament_name, event_id, tournament_category, tournament_weapon, tournament_note) %>%
    dplyr::summarize(match_count = dplyr::n_distinct(match_id), fighter_count = dplyr::n_distinct(fighter_id), .groups = "drop")
  
  
  # Save data as CSV files
  data_list <- list(
    hema_clubs = hema_clubs,
    hema_countries = hema_countries,
    hema_events = hema_events,
    hema_fighters = hema_fighters,
    hema_fights = hema_fights,
    hema_match_results = hema_match_results,
    hema_tournaments = hema_tournaments
  )
  
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }
  
  
  message(paste0("Saving the data in the ", path, " directory"))
  
  purrr::walk(names(data_list), function(name) {
    file_path <- file.path(path, paste0(name, ".csv"))
    readr::write_csv(data_list[[name]], file = file_path, na = "")
  })
  
  message("calculating HEMA Achievements")
  generate_achievements(path, TRUE)
  
}

