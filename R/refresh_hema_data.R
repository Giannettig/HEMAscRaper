#' Refresh HEMA Ratings Data
#'
#' Downloads and processes HEMA ratings data, performing incremental or full updates, and saves the results as CSV files.
#' Individual fencer achievements are calculated and stored in \code{path}.
#'
#' @param incremental Logical. If `TRUE`, performs an incremental update of the fights dataset.
#'   If `FALSE`, downloads the entire fights dataset. Defaults to `TRUE`.
#' @param path Character. The directory where the processed CSV files will be saved.
#'   Defaults to `"./hema_ratings"`.
#'
#' @return None. This function saves the processed data to the specified path as CSV files.
#'
#' @importFrom dplyr bind_rows group_by ungroup mutate select filter summarize rename arrange cur_group_id left_join slice_max
#' @importFrom purrr walk
#' @importFrom readr read_csv write_csv
#' @importFrom stringr str_extract str_to_lower str_detect str_squish
#' @importFrom tidyr pivot_wider
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
  
  # Download core data (non-incremental logic for clubs, fighters, events)
  hema_clubs    <- get_clubs()
  hema_fighters <- get_fighters()
  hema_events   <- get_events()
  
  # Incremental or non-incremental update of rankings
  if (!incremental) {
    hema_rankings <- get_ranking(incremental = FALSE)
  } else if (file.exists(file.path(path, "hema_rankings.csv"))) {
    hema_rankings <- readr::read_csv(file.path(path, "hema_rankings.csv"), show_col_types = FALSE)
  } else {
    hema_rankings <- HEMAscRaper::hema_rankings
  }
  
  # If current date is different from the max date in the dataset, get new ranking data
  # CHANGED: `.data$month_date`
  if (Sys.Date() != max(hema_rankings$month_date)) {
    new_rankings  <- get_ranking(incremental = TRUE)
    hema_rankings <- dplyr::bind_rows(hema_rankings, new_rankings)
  }
  
  # Incremental or non-incremental update of fights
  if (!incremental) {
    hema_fights <- get_fights()
  } else if (file.exists(file.path(path, "hema_fights.csv"))) {
    hema_fights <- readr::read_csv(file.path(path, "hema_fights.csv"), show_col_types = FALSE)
  } else {
    # This assumes HEMAscRaper::hema_fights is loaded
    hema_fights <- hema_fights
  }
  
  # Find the latest year in fight data (based on event_name)
  # CHANGED: .data$event_name in pipeline
  last_year <- hema_fights %>%
    dplyr::pull(.data$event_name) %>%   # Equivalent to hema_fights$event_name
    unique() %>%
    stringr::str_extract("\\b\\d{4}\\b") %>%
    unique() %>%
    as.integer() %>%
    max(na.rm = TRUE)
  
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  
  # Get missing years of data
  new_fights <- get_fights(last_year:current_year)
  
  # Filter out fights already present
  # CHANGED: .data$event_id
  old_fights <- hema_fights %>%
    dplyr::filter(!(.data$event_id %in% new_fights$event_id))
  
  message("Adding last known rank to fighter data")
  
  # CHANGED: .data$ in slice_max(), select(), filter()
  last_rank <- hema_rankings %>%
    dplyr::slice_max(.data$month_date) %>%
    dplyr::select(.data$fighter_id, .data$category, .data$rank, .data$weighted_rating) %>%
    dplyr::filter(.data$category == "Longsword (Mixed & Men's, Steel)") %>%
    tidyr::pivot_wider(
      names_from   = "category",             # typically pivot_wider can accept plain strings
      values_from  = c("rank", "weighted_rating")
    )
  
  # Left-join last known rank to fighter data
  hema_fighters <- hema_fighters %>%
    dplyr::left_join(last_rank, by = "fighter_id")
  
  # Combine new and old fight data
  # CHANGED: group_by(.data$event_id, .data$tournament_name), .data$match_id
  hema_fights <- dplyr::bind_rows(new_fights, old_fights) %>%
    dplyr::group_by(.data$event_id, .data$tournament_name) %>%
    dplyr::mutate(tournament_id = dplyr::cur_group_id()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(match_id = dplyr::row_number()) %>%
    dplyr::select(.data$match_id, .data$tournament_id, .data$event_id, dplyr::everything())
  
  message("Refining HEMA Matches")
  
  # We create the match results by stacking two data frames (fighter_1 perspective and fighter_2 perspective).
  # CHANGED: references to .data$stage, etc. in mutate()
  hema_match_results <- hema_fights %>%
    {
      result <- .
      dplyr::bind_rows(
        dplyr::rename(
          result,
          fighter_name  = fighter_1,
          opponent_name = fighter_2,
          result        = fighter_1_result
        ) %>%
          dplyr::select(-fighter_2_result),
        dplyr::rename(
          result,
          fighter_id    = opponent_id,
          opponent_id   = fighter_id,
          fighter_name  = fighter_2,
          opponent_name = fighter_1,
          result        = fighter_2_result
        ) %>%
          dplyr::select(-fighter_1_result)
      )
    } %>%
    dplyr::mutate(
      match_id  = dplyr::row_number(),
      is_final  = stringr::str_detect(
        stringr::str_to_lower(.data$stage),
        "^(final|grand final|gold|gold (meda|final|match|medal.*|round \\d+|silver match|.*))$"
      )
    ) %>%
    dplyr::left_join(
      dplyr::select(hema_fighters, .data$fighter_id, .data$fighter_club_id),
      by = dplyr::join_by(fighter_id)
    ) %>%
    dplyr::rename(club_id = .data$fighter_club_id) %>%
    dplyr::group_by(.data$fighter_id, .data$fighter_name) %>%
    dplyr::arrange(.data$fighter_id, .data$event_id) %>%
    dplyr::mutate(debut_fight = dplyr::row_number() == 1) %>%
    dplyr::ungroup() %>%
    # Here we select columns by numeric index (1:9) plus newly created columns:
    # If you prefer, you can do `select(.data$match_id, .data$tournament_id, ... )` explicitly
    dplyr::select(1:9, .data$is_final, .data$club_id, dplyr::everything())
  
  message("Refining HEMA Tournaments")
  
  # Summarize tournaments by grouping
  # CHANGED: .data$ references in group_by
  hema_tournaments <- hema_match_results %>%
    dplyr::group_by(
      .data$tournament_id,
      .data$tournament_name,
      .data$event_id,
      .data$tournament_category,
      .data$tournament_weapon,
      .data$tournament_note
    ) %>%
    dplyr::summarize(
      match_count   = dplyr::n_distinct(.data$match_id),
      fighter_count = dplyr::n_distinct(.data$fighter_id),
      .groups       = "drop"
    )
  
  # Save data as CSV files
  # (Make sure `hema_countries` is defined somewhere globally or in the package)
  data_list <- list(
    hema_clubs         = hema_clubs,
    hema_countries     = hema_countries,  # ← check that this object exists
    hema_events        = hema_events,
    hema_fighters      = hema_fighters,
    hema_fights        = hema_fights,
    hema_match_results = hema_match_results,
    hema_tournaments   = hema_tournaments,
    hema_rankings      = hema_rankings
  )
  
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }
  
  message(paste0("Saving the data in the ", path, " directory"))
  
  purrr::walk(names(data_list), function(name) {
    file_path <- file.path(path, paste0(name, ".csv"))
    readr::write_csv(data_list[[name]], file = file_path, na = "", quote="all")
  })
  
  message("calculating HEMA Achievements")
  generate_achievements(path, TRUE)
}