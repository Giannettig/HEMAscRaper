#' Generate Achievements
#'
#' @description
#' External function to generate achievements for HEMA ratings. This function
#' retrieves data from the specified path or uses package data as a fallback.
#' It computes achievements by invoking all functions prefixed with `ach_` in
#' the namespace of the `HEMAscRaper` package.
#'
#' @param path Character. Path to the folder containing HEMA data files. Defaults to \code{"./hema_ratings"}.
#' @param export_csv Logical. If \code{TRUE}, exports the achievements list as a CSV to \code{path}.
#'
#' @return A data frame containing all achievements, with columns:
#' \describe{
#'   \item{fighter_id}{ID of the fighter.}
#'   \item{tier_id}{Numeric tier ID (1-4).}
#'   \item{achieved}{Logical, \code{TRUE} if achieved.}
#'   \item{percentile}{Proportion of fighters who achieved this tier.}
#'   \item{achievement_tier}{Character. Tier name ("Bronze", "Silver", etc.).}
#'   \item{achievement_name}{Character. Name of the achievement.}
#'   \item{achievement_description}{Character. Description of the achievement.}
#'   \item{achievement_icon}{Character. Icon file name for the achievement.}
#' }
#'
#' @importFrom dplyr select filter left_join rename
#' @importFrom purrr imap_dfr
#' @importFrom readr read_csv write_csv
#' @importFrom stats setNames
#' @importFrom progress progress_bar
#'
#' @examples
#' \dontrun{
#' generate_achievements("./hema_ratings")
#' }
#'
#' @export
generate_achievements <- function(path = "./hema_ratings", export_csv = FALSE) {
  start_time <- Sys.time()
  
  # Check if the folder exists
  if (!dir.exists(path)) {
    message("The folder with data '", path, "' does not exist. Consider running refresh_hema_data() to refresh. Package data will be used.")
  }
  
  # Import input data
  hema_match_results <- if (file.exists(file.path(path, "hema_match_results.csv"))) {
    readr::read_csv(file.path(path, "hema_match_results.csv"), show_col_types = FALSE)
  } else {
    HEMAscRaper::hema_match_results
  }
  
  hema_clubs <- if (file.exists(file.path(path, "hema_clubs.csv"))) {
    readr::read_csv(file.path(path, "hema_clubs.csv"), show_col_types = FALSE)
  } else {
    HEMAscRaper::hema_clubs
  }
  
  hema_events <- if (file.exists(file.path(path, "hema_events.csv"))) {
    readr::read_csv(file.path(path, "hema_events.csv"), show_col_types = FALSE)
  } else {
    HEMAscRaper::hema_events
  }
  
  hema_fighters <- if (file.exists(file.path(path, "hema_fighters.csv"))) {
    readr::read_csv(file.path(path, "hema_fighters.csv"), show_col_types = FALSE)
  } else {
    HEMAscRaper::hema_fighters
  }
  
  hema_countries <- HEMAscRaper::hema_countries
  
  # Data preparation
  data <- hema_match_results %>%
    dplyr::select(-club_id) %>%
    dplyr::left_join(hema_fighters, by = c("fighter_id", "fighter_name")) %>%
    dplyr::left_join(hema_events, by = c("event_id", "event_name")) %>%
    dplyr::left_join(hema_countries, by = c("event_country" = "name"))%>%select(-id) %>%
    dplyr::rename(
      event_region = "region",
      event_sub_region = "sub_region",
      event_community = "community_label"
    ) %>%
    dplyr::left_join(
      hema_clubs %>% dplyr::select(-club_name),
      by = c("fighter_club_id" = "club_id")
    ) %>%
    dplyr::left_join(hema_countries%>%select(-id,-population,-community), by = c("club_country" = "name")) %>%
    dplyr::rename(
      club_region = "region",
      club_sub_region = "sub_region",
      club_community = "community_label"
    ) %>%
    dplyr::filter(
      .data$fighter_id != 0 &
        !is.na(.data$fighter_id) &
        !is.na(.data$event_id) &
        !is.na(.data$event_year)
    )
  
  # Retrieve all `ach_` prefixed functions from HEMAscRaper namespace
  achievement_list <- grep(
    "^ach_",
    ls(envir = asNamespace("HEMAscRaper"), all.names = TRUE),
    value = TRUE
  )
  
  achievement_functions <- stats::setNames(
    lapply(achievement_list, get, envir = asNamespace("HEMAscRaper")),
    achievement_list
  )
  
  # Columns expected in the final output
  required_cols <- c(
    "fighter_id", "tier_id", "achieved", "percentile",
    "achievement_tier", "achievement_name", "achievement_description", "achievement_icon"
  )
  
  # A zero-row tibble with all the necessary columns
  empty_achievement <- tibble::tibble(
    fighter_id = numeric(0),
    tier_id = numeric(0),
    achieved = logical(0),
    percentile = numeric(0),
    achievement_tier = character(0),
    achievement_name = character(0),
    achievement_description = character(0),
    achievement_icon = character(0)
  )
  
  # Keep track of achievements that return no results
  no_results <- character(0)
  
  # Progress bar setup
  pb <- progress::progress_bar$new(
    format = "[:bar] :current/:total (:percent) - :elapsed - Processing: :function",
    total = length(achievement_functions),
    clear = FALSE,
    width = 80
  )
  
  # Compute achievements with error and warning handling
  achievements <- purrr::imap_dfr(achievement_functions, function(fn, name) {
    pb$tick(tokens = list("function" = name))
    result <- tryCatch(
      withCallingHandlers(
        fn(data),
        warning = function(w) {
          message(sprintf("Warning in achievement function '%s': %s", name, w$message))
          invokeRestart("muffleWarning")  # Prevent warning from being rethrown
        }
      ),
      error = function(e) {
        warning(sprintf("Error in achievement function '%s': %s", name, e$message), call. = FALSE)
        NULL
      }
    )
    
    # If the function returned NULL or an empty data frame, record it and return an empty tibble
    if (is.null(result) || nrow(result) == 0) {
      no_results <<- c(no_results, name)
      return(empty_achievement)
    }
    
    # Otherwise, ensure all required columns are present.
    missing_cols <- setdiff(required_cols, names(result))
    if (length(missing_cols) > 0) {
      for (col in missing_cols) {
        # Match type from empty_achievement
        result[[col]] <- empty_achievement[[col]]
      }
    }
    
    # Reorder columns so they match the required order at the end
    result <- result[, union(required_cols, names(result))]
    
    return(result)
  })
  
  # End computation and print time
  end_time <- Sys.time()
  message(
    "Achievement generation completed in ",
    round(difftime(end_time, start_time, units = "secs"), 2),
    " seconds."
  )
  
  # Warn if any achievement function returned no results
  if (length(no_results) > 0) {
    warning(
      "The following achievement function(s) returned no results: ",
      paste(no_results, collapse = ", ")
    )
  }
  
  # Optionally export to CSV
  if (export_csv) {
    csv_path <- file.path(path, "hema_achievements.csv")
    readr::write_csv(achievements, csv_path)
    message(paste0("Achievements saved in the ", path, " folder."))
  }
  
  return(achievements)
}