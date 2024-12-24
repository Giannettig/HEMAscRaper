#' Generate Achievements
#'
#' @description
#' External function to generate achievements for HEMA ratings. This function
#' retrieves data from the specified path or uses package data as a fallback.
#' It computes achievements by invoking all functions prefixed with `ach_` in
#' the namespace of the `HEMAscRaper` package.
#'
#' @param path Character. Path to the folder containing HEMA data files. Defaults to `./hema_ratings`.
#' @param export_csv Bool. Will export the achievements list as CSV to Path.
#'
#' @return A data frame containing all achievements, with columns:
#' \describe{
#'   \item{fighter_id}{ID of the fighter.}
#'   \item{tier_id}{Numeric tier ID (1-4).}
#'   \item{achieved}{Logical, TRUE if achieved.}
#'   \item{percentile}{Proportion of fighters who achieved this tier.}
#'   \item{achievement_tier}{Character. Tier name ("Bronze", "Silver", etc.).}
#'   \item{achievement_name}{Character. Name of the achievement.}
#'   \item{achievement_description}{Character. Description of the achievement.}
#'   \item{achievement_icon}{Character. Icon file name for the achievement.}
#' }
#'
#' @importFrom dplyr select filter left_join rename
#' @importFrom purrr imap_dfr
#' @importFrom readr read_csv
#' @importFrom stats setNames
#' @importFrom progress progress_bar
#' @examples
#' generate_achievements("./hema_ratings")
#' @export
generate_achievements <- function(path = "./hema_ratings", export_csv=FALSE) {
  start_time <- Sys.time()

  # Check if the folder exists
  if (!dir.exists(path)) {
    message("The folder with data '", path, "' does not exist. Consider running refresh_hema_data() to refresh. Package data will be used.")
  }

  # Import input data
  hema_match_results <- if (file.exists(file.path(path, "hema_match_results.csv"))) {
    readr::read_csv(file.path(path, "hema_match_results.csv"),show_col_types = FALSE)
  } else {
    HEMAscRaper::hema_match_results
  }

  hema_clubs <- if (file.exists(file.path(path, "hema_clubs.csv"))) {
    readr::read_csv(file.path(path, "hema_clubs.csv"),show_col_types = FALSE)
  } else {
    HEMAscRaper::hema_clubs
  }

  hema_events <- if (file.exists(file.path(path, "hema_events.csv"))) {
    readr::read_csv(file.path(path, "hema_events.csv"),show_col_types = FALSE)
  } else {
    HEMAscRaper::hema_events
  }

  hema_fighters <- if (file.exists(file.path(path, "hema_fighters.csv"))) {
    readr::read_csv(file.path(path, "hema_fighters.csv"),show_col_types = FALSE)
  } else {
    HEMAscRaper::hema_fighters
  }

  hema_countries <- HEMAscRaper::hema_countries

  # Data preparation
  data <- hema_match_results %>%
    dplyr::select(-club_id) %>%
    dplyr::left_join(hema_fighters, by = c("fighter_id", "fighter_name")) %>%
    dplyr::left_join(hema_events, by = c("event_id", "event_name")) %>%
    dplyr::left_join(hema_countries, by = c("event_country" = "name")) %>%
    dplyr::rename(event_region = region,
                  event_sub_region = sub_region,
                  event_community = community_label) %>%
    dplyr::left_join(hema_clubs %>% dplyr::select(-club_name), by = c("fighter_club_id" = "club_id")) %>%
    dplyr::left_join(hema_countries, by = c("club_country" = "name")) %>%
    dplyr::rename(club_region = region,
                  club_sub_region = sub_region,
                  club_community = community_label) %>%
    dplyr::filter(fighter_id != 0 & !is.na(fighter_id) & !is.na(event_id) & !is.na(event_year))  # Filter out anonymous fighter data

  # Retrieve all `ach_` prefixed functions from HEMAscRaper namespace
  achievement_list <- grep("^ach_", ls("package:HEMAscRaper"), value = TRUE)
  achievement_functions <- stats::setNames(lapply(achievement_list, get, envir = asNamespace("HEMAscRaper")), achievement_list)

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
    tryCatch(
      withCallingHandlers(
        fn(data),
        warning = function(w) {
          message(sprintf("Warning in achievement function '%s': %s", name, w$message))
          invokeRestart("muffleWarning")  # Prevent warning from being rethrown
        }
      ),
      error = function(e) {
        warning(sprintf("Error in achievement function '%s': %s", name, e$message), call. = FALSE)
        NULL  # Return NULL in case of error
      }
    )
  })

  # End computation and print time
  end_time <- Sys.time()
  message("Achievement generation completed in ", round(difftime(end_time, start_time, units = "secs"), 2), " seconds.")
  if(export_csv==TRUE){
  readr::write_csv(achievements, paste0(path,"/hema_achievements.csv"))
    message(paste0("Achievements saved in the ", path, " folder."))}
  achievements
}
#
