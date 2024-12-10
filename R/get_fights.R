#' Download and Extract HEMA Fights Data by Year
#'
#' This function downloads data from the *HEMA Ratings* website and extracts detailed fight statistics
#' for events that occurred within a specified year or set of years. If no year is specified, the function
#' gathers fight data for all available events.
#'
#' @param year An optional vector of years (as integers) to filter the events. If `NULL`, data for all available years is fetched. Defaults to `NULL`.
#' @param raw Logical. If `TRUE`, returns the raw, unprocessed data as scraped from the website. 
#' If `FALSE` (default), the data is normalized for consistency (e.g., names, categories, and stages).
#'
#' @return A tibble containing detailed fight statistics for the specified events with columns that may include:
#' \describe{
#'   \item{\code{event_id}}{Unique identifier of the event. Integer.}
#'   \item{\code{event_name}}{Name of the event. Character.}
#'   \item{\code{tournament_name}}{Name of the specific tournament or discipline within the event. Character.}
#'   \item{\code{tournament_category}}{Category of the tournament (e.g., Mixed, Men's, Women's, Underrepresented Genders). Character.}
#'   \item{\code{event_weapon}}{Weapon used in the tournament (e.g., Longsword, Rapier). Character.}
#'   \item{\code{fighter_id}}{Unique identifier of the first fighter in a match. Integer.}
#'   \item{\code{opponent_id}}{Unique identifier of the opponent fighter in a match. Integer.}
#'   \item{\code{fighter_score}}{Score of the first fighter. Integer.}
#'   \item{\code{opponent_score}}{Score of the opponent fighter. Integer.}
#' }
#'
#' @details
#' This function scrapes the HEMA Ratings events page and compiles a list of all fights for the specified years.
#' If no `year` is provided, it extracts fight data for all events available on the website. For each event, it calls
#' the `.get_tournament()` function to gather detailed fight-level information.
#'
#' When the `raw` parameter is set to `FALSE`, the function normalizes data for consistency, ensuring uniformity
#' in names, categories, and other attributes. Missing data (e.g., anonymized fighters) is handled appropriately.
#'
#' @note
#' The function uses `rvest` for HTML parsing and `purrr` for handling multiple event pages. It performs basic error handling
#' to check if events matching the specified years are available. Depending on the number of events and the size of the tournament data,
#' the function may take some time to run.
#'
#' @examples
#' \dontrun{
#' # Download all HEMA fight data for the year 2022
#' fights_2022 <- get_fights(2022)
#'
#' # Download HEMA fight data for multiple years (e.g., 2019 and 2020)
#' fights_2019_2020 <- get_fights(c(2019, 2020))
#'
#' # Download HEMA fight data for all available years
#' all_fights <- get_fights()
#'
#' # Download raw fight data for 2022 without normalization
#' raw_fights_2022 <- get_fights(2022, raw = TRUE)
#' }
#'
#' @importFrom rvest read_html html_elements html_attr html_element
#' @importFrom purrr map_chr map_dfr is_empty
#' @importFrom stringr str_detect str_remove str_replace_all str_extract
#' @importFrom dplyr tibble mutate
#' @importFrom magrittr %>%
#' @importFrom dplyr if_else
#' @export
get_fights <- function(year = NULL, raw = FALSE) {
  
  events_url <- "https://hemaratings.com/events/"
  
  # Read the HTML content from the URL
  web_page <- rvest::read_html(events_url)
  
  # Determine years to filter
  if (!is.null(year)) {
    years_to_filter <- as.vector(year)
    year_conditions <- paste0("contains(@id, '", years_to_filter, "')", collapse = " or ")
    xpath_expr <- paste0("//*[contains(@class, 'panel-group') and (", year_conditions, ")]")
  } else {
    xpath_expr <- "//*[contains(@class, 'panel-group')]"
    years_to_filter <- "all time"
  }
  
  # Extract event URLs
  events_url <- web_page %>%
    rvest::html_elements(xpath = xpath_expr) %>%
    rvest::html_elements(".panel-body") %>%
    rvest::html_element("a") %>%
    rvest::html_attr("href") %>%
    purrr::map_chr(~ paste0("https://hemaratings.com", .))
  
  # Handle empty events_url case
  if (purrr::is_empty(events_url)) {
    message("The events_url vector is empty. There are no events matching the selected time range.")
    return(dplyr::tibble())
  } else {
    message(paste("Searching HEMA Ratings for Fights in:", paste(years_to_filter, collapse = ", ")))
    fights <- events_url %>% purrr::map_dfr(get_tournament, .progress = TRUE)
    
    if (!raw) {
      fights <- fights %>%
        dplyr::mutate(
          event_name = normalize_names(event_name),
          tournament_name = normalize_names(tournament_name),
          tournament_category = normalize_names(tournament_category),
          stage = normalize_names(stage),
          fighter_1 = dplyr::if_else(fighter_id == 0, NA, fighter_1) %>% normalize_names(),
          fighter_2 = dplyr::if_else(opponent_id == 0, NA, fighter_2) %>% normalize_names()
        )
    }
    
    return(fights)
  }
}
