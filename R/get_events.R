#' Download and Extract HEMA Events Data
#'
#' This function downloads data from the *HEMA Ratings* website and extracts detailed information
#' about historical European martial arts events, including their dates, locations, and corresponding URLs.
#'
#' @param raw Logical. If `TRUE`, returns the raw, unprocessed data as scraped from the website. 
#' If `FALSE` (default), the data is normalized for consistency (e.g., names, countries, and cities).
#'
#' @return A tibble containing events' details with the following columns:
#' \describe{
#'   \item{\code{event_id}}{Unique identifier of the event. Integer.}
#'   \item{\code{event_brand}}{Name of the event brand. Character.}
#'   \item{\code{event_name}}{Name of the event. Character.}
#'   \item{\code{event_year}}{Year in which the event took place. Integer.}
#'   \item{\code{event_date}}{Date of the event in "YYYY-MM-DD" format. Date.}
#'   \item{\code{event_country}}{Country where the event was held. Character.}
#'   \item{\code{event_city}}{City where the event took place, if available. Character.}
#'   \item{\code{event_url}}{URL of the event's page on the HEMA Ratings website. Character.}
#' }
#'
#' @details
#' This function scrapes the HEMA Ratings events page and compiles a list of all HEMA events, including
#' the event name, date, country, and other relevant details. When the `raw` parameter is set to `FALSE`,
#' the data is normalized to ensure consistency in names, countries, and cities. 
#'
#' If city information is missing for an event, the function defaults it to `NA`.
#'
#' The function assumes the structure of the HEMA Ratings webpage remains consistent. If the webpage layout changes,
#' this function may need adjustments.
#'
#' @examples
#' \dontrun{
#' # Download HEMA events data and display the first few rows
#' events_data <- get_events()
#' head(events_data)
#'
#' # Retrieve raw, unprocessed events data
#' raw_events_data <- get_events(raw = TRUE)
#' }
#'
#' @importFrom rvest read_html html_elements html_text html_attr html_element
#' @importFrom dplyr tibble select mutate
#' @importFrom purrr map map_chr
#' @importFrom stringr str_remove str_extract
#' @importFrom lubridate as_date
#' @importFrom magrittr %>%
#' @export
get_events <- function(raw = FALSE) {
  
  events_url <- "https://hemaratings.com/events/"
  message("Searching HEMA Ratings for Events")
  
  # Read the HTML content from the URL
  web_page <- rvest::read_html(events_url)
  
  events <- web_page %>%
    rvest::html_elements(".panel-group") %>%
    .[1:length(.)] %>%
    {
      row <- .
      dplyr::tibble(
        event_name = row %>% rvest::html_elements(".event-title") %>% rvest::html_text() %>% stringr::str_remove(".*-\\s*"),
        event_brand = event_name %>% stringr::str_remove("\\s*\\d{4}$"),
        event_day = row %>% rvest::html_elements(".panel-body") %>% purrr::map(~ rvest::html_elements(.x, "dd")[1]) %>% 
          purrr::map_chr(~ rvest::html_text(.), .progress = TRUE),
        event_year = row %>% rvest::html_attr("id") %>% stringr::str_extract("\\d+") %>% as.numeric(),
        event_date = paste(event_year, event_day, " ") %>% as.Date(format = "%Y %B %d"),
        event_country = row %>% rvest::html_elements(".panel-body") %>% purrr::map(~ rvest::html_elements(.x, "dd")[2]) %>% 
          purrr::map_chr(~ rvest::html_text(.)),
        event_city = row %>% rvest::html_elements(".panel-body") %>% purrr::map(~ rvest::html_elements(.x, "dd") %>%
                                                                                  { links <- .; ifelse(length(links) < 3, NA, links[3] %>% rvest::html_text(.)) }) %>% as.character(),
        event_url = row %>% rvest::html_elements(".panel-body") %>% rvest::html_element("a") %>% rvest::html_attr("href"),
        event_id = stringr::str_extract(event_url, "\\d+") %>% as.numeric()
      ) %>% dplyr::select(event_id, event_brand, event_year, event_name, event_date, event_country, event_city, event_url)
    }
  
  if (!raw) {
    events <- events %>%
      dplyr::mutate(
        event_brand = normalize_events(event_brand),
        event_name = normalize_names(event_name),
        event_country = normalize_countries(event_country),
        event_city = normalize_names(event_city)
      )
  }
  
  return(events)
}