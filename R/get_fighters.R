#' Download and Extract HEMA Fighters Data
#'
#' This function downloads data from the *HEMA Ratings* website and extracts detailed information
#' about historical European martial arts fighters, including their names, nationalities, and associated clubs.
#'
#' @param raw Logical. If `TRUE`, returns the raw, unprocessed data. If `FALSE` (default), normalizes names and nationalities for consistency.
#'
#' @return A tibble containing fighters' details with the following columns:
#' \describe{
#'   \item{\code{fighter_id}}{Unique identifier of the fighter. Integer.}
#'   \item{\code{fighter_name}}{Name of the fighter. Character.}
#'   \item{\code{fighter_nationality}}{Nationality of the fighter (country flag displayed). Character.}
#'   \item{\code{fighter_club_id}}{Unique identifier of the club, if available. Integer.}
#'   \item{\code{fighter_club_name}}{Name of the fighter's club, if available. Character.}
#'   \item{\code{fighter_url}}{URL of the fighter's profile on the HEMA Ratings website. Character.}
#' }
#'
#' @details
#' This function scrapes the HEMA Ratings website, specifically the fighters table, and extracts
#' relevant details for each fighter. It uses the `rvest` and `dplyr` packages to parse the HTML structure and
#' organize the extracted information into a tibble. If the `raw` parameter is set to `FALSE`, the function 
#' normalizes the data to ensure consistency in names and nationalities.
#'
#' @note
#' The function assumes the structure of the HEMA Ratings webpage remains consistent. If the webpage layout changes,
#' this function may break.
#'
#' @examples
#' \dontrun{
#' # Download HEMA fighters data and display the first few rows
#' fighters_data <- get_fighters()
#' head(fighters_data)
#'
#' # Retrieve raw, unprocessed fighters data
#' raw_fighters_data <- get_fighters(raw = TRUE)
#' }
#'
#' @importFrom rvest read_html html_element html_elements html_attr html_text
#' @importFrom dplyr tibble select
#' @importFrom purrr map_chr
#' @importFrom stringr str_extract
#' @importFrom magrittr %>%
#' @export
get_fighters <- function(raw = FALSE) {
  
  fighters_url <- "https://hemaratings.com/fighters/"
  message("Searching HEMA Ratings for Fighters")
  
  # Read the HTML content from the URL
  web_page <- rvest::read_html(fighters_url)
  
  fighters <- web_page %>%
    rvest::html_element("table") %>%
    rvest::html_elements("tr") %>%
    .[2:length(.)] %>%
    {
      row <- .
      dplyr::tibble(
        fighter_url = rvest::html_element(row, "a") %>% rvest::html_attr("href"),
        fighter_name = rvest::html_element(row, "a") %>% rvest::html_text(),
        fighter_id = stringr::str_extract(fighter_url, "\\d+") %>% as.numeric(),
        fighter_nationality = rvest::html_element(row, "i") %>% rvest::html_attr("title"),
        fighter_club_url = row %>% purrr::map_chr(~ rvest::html_elements(.x, "a") %>%
                                                    { links <- .; ifelse(length(links) < 2, NA, links[2] %>% rvest::html_attr("href")) },.progress = TRUE),
        fighter_club_id = stringr::str_extract(fighter_club_url, "\\d+") %>% as.numeric(),
        fighter_club_name = row %>% purrr::map_chr(~ rvest::html_elements(.x, "a") %>%
                                                     { links <- .; ifelse(length(links) < 2, NA, links[2] %>% rvest::html_text()) },.progress = TRUE)
      ) %>% dplyr::select(fighter_id, fighter_name, fighter_nationality, fighter_club_id, fighter_club_name, fighter_url)
    }
  
  if (!raw) {
    fighters <- fighters %>% mutate(
      fighter_name = normalize_names(fighter_name),
      fighter_nationality = normalize_countries(fighter_nationality),
      fighter_club_name = normalize_names(fighter_club_name)
    )
  }
  
  return(fighters)
}