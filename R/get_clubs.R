
#' Download and Extract HEMA Clubs Data
#'
#' This function downloads data from the *HEMA Ratings* website and extracts detailed information
#' about historical European martial arts clubs, including their locations, member counts, and parent organizations.
#'
#' @param raw Logical. If `TRUE`, returns the raw, unprocessed data. If `FALSE` (default), normalizes names and countries for consistency.
#'
#' @return A tibble containing clubs' details with the following columns:
#' \describe{
#'   \item{\code{club_id}}{Unique identifier of the club. Integer.}
#'   \item{\code{club_name}}{Name of the club. Character.}
#'   \item{\code{club_country}}{Country where the club is located. Character.}
#'   \item{\code{club_state}}{State or region of the club (if applicable). Character.}
#'   \item{\code{club_city}}{City where the club is based. Character.}
#'   \item{\code{club_members}}{Number of registered fighters associated with the club. Integer.}
#'   \item{\code{club_parent_id}}{Unique identifier of the parent club, if the club is a sub-organization. Integer.}
#'   \item{\code{club_url}}{URL of the club's profile on the HEMA Ratings website. Character.}
#' }
#'
#' @details
#' This function scrapes the HEMA Ratings clubs page and compiles a list of all clubs with relevant metadata.
#' It identifies parent-child club relationships and merges them with additional club details such as state,
#' city, and number of members. If the `raw` parameter is set to `FALSE`, the function normalizes names,
#' countries, and states to ensure consistency.
#'
#' @note
#' The function assumes the structure of the HEMA Ratings webpage remains consistent. If the webpage layout changes,
#' this function may need adjustments.
#'
#' @examples
#' \dontrun{
#' # Download HEMA clubs data and display the first few rows
#' clubs_data <- get_clubs()
#' head(clubs_data)
#'
#' # Retrieve raw, unprocessed clubs data
#' raw_clubs_data <- get_clubs(raw = TRUE)
#' }
#'
#' @importFrom rvest read_html html_element html_elements html_attr html_text html_table
#' @importFrom dplyr tibble select left_join rename mutate
#' @importFrom purrr map_dfr
#' @importFrom stringr str_extract str_replace_all str_squish str_remove
#' @importFrom magrittr %>%
#' @export
get_clubs <- function(raw = FALSE) {
  
  clubs_url <- "https://hemaratings.com/clubs/"
  
  # Read the HTML content from the URL
  message("Searching HEMA Ratings for Clubs")
  web_page <- rvest::read_html(clubs_url)
  
  clubs_meta <- web_page %>%
    rvest::html_element("table") %>%
    rvest::html_elements("tr") %>%
    .[2:length(.)] %>%
    {
      row <- .
      dplyr::tibble(
        club_url = rvest::html_element(row, "a") %>% rvest::html_attr("href"),
        club_name = rvest::html_element(row, "a") %>% rvest::html_text() %>% stringr::str_squish(),
        club_id = stringr::str_extract(club_url, "\\d+") %>% as.numeric(),
        club_country = rvest::html_element(row, "i") %>% rvest::html_attr("title")
      )
    }
  
  clubs_table <- web_page %>%
    rvest::html_element("table") %>%
    rvest::html_table() %>%
    dplyr::select(-Country) %>%
    dplyr::mutate(Name = Name %>% stringr::str_remove("^\\s*-|-$") %>% stringr::str_squish()) %>%
    dplyr::rename(
      club_name = Name,
      club_state = State,
      club_city = City,
      club_members = Fighters
    ) %>% .[!duplicated(.$club_name), ]
  
  # Deal with sub-clubs and their parent organizations
  children_url <- web_page %>%
    as.character() %>%
    stringr::str_replace_all(., " - <a", '<a class="children"') %>%
    rvest::read_html() %>%
    rvest::html_element("table") %>%
    rvest::html_elements(".children") %>%
    rvest::html_attr("href") %>% paste0("http://hemaratings.com", .)
  
  parent_clubs <- purrr::map_dfr(children_url, get_parent, .progress = TRUE)
  
  clubs <- clubs_meta %>%
    dplyr::left_join(parent_clubs, by = "club_id") %>%
    dplyr::left_join(clubs_table, by = "club_name") %>%
    dplyr::select(club_id, club_name, club_country, club_state, club_city, club_members, club_parent_id, club_url)
  
  if (!raw) {
    clubs <- clubs %>% mutate(
      club_country = normalize_countries(club_country),
      club_state = normalize_names(club_state)
    )
  }
  
  return(clubs)
}