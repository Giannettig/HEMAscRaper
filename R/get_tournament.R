#' Scrape Tournament Data from HEMA Ratings
#'
#' Internal function to scrape data from a specified HEMA Ratings tournament page.
#' Extracts detailed fight statistics, including tournament metadata and match-level data.
#'
#' @param tournament_url Character string. URL of the specific tournament page on the HEMA Ratings website.
#'
#' @return A tibble containing detailed fight statistics with the following columns:
#' \describe{
#'   \item{\code{event_id}}{Unique identifier for the event. Integer.}
#'   \item{\code{event_name}}{Name of the event. Character.}
#'   \item{\code{tournament_name}}{Name of the specific tournament or discipline within the event. Character.}
#'   \item{\code{tournament_category}}{Category of the tournament (e.g., Mixed, Men's, Women's, Underrepresented Genders). Character.}
#'   \item{\code{tournament_note}}{Additional notes about the tournament (e.g., special rules). Character.}
#'   \item{\code{tournament_weapon}}{Weapon used in the tournament (e.g., Longsword, Rapier). Character.}
#'   \item{\code{fighter_id}}{Unique identifier for the first fighter in each match. Integer.}
#'   \item{\code{opponent_id}}{Unique identifier for the opponent in each match. Integer.}
#'   \item{\code{stage}}{Stage of the tournament (e.g., Pool, Finals). Character.}
#'   \item{\code{fighter_score}}{Score of the first fighter. Character.}
#'   \item{\code{opponent_score}}{Score of the opponent. Character.}
#' }
#'
#' @details
#' This function scrapes the specified tournament page on the HEMA Ratings website, extracting match-level information.
#' The data includes event metadata, tournament categories, weapon types, and anonymized fighter placeholders where necessary.
#'
#' The function assumes the page structure on the HEMA Ratings website remains consistent. Any changes in layout may require updates to the scraping logic.
#'
#' @note
#' This is an internal utility function and is not intended for direct use. It relies on `httr`, `rvest`, and `dplyr` for web scraping and data manipulation.
#'
#' @importFrom httr RETRY
#' @importFrom rvest read_html html_elements html_text html_attr html_element html_table
#' @importFrom dplyr tibble bind_cols bind_rows mutate
#' @importFrom purrr map2
#' @importFrom stringr str_detect str_extract str_remove str_replace_all str_squish
#' @importFrom snakecase to_snake_case
get_tournament <- function(tournament_url) {
  
  # Retrieve the tournament page
  web_page <- httr::RETRY('GET', tournament_url) %>%
    rvest::read_html()
  
  # Extract tournament names and metadata
  result <- web_page %>%
    rvest::html_elements(".panel-default") %>%
    rvest::html_elements(".event-title") %>%
    rvest::html_text()
  
  result_index <- result %>% stringr::str_detect("fights") %>% which()
  
  tournaments <- result[result_index] %>% {
    dplyr::tibble(
      tournament_name = stringr::str_extract(., ".*(?= - )"),
      discipline = dplyr::if_else(
        stringr::str_detect(., "\\("),
        stringr::str_extract(., ".*(?= \\()"),
        stringr::str_extract(., ".*(?= - )")
      ),
      fights = stringr::str_extract(., "(?<= - ).*") %>%
        stringr::str_remove("fights") %>%
        as.numeric()
    )
  }
  
  # Process fight tables for each tournament
  tournament_tables <- web_page %>%
    as.character() %>%
    stringr::str_replace_all(., "Anonymous fighter", '<a href="/fighters/details/0000/">Anonymous fighter</a>') %>%
    rvest::read_html() %>%
    rvest::html_elements(".panel-default") %>%
    rvest::html_element("table") %>%
    .[result_index] %>%
    purrr::map2(result[result_index], function(x, y) {
      row <- x
      dplyr::tibble(
        event_id = stringr::str_extract(tournament_url, "\\d+") %>% as.numeric(),
        event_name = web_page %>% rvest::html_element("h2") %>% rvest::html_text(),
        tournament_name = y,
        tournament_category = y %>% stringr::str_extract("Mixed|Men's|Women's|Underrepresented Genders"),
        tournament_note = y %>% stringr::str_extract("\\(.*\\)") %>% stringr::str_remove_all("[\\(\\)]"),
        tournament_weapon = y %>% stringr::str_remove_all("Mixed|Men's|Women's|Underrepresented Genders|\\(.*\\)|-\\s*\\d+\\s*fight[s]?") %>% stringr::str_squish(),
        fighter_id = row %>% rvest::html_elements("a") %>%
          { links <- .; links[seq(1, length(links), 2)] } %>%
          rvest::html_attr("href") %>%
          stringr::str_extract("\\d+") %>%
          as.numeric(),
        opponent_id = row %>% rvest::html_elements("a") %>%
          { links <- .; links[seq(2, length(links), 2)] } %>%
          rvest::html_attr("href") %>%
          stringr::str_extract("\\d+") %>%
          as.numeric()
      ) %>% dplyr::bind_cols(row %>% rvest::html_table()) %>%
        dplyr::mutate(stage = as.character(Stage))
    }) %>%
    dplyr::bind_rows()%>%dplyr::select(-Stage)
  
  names(tournament_tables) <- snakecase::to_snake_case(names(tournament_tables))
  
  return(tournament_tables)
}
