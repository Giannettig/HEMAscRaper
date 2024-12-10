#' Retrieve Parent Club Information
#'
#' This function extracts the parent club ID of a given child club URL by parsing the associated web page.
#' 
#' @param children_url A character string representing the URL of the child club's web page.
#' 
#' @return A tibble with two columns:
#' \itemize{
#'   \item \code{club_id}: The numeric ID of the child club extracted from the given URL.
#'   \item \code{club_parent_id}: The numeric ID of the parent club extracted from the web page, or \code{NA} if not found.
#' }
#' 
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Sends an HTTP GET request to the provided \code{children_url}.
#'   \item Parses the HTML content of the web page.
#'   \item Extracts the hyperlink associated with the parent club from a table with the class \code{.table-striped}.
#'   \item Extracts the numeric IDs for both the child club and the parent club from their respective URLs.
#' }
#' 
#' @note
#' This function is designed to be a helper function within a larger workflow and is not intended for standalone use.
#' 
#' @examples
#' \dontrun{
#' # Example usage
#' children_url <- "https://example.com/club/12345"
#' parent_data <- .get_parent(children_url)
#' print(parent_data)
#' }
#' 
#' @importFrom httr RETRY
#' @importFrom rvest read_html html_element html_attr
#' @importFrom dplyr tibble
#' @importFrom stringr str_extract
#' @keywords internal
#' 

get_parent <- function(children_url) {
  # Send HTTP GET request and parse HTML content
  web_page <- httr::RETRY('GET', children_url) |> rvest::read_html()
  
  # Extract the parent club URL from the table
  parent_url <- tryCatch(
    web_page |> 
      rvest::html_element(".table-striped") |> 
      rvest::html_element("a") |> 
      rvest::html_attr("href"),
    error = function(e) NA_character_  # Return NA if the parent URL cannot be found
  )
  
  # Extract numeric IDs for the child and parent clubs
  parent <- dplyr::tibble(
    club_id = stringr::str_extract(children_url, "\\d+") |> as.numeric(),
    club_parent_id = if (!is.na(parent_url)) {
      stringr::str_extract(parent_url, "\\d+") |> as.numeric()
    } else {
      NA_real_
    }
  )
  
  return(parent)
}
