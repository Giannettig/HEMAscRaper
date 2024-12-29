#' @title Retrieve HEMA Ranking Data
#'
#' @description
#' Downloads HEMA ranking data from multiple ranking URLs and combines it into a single data frame.
#' By default (\code{incremental = TRUE}), it fetches a snapshot of rankings for each base URL,
#' attaching the current system date. When \code{incremental = FALSE}, it attempts a more
#' exhaustive retrieval (including all months for each ranking URL), which can be time-consuming.
#'
#' @details
#' - If \code{incremental = FALSE}, the function iterates over all month options for each
#'   ranking URL, potentially resulting in a very large number of page fetches.
#'   (This process could take a few hours, depending on network and server speed.)
#' - If \code{incremental = TRUE}, it fetches only the base URLs and assigns the current date
#'   to the \code{month_date} column, resulting in a “snapshot” of the current standings.
#'
#' @param incremental Logical. If \code{TRUE}, fetches rankings from the base URLs only,
#'   labeling them with the current date. If \code{FALSE}, iterates over all known month
#'   options for each base URL (this can be very slow).
#'
#' @return A tibble (data frame) of ranking data with columns such as:
#' \itemize{
#'   \item \code{rank}
#'   \item \code{fighter_id}
#'   \item \code{fighter_name}
#'   \item \code{category}
#'   \item \code{month}
#'   \item \code{weighted_rating}
#'   \item \code{club}
#'   \item \code{month_date}
#' }
#' (Exact columns depend on \code{\link{get_ranking_page}}.)
#'
#' @importFrom rvest read_html html_element html_text html_elements html_attr
#' @importFrom dplyr mutate
#' @importFrom purrr map map_chr map_df
#' @importFrom stringr str_split_1
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#'   # Fast incremental ranking retrieval
#'   rank_data <- get_ranking(incremental = TRUE)
#'   head(rank_data)
#'
#'   # Comprehensive retrieval (can take hours!)
#'   rank_data_full <- get_ranking(incremental = FALSE)
#' }
#'
#' @export
get_ranking <- function(incremental = TRUE) {
  
  # Build base URLs for up to 12 rating sets (1..12).
  ranking_urls <- paste0("https://hemaratings.com/periods/details/?ratingsetid=", 1:12)
  
  # Use the first URL to extract all possible month "option" values.
  web_page <- rvest::read_html(ranking_urls[1])
  
  # This might be used for debugging or for further logic:
  ranking_page <- web_page %>%
    rvest::html_element("h2") %>%
    rvest::html_text()
  
  # Build query parameters for each available month in the dropdown
  months_links <- web_page %>%
    rvest::html_element("#selectMonth") %>%
    rvest::html_elements("option") %>%
    rvest::html_attr("value") %>%
    purrr::map_chr(function(x) {
      parts <- stringr::str_split_1(x, "-")
      paste0("&year=", parts[1], "&month=", parts[2])
    })
  
  # If incremental = FALSE, build a URL for every combination of base URL and month link
  # Otherwise, just fetch each base URL once and add the current date
  if (!incremental) {
    # This process can be very long (potentially hours)
    links <- purrr::map(ranking_urls, ~ paste0(.x, months_links)) %>%
      unlist()
    
    hema_rankings <- purrr::map_df(links, get_ranking_page, .progress = TRUE)
  } else {
    # Fetch the base URLs only, label them with today's date
    hema_rankings <- purrr::map_df(ranking_urls, get_ranking_page, .progress = TRUE) %>%
      dplyr::mutate(month_date = Sys.Date())
  }
  
  return(hema_rankings)
}