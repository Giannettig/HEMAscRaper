#' @title Retrieve HEMA Rankings Page with Month and Category Details
#' @description Scrapes the HEMA rankings page for a specified month and extracts table data along with fighter details, including category and month metadata.
#' @param ranking_month_url A character string representing the URL of the rankings page for the desired month.
#' @return A tibble containing the final rankings table with fighter details and metadata. Columns are:
#' - **rank**: Fighter rank.
#' - **nationality**: Fighter's nationality.
#' - **points**: Points earned.
#' - **fighter_url**: URL to the fighter's profile.
#' - **fighter_name**: Fighter's name.
#' - **fighter_id**: Unique identifier for the fighter.
#' - **category**: Competition category (e.g., "Mixed & Men's Steel").
#' - **month**: Month of the ranking period (e.g., "December 2024").
#' - **months_links**: Links to other available months in the dropdown.
#' @details This function uses the `chromote` package to interact with the rankings page, selecting the "All" option in the dropdown to load all rows. It then parses the HTML using `rvest`, extracts fighter details, metadata (category and month), and returns a clean table.
#' @importFrom chromote ChromoteSession
#' @importFrom rvest read_html html_element html_elements html_table html_text html_attr
#' @importFrom dplyr tibble select bind_cols mutate left_join
#' @importFrom stringr str_extract str_split_1 str_squish
#' @importFrom snakecase to_snake_case
#' @importFrom testthat expect_identical
#' @keywords internal
#' @examples
#' \dontrun{
#'   ranking_month_url <- "https://hemaratings.com/periods/details/?ratingsetid=1&year=2024&month=12"
#'   rankings_table <- get_ranking_page(ranking_month_url)
#'   print(rankings_table)
#' }
get_ranking_page <- function(ranking_month_url) {
  
  # Debug: Ensure the URL is valid
  stopifnot(is.character(ranking_month_url), length(ranking_month_url) == 1)
  
  # Create a new Chromote session
  b <- chromote::ChromoteSession$new()
  
  # Navigate to the rankings page
  b$Page$navigate(ranking_month_url)
  
  # Wait for the dropdown and table to load
  Sys.sleep(1)
  
  # Set the dropdown to "All" to load all rows
  b$Runtime$evaluate("
    document.querySelector('#dt-length-0').value = '-1';
    document.querySelector('#dt-length-0').dispatchEvent(new Event('change'));
  ")
  
  # Scrape the fully rendered page content
  page_content <- b$Runtime$evaluate("document.body.innerHTML")$result$value
  
  # Close the Chromote session
  b$close()
  
  # Parse the HTML content with rvest
  web_page <- rvest::read_html(page_content)
  
  # Extract month links and text from the dropdown
  month_date <- web_page %>%
    rvest::html_element("#selectMonth") %>%
    rvest::html_elements("option") %>%
    rvest::html_attr("value")
  
  months_text <- web_page %>%
    rvest::html_element("#selectMonth") %>%
    rvest::html_elements("option") %>%
    rvest::html_text()
  
  months <- dplyr::tibble(month_date, months_text)
  
  # Extract rankings page header for category and month information
  ranking_page <- web_page %>%
    rvest::html_element("h2") %>%
    rvest::html_text()
  
  # Extract the main table from the page
  main_table <- web_page %>%
    rvest::html_element("#mainTable") %>%   # Select table by ID
    rvest::html_table() %>%
    dplyr::select(1, 3, 5, 6)  # Adjust column indices as needed
  
  # Extract fighter details (URLs, names, IDs)
  fighters_ids <- web_page %>%
    rvest::html_element("#mainTable") %>%
    rvest::html_elements("tr") %>%
    .[2:length(.)] %>%  # Skip the header row
    {
      row <- .
      dplyr::tibble(
        fighter_url  = rvest::html_element(row, "a") %>% rvest::html_attr("href"),
        fighter_name = rvest::html_element(row, "a") %>% rvest::html_text(),
        fighter_id   = stringr::str_extract(fighter_url, "\\d+") %>% as.numeric()
      )
    }
  
  # Verify that names in main_table and fighters_ids match
  # (Because these are direct $-accesses outside a dplyr pipeline, .data$ is not needed)
  testthat::expect_identical(main_table$Name, fighters_ids$fighter_name)
  
  # Combine main table and fighter details with metadata
  final_table <- dplyr::bind_cols(
    # CHANGED: reference "Name" with .data$ in the pipeline if removing it
    main_table %>% dplyr::select(-.data$Name),  
    fighters_ids %>%
      dplyr::mutate(
        category = stringr::str_split_1(ranking_page, "-") %>%
          stringr::str_squish() %>%
          .[1],
        month = stringr::str_split_1(ranking_page, "-") %>%
          stringr::str_squish() %>%
          .[2]
      ) %>%
      dplyr::left_join(months, by = c("month" = "months_text"))
  )
  
  # Clean column names to snake_case
  names(final_table) <- snakecase::to_snake_case(names(final_table))
  
  # CHANGED: in the final select, refer to columns with .data$
  return(
    final_table %>%
      dplyr::select(
        .data$rank,
        .data$fighter_id,
        .data$fighter_name,
        .data$category,
        .data$month,
        .data$weighted_rating,
        .data$club,
        .data$month_date
      )
  )
}
