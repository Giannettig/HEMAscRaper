#' Transform Country Names
#'
#' This function standardizes or transforms country names based on predefined rules.
#' It is intended to handle specific cases where country names or descriptions need
#' normalization or exclusion (e.g., removing non-country entries or renaming countries).
#'
#' @param country A character vector of country names or descriptions.
#'
#' @return A character vector with transformed country names. Entries that do not match
#' the predefined rules are returned unchanged. Non-country entries are replaced with `NA`.
#'
#' @examples
#'  \dontrun{
#' # Example usage:
#' countries <- c(
#'   "The nylon (Patreon Bronze-level subscriber)",
#'   "Czech Republic",
#'   "United States",
#'   "Germany"
#' )
#' normalize_countries(countries)
#' }
#' # [1] "Ukraine" NA "Czechia" "United States of America" "Germany"
#' @keywords internal
#' 
#' # Convert the column to UTF-8 encoding
normalize_countries <- function(country) {
  country <- iconv(country,from = "ISO-8859-1", to = "UTF-8", sub = "")%>%textclean::replace_non_ascii() # Remove non-ASCII characters
  dplyr::case_when(
    stringr::str_detect(tolower(country), "ukraine") ~ "Ukraine",
    country == "The nylon (Patreon Bronze-level subscriber)" ~ NA_character_,
    country == "The Lightsaber (For making a substantial contribution to HEMA Ratings)" ~ NA_character_,
    country == "Czech Republic" ~ "Czechia",
    country == "United States" ~ "United States of America",
    country == "United States Of America" ~ "United States of America",
    TRUE ~ country
  ) 
}