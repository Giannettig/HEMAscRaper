#' Normalize Event Names
#'
#' This function normalizes event names by:
#' - Removing non-ASCII characters.
#' - Converting text to lowercase.
#' - Removing unwanted patterns such as ordinal numbers, Roman numerals, and event-specific identifiers.
#' - Converting the text to title case.
#' - Trimming and squishing extra spaces.
#'
#' @param name A character vector of event names to be cleaned and normalized.
#'
#' @return A character vector of cleaned and normalized event names.
#'
#' @importFrom textclean replace_non_ascii
#' @importFrom stringr str_remove_all str_to_title str_squish
#'
#' @examples
#'  \dontrun{
#' # Example usage:
#' event_names <- c(
#'   "1st Capitol Clash '22",
#'   "V. Annual Clash #123",
#'   "Capitol Clash Event 2023"
#' )
#' normalize_events(event_names)
#' # [1] "Capitol Clash" "Annual Clash" "Capitol Clash"
#' }
#'
#' @keywords internal
normalize_events <- function(name) {
  name <- textclean::replace_non_ascii(name) |> # Remove non-ASCII characters
    tolower() |> # Convert to lowercase
    stringr::str_remove_all(
      "^\\d+\\.?\\s*|^(\\d+|\\d+[\u00BA\u00AA]|1st|2nd|3rd|\\d+th|\\d+st|\\d+nd|\\d+rd)\\s*|^(iv|v|vi{1,3}|vii|viii|ix|x{1,3}|l|c|d|m)\\s*|#\\d+\\s*$|\\s*event \\d+$|\\s*'\\d{2}\\s*$|\\s*\\d+$"
    ) |> # Remove unwanted patterns
    stringr::str_to_title() |> # Convert to title case
    stringr::str_squish() # Trim and remove extra spaces
  return(name)
}