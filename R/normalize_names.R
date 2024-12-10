#' Normalize Names
#'
#' This function normalizes names by:
#' - Removing non-ASCII characters.
#' - Converting text to lowercase.
#' - Converting the text to title case.
#' - Trimming and squishing extra spaces.
#'
#' @param name A character vector of names to be cleaned and normalized.
#'
#' @return A character vector of cleaned and normalized names.
#'
#' @importFrom textclean replace_non_ascii
#' @importFrom stringr str_to_title str_squish
#'
#' @examples
#'  \dontrun{
#' # Example usage:
#' names <- c(
#'   "JOHN DOE  ", 
#'   "àLEX smith", 
#'   "MARY-jane"
#' )
#' normalize_names(names)
#' # [1] "John Doe"   "Alex Smith" "Mary-Jane"
#'}
#'
#' @keywords internal

normalize_names <- function(name) {
  name <- textclean::replace_non_ascii(name) |>
    tolower() |>
    str_to_title() |>
    str_squish()
  return(name)
} 