#' @title Generate Color Palette
#' @description Creates a customizable color palette of a specified size. 
#' The palette ensures enough distinct colors are available by combining a predefined base palette with dynamically generated colors if needed.
#'
#' @param palette_size An integer specifying the number of colors to generate.
#' @return A character vector of hex color codes.
#' @details The function starts with a predefined base palette of vibrant colors. 
#' If the requested size exceeds the base palette, additional colors are dynamically generated using `grDevices::rainbow()`.
#' 
#' @examples
#' \dontrun{
#' # Generate a palette with 5 colors
#' palette <- generate_palette(5)
#'
#' # Generate a palette with 25 colors (uses additional colors dynamically)
#' palette <- generate_palette(25)
#' }
#'
#' @import grDevices
#' @keywords internal

generate_palette <- function(palette_size) {
  base_palette <- c(
    "#000000", "#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E",
    "#E6AB02", "#A6761D", "#666666", "#00BFFF", "#DC143C", "#1E90FF",
    "#7F00FF", "#FF4500", "#FF00FF", "#00FF00", "#FF6F61", "#FF69B4",
    "#00FFFF", "#40E0D0"
  )
  if (palette_size > length(base_palette)) {
    additional_colors <- grDevices::rainbow(palette_size - length(base_palette))
    base_palette <- c(base_palette, additional_colors)
  }
  return(base_palette[1:palette_size])
}