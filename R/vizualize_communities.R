#' @title Visualize HEMA Communities
#' @description Generates a visual representation of a graph object, applying community detection and customizable layouts.
#' Filters edges by weight and applies a color palette to distinguish communities. Provides an aesthetically styled plot.
#'
#' @param graph A `tbl_graph` object from the `tidygraph` package, representing nodes and edges of the graph.
#' @param color_palette A character vector specifying custom colors for communities. If `NULL`, a default palette is used.
#' @param layout_type A character string specifying the layout algorithm. Options include `"fr"` (default), `"kk"`, `"mds"`, and `"circle"`.
#' @param weight_threshold_dotted A numeric value specifying the threshold for dotted line edges. Default: 5.
#' @param show_labels boolean - turn off the grapgh description.
#'
#' @return A ggplot object representing the visualized graph.
#' @examples
#' \dontrun{
#' # Example 1: Basic graph visualization with default settings
#' library(dplyr)
#' generate_communities_graph( tournament_weapon = "Steel Longsword", 
#'                             year=2024,
#'                            weight_threshold = 5, 
#'                            population_threshold = 15)%>%
#' visualize_communities()
#'
#' # Example 2: Custom layout and color palette
#' custom_palette <- c("#1B9E77", "#D95F02", "#7570B3")
#' visualize_communities(graph, year = 2024, color_palette = custom_palette, layout_type = "kk")
#' }
#'
#' @importFrom tidygraph activate
#' @importFrom dplyr as_tibble
#' @import ggraph
#' @importFrom ggforce geom_mark_hull
#' @import ggplot2 
#' @importFrom scales rescale
#' @importFrom colorspace lighten
#' @export

visualize_communities <- function(
    graph,
    color_palette = NULL,
    layout_type = "fr",
    weight_threshold_dotted = 5,
    show_labels=TRUE
) {
  # Validate layout_type
  valid_layouts <- c("fr", "kk", "mds", "circle")
  if (!layout_type %in% valid_layouts) {
    stop("Invalid layout_type. Choose from: 'fr', 'kk', 'mds', 'circle'.")
  }
  
  # Calculate number of communities
  num_communities <- tidygraph::activate(graph, nodes) %>%
    dplyr::as_tibble() %>%
    dplyr::pull(Community) %>%
    base::nlevels()
  
  # Generate default palette if none is provided
  if (is.null(color_palette)) {
    color_palette <- generate_palette(num_communities*2)
  }
  
  # Define dark theme for the plot
  dark_theme <- ggplot2::theme_void() +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "#1B1D2F", color = NA),
      panel.background = ggplot2::element_rect(fill = "#1B1D2F", color = NA),
      legend.background = ggplot2::element_rect(fill = "#1B1D2F", color = NA),
      legend.text = ggplot2::element_text(color = "white"),
      legend.title = ggplot2::element_text(color = "white"),
      plot.title = ggplot2::element_text(color = "white", size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = ggplot2::element_text(color = "white", size = 12, hjust = 0.5)
    )
  
  # Add line type based on Weight
  graph <- tidygraph::activate(graph, edges) %>%
    dplyr::mutate(
      Line_Type = dplyr::if_else(Weight <= weight_threshold_dotted, "dotted", "solid"),
      Width_Scaled = scales::rescale(Weight, to = c(0.5, 5))
    )
  
  # Create layout for visualization
  graph_layout <- ggraph::create_layout(
    graph,
    layout = layout_type,
    niter = if (layout_type == "fr") 1500 else NULL  # Specific to force-directed layout
  ) %>%
    dplyr::mutate(
      x = x * 1.5,
      y = y * 1.5
    )
  
  # Generate the graph visualization
  g<-ggraph::ggraph(graph_layout) +
    ggraph::geom_edge_link(
      ggplot2::aes(width = Weight, linetype = Line_Type, alpha = Weight),
      color = "#666666"
    ) +
    ggraph::scale_edge_width(range = c(0.5, 5)) +
    ggraph::scale_edge_alpha(range = c(0.4, 1)) +
    ggraph::scale_edge_linetype_manual(
      values = c("dotted" = "dotted", "solid" = "solid"),
      labels = c("5 or fewer travels", "More than 5 travels")
    ) +
    ggforce::geom_mark_hull(
      ggplot2::aes(x = x, y = y, fill = as.factor(Community), group = as.factor(Community)),
      alpha = 0.1, color = NA, expand = ggplot2::unit(5, "mm")
    ) +
    ggraph::geom_node_point(
      ggplot2::aes(x = x, y = y, size = Population, fill = as.factor(Community)),
      shape = 21, stroke = 0
    ) +
    ggraph::geom_node_text(
      ggplot2::aes(x = x, y = y, label = Label, color = as.factor(Community)),
      repel = TRUE, nudge_y = 0.2, size = 3.5, max.overlaps = 30
    )  +
    ggplot2::scale_size_continuous(range = c(5, 20)) +
    ggplot2::scale_fill_manual(values = color_palette[-1]) +
    ggplot2::scale_color_manual(values = colorspace::lighten(color_palette[-1], 0.7)) +
    dark_theme +
    ggplot2::theme(
      plot.margin = ggplot2::margin(20, 20, 20, 20),
      legend.position = "bottom right",
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_blank(),
      text = ggplot2::element_text(family = "Avenir Next")
    )
  
  # Add annotations conditionally based on `show_labels`
  if (show_labels) {
    g <- g +
    # Title
    ggplot2::annotate(
      "text",
      label = "HEMA Communities",
      x = Inf, y = Inf,
      hjust = 1, vjust = 2,
      color = "#00FF00",
      size = 6, fontface = "bold"
    ) +
    # Subtitle with tabular information
    ggplot2::annotate(
      "text",
      label = paste(
        "Year: ", graph$year, "\n",
        "Weapon: ", graph$tournament_weapon, "\n",
        "Min Travels: ", graph$weight_threshold, "\n",
        "Min Country Fencers: ", graph$population_threshold
      ),
      x = Inf, y = Inf,
      hjust = 1, vjust = 1.8,  # Adjust `vjust` to position the table below the title
      color = "white",
      size = 4
    )
    
  }
  
  print(g)
}