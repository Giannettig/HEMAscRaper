#' @title Generate Communities Graph
#' @description Generates a network graph from HEMA-related datasets, representing clubs, events, and matches. 
#' Filters the data to include only edges with sufficient weight and nodes with a minimum population. 
#' Applies community detection using the Louvain algorithm to group nodes into communities based on their connections.
#' 
#' ## Result Presentation
#' After generating the graph, you can inspect the filtered edges and nodes, as well as explore the detected communities:
#' 
#' @param year A numeric value, vector of numeric values, or NULL. Filters the data to include only matches from the specified year(s). Default: NULL (all years).
#' @param tournament_weapon A character vector or NULL. Filters the data to include only matches involving the specified tournament weapon(s). Default: NULL (all weapons).
#' @param weight_threshold An integer. Minimum weight for edges (connections between nodes) to be included in the graph. Default: 5.
#' @param population_threshold An integer. Minimum population size for nodes (clubs) to be included in the graph. Default: 10.
#' @param hema_clubs A dataset containing HEMA clubs. Default: `HEMAscRaper::hema_clubs`.
#' @param hema_events A dataset containing HEMA events. Default: `HEMAscRaper::hema_events`.
#' @param hema_match_results A dataset containing HEMA match results. Default: `HEMAscRaper::hema_match_results`.
#' 
#' @return A `tbl_graph` object with nodes and edges representing the filtered data, including detected communities and their attributes.
#' 
#' @examples
#' # Example 1: Generate a graph for 2024 with default thresholds and "Steel Longsword" weapon
#' graph <- generate_communities_graph(
#'   year = 2024,
#'   tournament_weapon = "Steel Longsword"
#' )
#' 
#' # Example 2: Generate a graph for multiple years with custom thresholds
#' graph <- generate_communities_graph(
#'   year = c(2023, 2024),
#'   tournament_weapon = "Steel Longsword",
#'   weight_threshold = 10,
#'   population_threshold = 20
#' )
#' 
#' # Example 3: View nodes and communities
#' library(dplyr)
#' nodes_with_communities <- graph %>%
#'   tidygraph::activate(nodes) %>%
#'   as_tibble() %>%
#'   select(Id, Label, Population, Community, Community_Label)
#' 
#' # Example 4: View filtered edges
#' library(dplyr)
#' library(tidygraph)
#' filtered_edges <- graph %>%
#'   activate(edges) %>%
#'   as_tibble()
#' 
#' @export
#' @import dplyr tidygraph stringr HEMAscRaper

generate_communities_graph <- function(
    year = NULL, 
    tournament_weapon = NULL, 
    weight_threshold = 0, 
    population_threshold = 0, 
    hema_clubs = HEMAscRaper::hema_clubs, 
    hema_events = HEMAscRaper::hema_events, 
    hema_match_results = HEMAscRaper::hema_match_results
) {
  # Generate travel data
  travel_data <- generate_travel_data(
    year = year, 
    tournament_weapon = tournament_weapon, 
    hema_clubs = hema_clubs, 
    hema_events = hema_events, 
    hema_match_results = hema_match_results
  )
  
  # Create a graph object
  graph <- tidygraph::tbl_graph(
    nodes = travel_data$nodes, 
    edges = travel_data$edges, 
    directed = FALSE
  )
  
  # Filter edges and nodes, and apply community detection
  hema_communities_graph <- graph %>%
    tidygraph::activate(edges) %>%
    dplyr::filter(Weight >= weight_threshold) %>%
    tidygraph::activate(nodes) %>%
    dplyr::filter(Population >= population_threshold) %>%
    tidygraph::mutate(
      Community = tidygraph::group_louvain()
    ) %>%
    dplyr::group_by(Community) %>%
    dplyr::mutate(
      Community_Label = stringr::str_c(Label, collapse = ", "),
      Community_population = sum(Population),
      Community = as.factor(Community)
    ) %>%
    dplyr::ungroup()
  
  # Set year attribute for the graph
  hema_communities_graph$year <- if (is.null(year)) "All time" else year
  # Set weapon attribute for the graph
  hema_communities_graph$tournament_weapon <- if (is.null(tournament_weapon)) "All weapons" else tournament_weapon
  
  hema_communities_graph$weight_threshold <- weight_threshold
    
    hema_communities_graph$population_threshold <-population_threshold 
  
  return(hema_communities_graph)
}