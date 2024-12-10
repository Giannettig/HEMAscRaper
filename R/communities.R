# 
# # Load necessary libraries silently
# suppressPackageStartupMessages(library(readr))
# suppressPackageStartupMessages(library(dplyr))
# suppressPackageStartupMessages(library(tidyverse))
# 
# 
# 
# 
# library(tidyverse)
# library(tidygraph)
# 
# ## Parametrization
# weight_threshold <- 4
# population_threshold <- 5
# community_algorithm <- "louvain"  # Example algorithm
# 
# # Create a graph object
# graph <- tbl_graph(nodes = nodes_2024, edges = edges_2024, directed = FALSE)
# 
# # Filter edges based on the weight threshold after building the graph
# filtered_graph <- graph |>
#   activate(edges) |>
#   filter(Weight >= weight_threshold) |>
#   activate(nodes) |>
#   filter(Population >= population_threshold) |>
#   activate(nodes) |>
#   # Using Louvain algorithm for community detection
#   mutate(Community = group_louvain()) |>
#   group_by(Community) |>
#   mutate(Community_Label = str_c(Label, collapse = ", "),
#          Community_population = sum(Population),
#          Community = as.factor(Community)) |>
#   ungroup()
# 
# ## Result Presentation
# # Print nodes with communities
# filtered_graph |> activate(edges) |> as_tibble() 
# communities<-filtered_graph |> activate(nodes) |> as_tibble()|>select(1,2,6,7,8)
# 
# # Tvorba palety
# 
# library(tidyverse)
# library(tidygraph)
# library(ggraph)
# library(ggforce)
# library(colorspace)
# 
# # Define extracted color palette
# palette_definition <- function(num_communities, filtered_graph) {
#   color_palette <- c(
#     "#000000",  # Dark cyberpunk background
#     "#1B9E77",  # Teal
#     "#D95F02",  # Orange
#     "#7570B3",  # Purple
#     "#E7298A",  # Pink
#     "#66A61E",  # Green
#     "#E6AB02",  # Yellow
#     "#A6761D",  # Brown
#     "#666666",  # Gray
#     "#00BFFF",  # Deep Sky Blue
#     "#DC143C",  # Electric Crimson
#     "#1E90FF",  # Neon Blue
#     "#7F00FF",  # Neon Purple
#     "#FF4500",  # Bright Red-Orange
#     "#FF00FF",  # Magenta
#     "#00FF00",  # Neon Green
#     "#FF6F61",  # Vivid Coral
#     "#FF69B4",  # Hot Pink
#     "#00FFFF",  # Aqua Cyan
#     "#40E0D0"   # Bright Turquoise
#   )
#   
#   # Ensure color palette matches the number of communities
#   if (num_communities > length(color_palette) - 1) {
#     color_palette <- c(
#       color_palette,
#       grDevices::rainbow(num_communities - length(color_palette) + 1)  # Add more colors dynamically
#     )
#   }
#   return(color_palette)
# }
# 
# # Define dark theme for visualization
# dark_theme <- theme_void() +
#   theme(
#     plot.background = element_rect(fill = "#1B1D2F", color = NA),
#     panel.background = element_rect(fill = "#1B1D2F", color = NA),
#     legend.background = element_rect(fill = "#1B1D2F", color = NA),
#     legend.text = element_text(color = "white"),
#     legend.title = element_text(color = "white"),
#     plot.title = element_text(color = "white", size = 16, face = "bold", hjust = 0.5),
#     plot.subtitle = element_text(color = "white", size = 12, hjust = 0.5)
#   )
# 
# # Generate layout with coordinates and prepare for visualization
# visualize_graph <- function(filtered_graph, color_palette, year) {
#   # Add line type based on Weight
#   filtered_graph <- filtered_graph |>
#     activate(edges) |>
#     mutate(Line_Type = case_when(
#       Weight <= 5 ~ "dotted",
#       Weight > 5 ~ "solid"
#     ))
#   
#   graph_layout <- create_layout(
#     filtered_graph,
#     layout = "fr",
#     niter = 1500       # More iterations for stabilization
#   )  |>
#     mutate(
#       x = x * 1.5,  # Increase horizontal spacing
#       y = y * 1.5   # Increase vertical spacing
#     )
#   
#   # Plot the graph with adjusted palette
#   ggraph(graph_layout) +
#     geom_edge_link(
#       aes(width = Weight, linetype = Line_Type, alpha = Weight),  # Line width now proportional to Weight
#       color = "#666666"
#     ) +
#     scale_edge_width(range = c(0.5, 5)) +  # Adjusted range for better proportionality
#     scale_edge_alpha(range = c(0.4, 1)) +
#     scale_edge_linetype_manual(
#       values = c("dotted" = "dotted",  "solid" = "solid"),
#       labels = c("5 or fewer travels",  "More than 15 travels")  # Add explanatory labels
#     ) +
#     geom_mark_hull(
#       aes(x = x, y = y, fill = as.factor(Community), group = as.factor(Community)),
#       alpha = 0.1, color = NA, expand = unit(5, "mm")  # Increase expansion for more spacing
#     ) +
#     geom_node_point(
#       aes(x = x, y = y, size = Population, fill = as.factor(Community)),
#       shape = 21, stroke = 0
#     ) +
#     geom_node_text(
#       aes(x = x, y = y, label = Label, color = as.factor(Community)),  # Country labels
#       repel = TRUE, nudge_y = 0.2, size = 3.5, max.overlaps = 10  # Increased nudge and overlap limit
#     ) +
#     annotate(
#       "text",
#       label = paste("Year:", year),  # Add year as title
#       x = Inf, y = Inf,  # Place in the top-right corner
#       hjust = 1, vjust = 1,  # Align text to the top-right
#       color = "#00FF00",  # Neon green
#       size = 6, fontface = "bold"  # Adjust size and font style
#     ) +
#     scale_size_continuous(range = c(5, 20)) +
#     scale_fill_manual(values = color_palette[-1]) +
#     scale_color_manual(values = lighten(color_palette[-1], 0.7)) +
#     dark_theme +
#     theme(
#       plot.margin = margin(20, 20, 20, 20),
#       legend.position = "bottom right",  # Adjust legend position
#       legend.title = element_blank(),
#       legend.text = element_blank(),
#       text = element_text(family = "Avenir Next")  # Specify font (adjust as needed)
#     )
# }
# 
# # Example usage
# num_communities <- filtered_graph |> 
#   activate(nodes) |> 
#   as_tibble() |> 
#   pull(Community) |> 
#   nlevels()
# 
# color_palette <- palette_definition(num_communities, filtered_graph)
# viz <- visualize_graph(filtered_graph, color_palette, year)
# viz
# 
# 
# 
# hema_countries<-hema_countries|>left_join(communities|>select(Label,Community), by=c("name"="Label"))
# hema_communities<-communities|>select(Community, Community_Label, Community_population)|>distinct|>
#   mutate(Community_Name=case_when(
#     Community_Label == "Australia, Hong Kong, New Zealand" ~ "Oceania",
#     Community_Label == "Austria, Canada, Finland, Iceland, Ireland, Mexico, Norway, Sweden, United Kingdom, United States of America" ~ "Core HEMA",
#     Community_Label == "Belgium, France, Netherlands, Switzerland" ~ "Western Europe",
#     Community_Label == "Bulgaria, Croatia, Greece, Romania, Serbia" ~ "Balkans",	
#     Community_Label == "Czechia, Germany, Hungary, Israel, Italy, Latvia, Poland, Slovakia, Ukraine" ~ "CE Europe",	
#     Community_Label == "Philippines, South Korea, Taiwan" ~ "East Asia",
#     Community_Label == "Kazakhstan, Russia" ~ "Central Asia",
#     Community_Label == "Portugal, Spain" ~ "Iberia",
#     TRUE ~ Community_Label
#   ))|>rename(
#     Active_Fighters_2024 = Community_population
#   )
