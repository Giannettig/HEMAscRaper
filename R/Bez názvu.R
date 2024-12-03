# Load required libraries
library(shiny)
library(plotly)
library(dplyr)
library(sf)
library(tidygraph)

# UI
ui <- fluidPage(
  titlePanel("Interactive Travel Connections Map"),
  plotlyOutput("interactiveMap", height = "800px"),
  verbatimTextOutput("selectedCountry")
)

# Server
server <- function(input, output, session) {
  
  # Prepare edges with coordinates
  edges_with_coords <- reactive({
    filtered_edges %>%
      mutate(
        Source_Long = nodes_with_community$longitude[match(Source, nodes_with_community$Id)],
        Source_Lat = nodes_with_community$latitude[match(Source, nodes_with_community$Id)],
        Target_Long = nodes_with_community$longitude[match(Target, nodes_with_community$Id)],
        Target_Lat = nodes_with_community$latitude[match(Target, nodes_with_community$Id)]
      )
  })
  
  # Render interactive map
  output$interactiveMap <- renderPlotly({
    plot_ly() %>%
      # Add edges (lines of travel)
      add_segments(
        data = edges_with_coords(),
        x = ~Source_Long, xend = ~Target_Long,
        y = ~Source_Lat, yend = ~Target_Lat,
        line = list(color = 'gray', width = ~Weight / 5),
        hoverinfo = "none",
        showlegend = FALSE
      ) %>%
      # Add nodes (countries)
      add_markers(
        data = nodes_with_community,
        x = ~longitude, y = ~latitude,
        color = ~community,
        size = ~Population,
        text = ~paste0(
          "<b>Country:</b> ", Label, "<br>",
          "<b>Population:</b> ", Population, "<br>",
          "<b>Community:</b> ", community, "<br>",
          "<b>Community Members:</b> ", Community_Countries
        ),
        hoverinfo = "text",
        marker = list(opacity = 0.8)
      ) %>%
      layout(
        title = "Interactive Map of Fencers' Communities",
        geo = list(
          showland = TRUE,
          landcolor = "rgb(229, 229, 229)",
          showcountries = TRUE,
          countrycolor = "rgb(200, 200, 200)"
        )
      )
  })
  
  # Highlight connections on country click
  observeEvent(event_data("plotly_click"), {
    clicked_data <- event_data("plotly_click")
    
    if (!is.null(clicked_data)) {
      selected_country <- clicked_data$text %>%
        strsplit("<b>Country:</b> ") %>%
        .[[1]][2] %>%
        strsplit("<br>") %>%
        .[[1]][1]
      
      # Show selected country in the text output
      output$selectedCountry <- renderText({
        paste("Selected Country:", selected_country)
      })
      
      # Filter edges for the selected country
      highlight_edges <- edges_with_coords() %>%
        filter(Source_Label == selected_country | Target_Label == selected_country)
      
      # Update the plot dynamically
      output$interactiveMap <- renderPlotly({
        plot_ly() %>%
          # Add highlighted edges (lines of travel)
          add_segments(
            data = highlight_edges,
            x = ~Source_Long, xend = ~Target_Long,
            y = ~Source_Lat, yend = ~Target_Lat,
            line = list(color = 'red', width = 2),
            hoverinfo = "none",
            showlegend = FALSE
          ) %>%
          # Add all nodes
          add_markers(
            data = nodes_with_community,
            x = ~longitude, y = ~latitude,
            color = ~community,
            size = ~Population,
            text = ~paste0(
              "<b>Country:</b> ", Label, "<br>",
              "<b>Population:</b> ", Population, "<br>",
              "<b>Community:</b> ", community, "<br>",
              "<b>Community Members:</b> ", Community_Countries
            ),
            hoverinfo = "text",
            marker = list(opacity = 0.8)
          ) %>%
          layout(
            title = "Interactive Map of Fencers' Communities",
            geo = list(
              showland = TRUE,
              landcolor = "rgb(229, 229, 229)",
              showcountries = TRUE,
              countrycolor = "rgb(200, 200, 200)"
            )
          )
      })
    }
  })
}

# Run the Shiny app
shinyApp(ui, server)