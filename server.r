library(shiny)
library(ggplot2)
library(tidyverse)
library(leaflet)

# Load pre-geocoded data (loads instantly!)
bachelor_geocoded <- read.csv("bachelor_map_data.csv", stringsAsFactors = FALSE)
bachelorette_geocoded <- read.csv("bachelorette_map_data.csv", stringsAsFactors = FALSE)



# Load the CSV files (load once when app starts)
contestants <- read.csv("contestants.csv", stringsAsFactors = FALSE)
seasons <- read.csv("seasons.csv", stringsAsFactors = FALSE)

server <- function(input, output, session) {
  
  # When splash screen is clicked, go to split screen
  observeEvent(input$splash_click, {
    updateTabsetPanel(session, "tabs", selected = "Choose")
  })
  
  # When button is clicked, go to data upload (or wherever you want)
  observeEvent(input$start_btn, {
    updateTabsetPanel(session, "tabs", selected = "Upload Data")
  })
  
  # Navigate from Bachelor side to Bachelor Map
  observeEvent(input$bachelor_click, {
    updateTabsetPanel(session, "tabs", selected = "Bachelor Map")
  })
  
  # Navigate from Bachelorette side to Bachelorette Map
  observeEvent(input$bachelorette_click, {
    updateTabsetPanel(session, "tabs", selected = "Bachelorette Map")
  })
  

  # Create color palette function for Bachelor (red shades)
  bachelor_color_pal <- function(avg_week) {
    # Darker red for longer-lasting contestants
    colorNumeric(
      palette = c("#FFCCCB", "#FF6B6B", "#DC143C", "#8B0000"),
      domain = c(1, 11)
    )(avg_week)
  }
  
  # Create color palette function for Bachelorette (pink shades)
  bachelorette_color_pal <- function(avg_week) {
    # Darker pink for longer-lasting contestants
    colorNumeric(
      palette = c("#FFE4E1", "#FFB6C1", "#FF69B4", "#C71585"),
      domain = c(1, 11)
    )(avg_week)
  }
  
  # Render Bachelor Map
  output$bachelor_map <- renderLeaflet({
    data <- bachelor_geocoded %>% 
      filter(!is.na(latitude) & !is.na(longitude))
    
    
    leaflet(data) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        radius = ~sqrt(count) * 5,  # Larger circles for more contestants
        fillColor = ~bachelor_color_pal(avg_week),
        fillOpacity = 0.7,
        color = "#8B0000",
        weight = 2,
        popup = ~paste0(
          "<strong>", city_state, "</strong><br>",
          "Number of contestants: ", count, "<br>",
          "Average week eliminated: ", round(avg_week, 1), "<br>",
          "Longest lasting: Week ", max_week
        )
      ) %>%
      setView(lng = -98.5795, lat = 39.8283, zoom = 4)  # Center on USA
  })
  
  # Render Bachelorette Map
  output$bachelorette_map <- renderLeaflet({
    data <- bachelorette_geocoded %>% 
      filter(!is.na(latitude) & !is.na(longitude))
  
    
    leaflet(data) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        radius = ~sqrt(count) * 5,  # Larger circles for more contestants
        fillColor = ~bachelorette_color_pal(avg_week),
        fillOpacity = 0.7,
        color = "#C71585",
        weight = 2,
        popup = ~paste0(
          "<strong>", city_state, "</strong><br>",
          "Number of contestants: ", count, "<br>",
          "Average week eliminated: ", round(avg_week, 1), "<br>",
          "Longest lasting: Week ", max_week
        )
      ) %>%
      setView(lng = -98.5795, lat = 39.8283, zoom = 4)  # Center on USA
  })
}




