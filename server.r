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

# Function to extract state from hometown
get_state <- function(hometown) {
  parts <- strsplit(hometown, ",")[[1]]
  if (length(parts) >= 2) {
    state <- trimws(parts[length(parts)])
    return(state)
  }
  return(NA)
}

# Add state column to contestants data
contestants <- contestants %>%
  mutate(state = sapply(Hometown, get_state))

# Add state column to geocoded data
bachelor_geocoded <- bachelor_geocoded %>%
  mutate(state = sapply(city_state, get_state))

bachelorette_geocoded <- bachelorette_geocoded %>%
  mutate(state = sapply(city_state, get_state))

# Get unique states for dropdowns
bachelor_states <- sort(unique(bachelor_geocoded$state[!is.na(bachelor_geocoded$state)]))
bachelorette_states <- sort(unique(bachelorette_geocoded$state[!is.na(bachelorette_geocoded$state)]))

server <- function(input, output, session) {

  # Update state choices for Bachelor
  observe({
    updateSelectInput(session, "bachelor_state",
                      choices = c("All States" = "all", setNames(bachelor_states, bachelor_states)))
  })
  
  # Update state choices for Bachelorette
  observe({
    updateSelectInput(session, "bachelorette_state",
                      choices = c("All States" = "all", setNames(bachelorette_states, bachelorette_states)))
  })
  
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
  
  # Filtered Bachelor data based on state selection
  bachelor_filtered_data <- reactive({
    data <- bachelor_geocoded %>% 
      filter(!is.na(latitude) & !is.na(longitude))
    
    if (input$bachelor_state != "all") {
      data <- data %>% filter(state == input$bachelor_state)
    }
    
    data
  })
  
  # Filtered Bachelorette data based on state selection
  bachelorette_filtered_data <- reactive({
    data <- bachelorette_geocoded %>% 
      filter(!is.na(latitude) & !is.na(longitude))
    
    if (input$bachelorette_state != "all") {
      data <- data %>% filter(state == input$bachelorette_state)
    }
    
    data
  })
  
  # Render Bachelor Map
  output$bachelor_map <- renderLeaflet({
    data <- bachelor_filtered_data() %>% 
      filter(!is.na(latitude) & !is.na(longitude))
    
    map <- leaflet(data) %>%
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
      )   
    
    # Zoom to state or show full USA
    if (input$bachelor_state != "all" && nrow(data) > 0) {
      map <- map %>% fitBounds(
        lng1 = min(data$longitude) - 1,
        lat1 = min(data$latitude) - 1,
        lng2 = max(data$longitude) + 1,
        lat2 = max(data$latitude) + 1
      )
    } else {
      map <- map %>% setView(lng = -98.5795, lat = 39.8283, zoom = 4)
    }
    
    map
  })
  
  
  # Render Bachelorette Map
  output$bachelorette_map <- renderLeaflet({
    data <- bachelorette_filtered_data %>% 
      filter(!is.na(latitude) & !is.na(longitude))
  
    
      map <- leaflet(data) %>%
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
      ) 
    
    # Zoom to state or show full USA
    if (input$bachelorette_state != "all" && nrow(data) > 0) {
      map <- map %>% fitBounds(
        lng1 = min(data$longitude) - 1,
        lat1 = min(data$latitude) - 1,
        lng2 = max(data$longitude) + 1,
        lat2 = max(data$latitude) + 1
      )
    } else {
      map <- map %>% setView(lng = -98.5795, lat = 39.8283, zoom = 4)
    }
    
    map
  })
  
  # Display Bachelor contestants list
  output$bachelor_contestants_list <- renderUI({
    if (input$bachelor_state == "all") {
      return(p("Select a state to see contestants", style = "color: #888;"))
    }
    
    state_contestants <- contestants %>%
      filter(Show == "The Bachelor", state == input$bachelor_state) %>%
      arrange(desc(Eliminated))
    
    if (nrow(state_contestants) == 0) {
      return(p("No contestants from this state", style = "color: #888;"))
    }
    
    contestant_items <- lapply(1:nrow(state_contestants), function(i) {
      contestant <- state_contestants[i, ]
      tags$div(
        style = "border-bottom: 1px solid #ddd; padding: 10px 0;",
        tags$strong(contestant$Name),
        tags$br(),
        tags$small(paste0("Hometown: ", contestant$Hometown)),
        tags$br(),
        tags$small(paste0("Eliminated: ", contestant$Eliminated)),
        tags$br(),
        tags$small(paste0("Job: ", contestant$Job))
      )
    })
    
    tags$div(contestant_items)
  })
  
  # Display Bachelorette contestants list
  output$bachelorette_contestants_list <- renderUI({
    if (input$bachelorette_state == "all") {
      return(p("Select a state to see contestants", style = "color: #888;"))
    }
    
    state_contestants <- contestants %>%
      filter(Show == "The Bachelorette", state == input$bachelorette_state) %>%
      arrange(desc(Eliminated))
    
    if (nrow(state_contestants) == 0) {
      return(p("No contestants from this state", style = "color: #888;"))
    }
    
    contestant_items <- lapply(1:nrow(state_contestants), function(i) {
      contestant <- state_contestants[i, ]
      tags$div(
        style = "border-bottom: 1px solid #ddd; padding: 10px 0;",
        tags$strong(contestant$Name),
        tags$br(),
        tags$small(paste0("Hometown: ", contestant$Hometown)),
        tags$br(),
        tags$small(paste0("Eliminated: ", contestant$Eliminated)),
        tags$br(),
        tags$small(paste0("Job: ", contestant$Job))
      )
    })
    
    tags$div(contestant_items)
  })
}





