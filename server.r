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
    updateTabsetPanel(session, "tabs", selected = "Survey")
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
    data <- bachelorette_filtered_data() %>% 
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
  
 
 
  


    # Populate state dropdown based on gender selection
    observeEvent(input$survey_gender, {
      req(input$survey_gender)
      if (input$survey_gender == "female") {
        # Bachelor contestant - get states from Bachelor data
        states <- contestants %>%
          filter(Show == "The Bachelor", !is.na(state)) %>%
          pull(state) %>%
          unique() %>%
          sort()
        
      } else {
        # Bachelorette contestant - get states from Bachelorette data
        states <- contestants %>%
          filter(Show == "The Bachelorette", !is.na(state)) %>%
          pull(state) %>%
          unique() %>%
          sort()
        
      }
      
      updateSelectizeInput(session, "survey_state", 
                           choices = states,
                           server = TRUE)
    })
    
    # Populate occupation dropdown based on gender selection
    observeEvent(input$survey_gender, {
      req(input$survey_gender)
      if (input$survey_gender == "female") {
        # Bachelor contestant
        occupations <- contestants %>%
          filter(Show == "The Bachelor", !is.na(Job)) %>%
          pull(Job) %>%  # ← FIXED! Now pulling Job
          unique() %>%
          sort()
      } else {
        occupations <- contestants %>%
          filter(Show == "The Bachelorette", !is.na(Job)) %>%
          pull(Job) %>%  # ← FIXED! Now pulling Job
          unique() %>%
          sort()
      }
      
      
      updateSelectizeInput(session, "survey_occupation", 
                           choices = occupations,
                           server = TRUE)
    })
    
    # Calculate chances when survey is submitted
    observeEvent(input$submit_survey, {
      # Validate inputs
      req(input$survey_gender, input$survey_state, input$survey_occupation, input$survey_age)
      
      # Determine which show
      show_name <- if (input$survey_gender == "female") "The Bachelor" else "The Bachelorette"
      
      # Helper function to get week number
      get_week_num <- function(eliminated) {
        if (grepl("Winner", eliminated, ignore.case = TRUE)) {
          return(11)
        } else if (grepl("Runner", eliminated, ignore.case = TRUE)) {
          return(10)
        } else if (grepl("Week", eliminated, ignore.case = TRUE)) {
          week_num <- as.numeric(gsub(".*Week ([0-9]+).*", "\\1", eliminated))
          return(week_num)
        } else {
          return(1)
        }
      }
      
      
      # Get all contestants from the same show
      show_contestants <- contestants %>%
        filter(Show == show_name) %>%
        mutate(week_number = sapply(Eliminated, get_week_num)) 
          
      
      
      # Filter contestants with similar characteristics
      similar_contestants <- contestants %>%
        filter(Show == show_name,
               state == input$survey_state | is.na(state),
               Job == input$survey_occupation | is.na(Job)) %>%
        mutate(age_diff = abs(Age - input$survey_age))
      
      # Calculate week number for each contestant
      get_week_number <- function(eliminated) {
        if (grepl("Winner", eliminated, ignore.case = TRUE)) {
          return(11)
        } else if (grepl("Runner", eliminated, ignore.case = TRUE)) {
          return(10)
        } else if (grepl("Week", eliminated, ignore.case = TRUE)) {
          week_num <- as.numeric(gsub(".*Week ([0-9]+).*", "\\1", eliminated))
          return(week_num)
        } else {
          return(1)
        }
      }
      
      similar_contestants <- similar_contestants %>%
        mutate(week_number = sapply(Eliminated, get_week_number))
      
      # Calculate predictions based on different factors
      
      # 1. State-based prediction
      state_contestants <- similar_contestants %>%
        filter(state == input$survey_state)
      
      state_avg <- if (nrow(state_contestants) > 0) {
        mean(state_contestants$week_number, na.rm = TRUE)
      } else {
        mean(similar_contestants$week_number, na.rm = TRUE)
      }
      
      # 2. Occupation-based prediction
      job_contestants <- similar_contestants %>%
        filter(Job == input$survey_occupation)
      
      job_avg <- if (nrow(job_contestants) > 0) {
        mean(job_contestants$week_number, na.rm = TRUE)
      } else {
        mean(similar_contestants$week_number, na.rm = TRUE)
      }
      
      # 3. Age-based prediction
      age_contestants <- similar_contestants %>%
        arrange(age_diff) %>%
        head(20)  # Top 20 closest in age
      
      age_avg <- if (nrow(age_contestants) > 0) {
        mean(age_contestants$week_number, na.rm = TRUE)
      } else {
        mean(similar_contestants$week_number, na.rm = TRUE)
      }
      
      # Combined prediction (weighted average)
      predicted_week <- round((state_avg * 0.3 + job_avg * 0.4 + age_avg * 0.3), 1)
      
      # ADD THIS - guard against NA
      if (is.na(predicted_week)) predicted_week <- round(mean(show_contestants$week_number, na.rm = TRUE), 1)
      if (is.na(predicted_week)) predicted_week <- 5  # fallback if still NA
      
      # Convert to success percentage (out of 11 weeks)
      success_percentage <- round((predicted_week / 11) * 100)
      
      # Generate result message
      output$survey_results <- renderUI({
        # --- Stats for the paragraph ---
        
        # Number of contestants from user's state
        state_count <- show_contestants %>%
          filter(!is.na(state), state == input$survey_state) %>%
          nrow()
        
        # Age success %: % of close-age contestants who won or were runner-up
        age_success_pct <- show_contestants %>%
          mutate(age_diff = abs(Age - input$survey_age)) %>%
          filter(age_diff <= 3) %>%
          summarise(pct = round(mean(week_number >= 10, na.rm = TRUE) * 100)) %>%
          pull(pct)
        
        if (length(age_success_pct) == 0 || is.na(age_success_pct)) age_success_pct <- 0
        
        
        # Number of contestants with same occupation
        job_count <- show_contestants %>%
          filter(!is.na(Job), Job == input$survey_occupation) %>%
          nrow()
        
        # Overall success % (combined)
        overall_success_pct <- success_percentage
        
        # Predicted week message
        # --- Check for perfect match first ---
        perfect_match <- show_contestants %>%
          filter(
            state == input$survey_state,
            Job == input$survey_occupation,
            Age == input$survey_age
          )
        
        if (nrow(perfect_match) > 0) {
          # Use the best-performing perfect match
          best_match <- perfect_match %>% arrange(desc(week_number)) %>% slice(1)
          predicted_week <- best_match$week_number
          success_percentage <- 100
          
        } else {
          # Combined prediction (weighted average)
          predicted_week <- round((state_avg * 0.3 + job_avg * 0.4 + age_avg * 0.3), 1)
          
          # Guard against NA
          if (is.na(predicted_week)) predicted_week <- round(mean(show_contestants$week_number, na.rm = TRUE), 1)
          if (is.na(predicted_week)) predicted_week <- 5
          
          # Convert to success percentage (out of 11 weeks)
          success_percentage <- round((predicted_week / 11) * 100)
        }
        
        # Week message - place this AFTER the if/else block, BEFORE renderUI
        week_msg <- if (predicted_week >= 10.5) {
          "Congratulations, you've found love!"
        } else {
          paste0("You'd likely make it ", round(predicted_week), " week", if (round(predicted_week) != 1) "s" else "", "!")
        }
        
        tags$div(
          style = "margin-top: 30px; padding: 30px; background: linear-gradient(135deg, #FFE4E1 0%, #FFF5F5 100%); border-radius: 15px; text-align: center;",
          
          h3("Your Results!", style = "color: #DC143C; font-family: 'Lobster', cursive; font-size: 36px;"),
          
          tags$div(
            style = "background-color: white; padding: 30px 40px; border-radius: 10px; margin: 20px 0; box-shadow: 0 4px 15px rgba(0,0,0,0.1); text-align: center;",
            
            tags$p(
              paste0(
                "You chose the state of ", input$survey_state, ". There have been ", state_count,
                " contestant", if (state_count != 1) "s" else "", " on ", show_name, " from your state. ",
                "Your age of ", input$survey_age, " has a ", age_success_pct, "% chance of success on the show. ",
                "We have had ", job_count, " ", input$survey_occupation,
                if (job_count != 1) "s" else "", " compete on ", show_name, "."
              ),
              style = "font-size: 18px; color: #333; line-height: 1.8; margin-bottom: 16px;"
            ),
            
            tags$p(
              paste0(
                "Because of this, you have a ", overall_success_pct, "% chance of making it on ", show_name, "."
              ),
              style = "font-size: 18px; color: #333; line-height: 1.8; margin-bottom: 16px;"
            ),
            
            tags$p(
              week_msg,
              style = paste0(
                "font-size: 22px; font-weight: bold; font-family: 'Lobster', cursive; margin-top: 10px; ",
                "color: ", if (predicted_week >= 10.5) "#228B22" else "#DC143C", ";"
              )
            )
          ),
          
          br(),
          
          actionButton("try_again", 
                       "Try Again", 
                       style = "background-color: #DC143C; color: white; padding: 10px 30px; font-size: 18px; border: none; font-family: 'Lobster', cursive;")
        )
      })
    })
    
    # Reset survey
    observeEvent(input$try_again, {
      updateRadioButtons(session, "survey_gender", selected = character(0))
      updateSelectizeInput(session, "survey_state", selected = "")
      updateSelectizeInput(session, "survey_occupation", selected = "")
      updateNumericInput(session, "survey_age", value = NA)
      
      output$survey_results <- renderUI({})
    })
}





