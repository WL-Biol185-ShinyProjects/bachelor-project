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
bachelorette_outcomes <- read.csv("The_Bachelorette_(American_TV_series)_1-1.csv", stringsAsFactors = FALSE)
bachelor_outcomes <- read.csv("The_Bachelor_Success-1.csv", stringsAsFactors = FALSE)
quotes <- read.csv("bachelor_quotes.csv", stringsAsFactors = FALSE)
quotes <- quotes %>% filter(!is.na(Classification) & Classification != "")

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
  mutate(state = trimws(sapply(Hometown, get_state)))

# Add state column to geocoded data
bachelor_geocoded <- bachelor_geocoded %>%
  mutate(state = trimws(sapply(city_state, get_state)))

bachelorette_geocoded <- bachelorette_geocoded %>%
  mutate(state = trimws(sapply(city_state, get_state)))


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
  

  # Create color palette for Bachelor (red shades)
  bachelor_color_pal <- colorNumeric(
    palette = c("#FFCCCB", "#FF6B6B", "#DC143C", "#8B0000"),
    domain = c(1, 11)
  )
  
  
  
  # Create color palette for Bachelorette (pink shades)
  bachelorette_color_pal <- colorNumeric(
    palette = c("#FFE4E1", "#FFB6C1", "#FF69B4", "#C71585"),
    domain = c(1, 11)
  )
  
  
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
    
    # Safety check - if no data, show empty map
    if (nrow(data) == 0) {
      return(leaflet() %>%
               addTiles() %>%
               setView(lng = -98.5795, lat = 39.8283, zoom = 4))
    }
    
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
      )%>%
      addLegend(
        position = "topright",
        pal = bachelor_color_pal,
        values = ~avg_week,
        title = "<strong style='font-size: 14px;'>Color Legend</strong><br><span style='font-size: 12px;'>Average Week Eliminated</span>",
        opacity = 0.7,
        labFormat = labelFormat(suffix = " weeks")
      )%>%
      addControl(
        html = "<div style='background: white; padding: 10px; border-radius: 5px; border: 2px solid #8B0000;'>
            <strong style='font-size: 14px; color: #DC143C;'>Circle Size</strong><br>
            <span style='font-size: 12px;'>Larger = More Contestants</span><br>
            <div style='margin-top: 8px;'>
              <svg width='120' height='60'>
                <circle cx='15' cy='45' r='5' fill='#DC143C' opacity='0.7' stroke='#8B0000' stroke-width='2'/>
                <text x='25' y='50' font-size='11'>1-2 contestants</text>
                <circle cx='15' cy='25' r='10' fill='#DC143C' opacity='0.7' stroke='#8B0000' stroke-width='2'/>
                <text x='30' y='30' font-size='11'>4-6 contestants</text>
                <circle cx='20' cy='8' r='15' fill='#DC143C' opacity='0.7' stroke='#8B0000' stroke-width='2'/>
                <text x='40' y='13' font-size='11'>10+ contestants</text>
              </svg>
            </div>
          </div>",
        position = "topleft"
      )
    
    
    
    
    # Zoom to state or show full USA
    if (!is.null(input$bachelor_state) && input$bachelor_state != "all" && nrow(data) > 0) {
      map <- map %>% fitBounds(
        lng1 = min(data$longitude, na.rm = TRUE) - 1,
        lat1 = min(data$latitude, na.rm = TRUE) - 1,
        lng2 = max(data$longitude, na.rm = TRUE) + 1,
        lat2 = max(data$latitude, na.rm = TRUE) + 1
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
  
    # Safety check - if no data, show empty map
    if (nrow(data) == 0) {
      return(leaflet() %>%
               addTiles() %>%
               setView(lng = -98.5795, lat = 39.8283, zoom = 4))
    }
    
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
        ) %>%
        addLegend(
          position = "topright",
          pal = bachelorette_color_pal,
          values = ~avg_week,
          title = "<strong style='font-size: 14px;'>Color Legend</strong><br><span style='font-size: 12px;'>Average Week Eliminated</span>",
          opacity = 0.7,
          labFormat = labelFormat(suffix = " weeks")
        ) %>%
        addControl(
          html = "<div style='background: white; padding: 10px; border-radius: 5px; border: 2px solid #C71585;'>
            <strong style='font-size: 14px; color: #C71585;'>Circle Size</strong><br>
            <span style='font-size: 12px;'>Larger = More Contestants</span><br>
            <div style='margin-top: 8px;'>
              <svg width='120' height='60'>
                <circle cx='15' cy='45' r='5' fill='#FF69B4' opacity='0.7' stroke='#C71585' stroke-width='2'/>
                <text x='25' y='50' font-size='11'>1-2 contestants</text>
                <circle cx='15' cy='25' r='10' fill='#FF69B4' opacity='0.7' stroke='#C71585' stroke-width='2'/>
                <text x='30' y='30' font-size='11'>4-6 contestants</text>
                <circle cx='20' cy='8' r='15' fill='#FF69B4' opacity='0.7' stroke='#C71585' stroke-width='2'/>
                <text x='40' y='13' font-size='11'>10+ contestants</text>
              </svg>
            </div>
          </div>",
          position = "topleft"
        )
      
      
    
    # Zoom to state or show full USA
      if (!is.null(input$bachelorette_state) && input$bachelorette_state != "all" && nrow(data) > 0) {
        map <- map %>% fitBounds(
          lng1 = min(data$longitude, na.rm = TRUE) - 1,
          lat1 = min(data$latitude, na.rm = TRUE) - 1,
          lng2 = max(data$longitude, na.rm = TRUE) + 1,
          lat2 = max(data$latitude, na.rm = TRUE) + 1
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
      return(p(paste0("No contestants found. State: '", input$bachelor_state,
                      "'. Available: ",
                      paste(head(unique(contestants$state[contestants$Show == "The Bachelor"]), 3), collapse=", ")),
               style = "color: #888;"))
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
    
    # Bachelor Map → Title Screen
    observeEvent(input$bach_map_home, {
      updateTabsetPanel(session, "tabs", selected = "Will you accept this rose?")
    })
    
    # Bachelor Map → Data Analysis
    observeEvent(input$bach_map_analysis, {
      analysis_show("The Bachelor")
      updateTabsetPanel(session, "tabs", selected = "Data Analysis")
    })
    
    # Bachelorette Map → Title Screen
    observeEvent(input$bach_ette_map_home, {
      updateTabsetPanel(session, "tabs", selected = "Will you accept this rose?")
    })
    
    # Bachelorette Map → Data Analysis
    observeEvent(input$bach_ette_map_analysis, {
      analysis_show("The Bachelorette")
      updateTabsetPanel(session, "tabs", selected = "Data Analysis")
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
          11
        } else if (grepl("Runner-up", eliminated, ignore.case = TRUE)) {
          10
        } else if (grepl("Week", eliminated, ignore.case = TRUE)) {
          week_num <- as.numeric(gsub(".*Week ([0-9]+).*", "\\1", eliminated))
          week_num
        } else {
          1
        }
      }
      
      
      # Get all contestants from the same show
      show_contestants <- contestants %>%
        filter(Show == show_name) %>%
        mutate(week_number = sapply(Eliminated, get_week_num)) 

      # Total number of contestants ever on this show
      total_contestants <- nrow(show_contestants)
      
      # Calculate how common each attribute is
      
      # 1. State likelihood - what % of contestants came from this state?
      state_contestants <- show_contestants %>%
        filter(!is.na(state), state == input$survey_state)
      
      state_count <- nrow(state_contestants)
      state_likelihood <- (state_count / total_contestants) * 100
      
      # 2. Occupation likelihood - what % of contestants had this job?
      job_contestants <- show_contestants %>%
        filter(!is.na(Job), Job == input$survey_occupation)
      
      job_count <- nrow(job_contestants)
      job_likelihood <- (job_count / total_contestants) * 100
      
      # 3. Age likelihood - what % of contestants were within ±3 years of this age?
      age_contestants <- show_contestants %>%
        filter(!is.na(Age), abs(Age - input$survey_age) <= 3)
      
      age_count <- nrow(age_contestants)
      age_likelihood <- (age_count / total_contestants) * 100 
     
      
      
      # 4. Check for exact matches (same state + job + age)
      exact_matches <- show_contestants %>%
        filter(
          !is.na(state), state == input$survey_state,
          !is.na(Job), Job == input$survey_occupation,
          !is.na(Age), Age == input$survey_age
        )
      
      exact_match_count <- nrow(exact_matches)
      
      #Check if any exact match was a WINNER
      exact_match_winner <- exact_matches %>%
        filter(week_number == 11)
      
      has_winner_match <- nrow(exact_match_winner) > 0
      
      
      # Calculate overall casting likelihood
      # If exact matches exist, boost the percentage
      if (exact_match_count > 0) {
        # EXACT MATCH = 100% casting chance
        casting_percentage <- 100
        #If exact match includes a winner
        if (has_winner_match) {
          predicted_week <- 11
          is_winner_prediction <- TRUE
        } else {
          # Use the EXACT week number from best-performing exact match
          predicted_week <- max(exact_matches$week_number, na.rm = TRUE)
          is_winner_prediction <- FALSE
        }
        
        if (is.na(predicted_week)) predicted_week <- 5
      } else {
        # No exact match - use weighted calculation
        is_winner_prediction <- FALSE
      
        
        # Weight the factors: state (20%), job (40%), age (40%)
        base_percentage <- (state_likelihood * 0.2) + (job_likelihood * 0.4) + (age_likelihood * 0.4)
        
        # Normalize to a reasonable range (15% - 85%)
        casting_percentage <- round(max(15, min(85, base_percentage)))
        
      }
      
      
      
      # Calculate predictions based on different factors
      
      #Only calculate predicted_week if we DON'T have an exact match
      if (exact_match_count == 0) {
        # Calculate predicted performance (how far they'd go)
        state_avg <- if (nrow(state_contestants) > 0) {
          mean(state_contestants$week_number, na.rm = TRUE)
        } else {
          mean(show_contestants$week_number, na.rm = TRUE)
        }
        
        job_avg <- if (nrow(job_contestants) > 0) {
          mean(job_contestants$week_number, na.rm = TRUE)
        } else {
          mean(show_contestants$week_number, na.rm = TRUE)
        }
        
      
      
      # 3. Age-based prediction
        age_avg <- if (nrow(age_contestants) > 0) {
          mean(age_contestants$week_number, na.rm = TRUE)
        } else {
          mean(show_contestants$week_number, na.rm = TRUE)
        }
        
      
      # Predicted week (how far they'd go)
      predicted_week <- round((state_avg * 0.3 + job_avg * 0.4 + age_avg * 0.3), 1)
      if (is.na(predicted_week)) predicted_week <- 5
      }
      
      
      # Convert to success percentage (out of 11 weeks)
      success_percentage <- round((predicted_week / 11) * 100)
      
      # Generate result message
      output$survey_results <- renderUI({
        # --- Calculate age success rate (ONLY thing we need to calculate here) ---
        age_success_pct <- show_contestants %>%
          filter(!is.na(Age), abs(Age - input$survey_age) <= 3) %>%
          summarise(pct = round(mean(week_number >= 10, na.rm = TRUE) * 100)) %>%
          pull(pct)
        
        if (length(age_success_pct) == 0 || is.na(age_success_pct)) age_success_pct <- 0
        
        # --- Week message (using predicted_week already calculated) ---
        week_msg <- if (is_winner_prediction) {
          "Congratulations, you will find love!🌹"
        } else if (predicted_week >= 10.5) {
          "If cast, you'd likely find love!"
        } else {
          paste0("If cast, you'd likely make it to Week ", round(predicted_week), "!")
        }
        
        
        # --- Display NEW FORMAT ---
        tags$div(
          style = "margin-top: 30px; padding: 30px; background: linear-gradient(135deg, #FFE4E1 0%, #FFF5F5 100%); border-radius: 15px; text-align: center;",
          
          h3("Your Results!", style = "color: #DC143C; font-family: 'Lobster', cursive; font-size: 36px;"),
          
          tags$div(
            style = "background-color: white; padding: 30px 40px; border-radius: 10px; margin: 20px 0; box-shadow: 0 4px 15px rgba(0,0,0,0.1); text-align: center;",
            
            # PARAGRAPH with state, age, job stats (uses variables already calculated)
            tags$p(
              paste0(
                "You chose the state of ", input$survey_state, ". There have been ", state_count,
                " contestant", if (state_count != 1) "s" else "", " on ", show_name, " from your state",
                if (state_count > 0) paste0(" (", round(state_likelihood, 1), "% of all contestants)") else "", ". ",
                "Your age of ", input$survey_age, " has appeared ", age_count, " time",
                if (age_count != 1) "s" else "", " on the show",
                if (age_count > 0) paste0(" (", round(age_likelihood, 1), "% of all contestants)") else "", ". ",
                "We've had ", job_count, " ", input$survey_occupation,
                if (job_count != 1) "s" else "", " compete on ", show_name,
                if (job_count > 0) paste0(" (", round(job_likelihood, 1), "% of contestants)") else "", "."
              ),
              style = "font-size: 18px; color: #333; line-height: 1.8; margin-bottom: 16px;"
            ),
            
            # EXACT MATCH message (if applicable)
            if (exact_match_count > 0) {
              tags$p(
                paste0(
                  "🎉 Perfect Match! We found ", exact_match_count, " contestant",
                  if (exact_match_count != 1) "s" else "",
                  " with your EXACT profile (age ", input$survey_age, ", ", 
                  input$survey_occupation, ", from ", input$survey_state, 
                  ") who ", if (exact_match_count == 1) "was" else "were", 
                  " cast on the show",
                  if (has_winner_match) " — and one of them WON!" else "!",  # ← NEW
                  " They made it to Week ", round(predicted_week),  # ← NEW
                  if (has_winner_match) " (Winner!)" else "", "."  # ← NEW
                ),
                style = "font-size: 20px; color: #228B22; font-weight: bold; line-height: 1.8; margin-bottom: 16px; background-color: #E8F5E9; padding: 15px; border-radius: 8px;"  # ← CHANGED: larger font, green background
              )
            },
            
            # BIG CASTING PERCENTAGE BOX (NEW - uses casting_percentage)
            tags$div(
              style = "background: linear-gradient(135deg, #FFE4E1, #FFF0F5); padding: 20px; border-radius: 10px; margin: 20px 0;",
              h1(paste0(casting_percentage, "%"), 
                 style = "font-size: 64px; color: #DC143C; margin: 0; font-family: 'Lobster', cursive;"),
              p("Chance of Being Cast on the Show", 
                style = "font-size: 20px; color: #666; margin-top: 10px; font-weight: bold;")
            ),
            
            # WEEK PREDICTION message
            tags$p(
              week_msg,
              style = paste0(
                "font-size: ", if (is_winner_prediction) "28px" else "20px", "; ",  # ← NEW: bigger for winner
                "font-family: 'Lobster', cursive; margin-top: 16px; ",
                "color: ", if (is_winner_prediction) "#D3AF37" else if (predicted_week >= 10.5) "#228B22" else "#DC143C", "; ",
                if (is_winner_prediction) "text-shadow: 2px 2px 4px rgba(0,0,0,0.2); font-weight: bold;" else "" 
                
                
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
    
    # ========== DATA ANALYSIS LOGIC ==========
    
    # Reactive value to store which show is selected for analysis
    analysis_show <- reactiveVal("The Bachelor")
    
    # Update when Bachelor button is clicked
    observeEvent(input$analysis_bachelor, {
      analysis_show("The Bachelor")
    })
    
    # Update when Bachelorette button is clicked
    observeEvent(input$analysis_bachelorette, {
      analysis_show("The Bachelorette")
    })
    
    # Display current selection
    output$analysis_selection <- renderUI({
      show_color <- if (analysis_show() == "The Bachelor") "#D21A00" else "#F8DEE7"
      text_color <- if (analysis_show() == "The Bachelor") "white" else "#DC143C"
      
      tags$div(
        style = paste0("text-align: center; padding: 15px; background-color: ", show_color, 
                       "; border-radius: 10px; margin-bottom: 20px;"),
        h4(paste0("Currently Viewing: ", analysis_show()),
           style = paste0("color: ", text_color, "; font-family: 'Lobster', cursive; margin: 0;"))
      )
    })
    
    # Helper function to get week number (reuse from survey)
    get_week_num_analysis <- function(eliminated) {
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
    
    # Age vs Performance Plot
    output$age_performance_plot <- renderPlot({
      # Get data for selected show
      show_data <- contestants %>%
        filter(Show == analysis_show(), !is.na(Age))
      
      # Count contestants by age
      age_summary <- show_data %>%
        group_by(Age) %>%
        summarise(count = n()) %>%
        arrange(Age)

      
      # Plot color based on show
      plot_color <- if (analysis_show() == "The Bachelor") "#DC143C" else "#FF69B4"
      
      ggplot(age_summary, aes(x = Age, y = count)) +
        geom_bar(stat = "identity", fill = plot_color, alpha = 0.7) +
        geom_text(aes(label = count), vjust = -0.5, size = 3, color = "#333") +
        labs(
          title = paste0(analysis_show(), ": Number of Contestants by Age"),
          subtitle = "Total contestants at each age",
          x = "Age",
          y = "Number of Contestants"
          
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 18, face = "bold", color = "#DC143C"),
          plot.subtitle = element_text(size = 12, color = "#666"),
          axis.title = element_text(size = 14, face = "bold"),
          axis.text = element_text(size = 12),
          panel.grid.major.x = element_blank()
        ) +
        scale_y_continuous(breaks = seq(0, max(age_summary$count) + 5, 5)) +
        scale_x_continuous(breaks = seq(min(age_summary$Age), max(age_summary$Age), 1))
    })
    
    # Occupation vs Performance Plot
    output$occupation_performance_plot <- renderPlot({
      # Get data for selected show
      show_data <- contestants %>%
        filter(Show == analysis_show(), !is.na(Job)) 
      
      # Calculate average week by occupation, get top 15
      occupation_summary <- show_data %>%
        group_by(Job) %>%
        summarise(count = n()) %>%
        arrange(desc(count)) %>%
        head(15) %>%
        arrange(count)  # Sort by count for better visualization
      
      
      # Plot color based on show
      plot_color <- if (analysis_show() == "The Bachelor") "#DC143C" else "#FF69B4"
      
      ggplot(occupation_summary, aes(x = reorder(Job, count), y = count)) +
        geom_bar(stat = "identity", fill = plot_color, alpha = 0.7) +
        geom_text(aes(label = count), hjust = -0.1, size = 3.5, color = "#333") +
        coord_flip() +
        labs(
          title = paste0(analysis_show(), ": Top 15 Most Common Occupations"),
          subtitle = "Sorted by number of contestants",
          x = "Occupation",
          y = "Number of Contestants"
          
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 18, face = "bold", color = "#DC143C"),
          plot.subtitle = element_text(size = 12, color = "#666"),
          axis.title = element_text(size = 14, face = "bold"),
          axis.text.y = element_text(size = 11),
          axis.text.x = element_text(size = 12),
          panel.grid.major.y = element_blank()
        ) +
        scale_y_continuous(breaks = seq(0, max(occupation_summary$count) + 2, 2))
      
    })
    
    output$success_rate_plot <- renderPlot({
      
      # Pick dataset based on selected show
      is_bachelor <- analysis_show() == "The Bachelor"
      outcomes_raw <- if (is_bachelor) bachelor_outcomes else bachelorette_outcomes
      plot_color1 <- if (is_bachelor) "#FF6B6B" else "#FF69B4"
      plot_color2 <- if (is_bachelor) "#DC143C" else "#C71585"
      show_label <- if (is_bachelor) "Bachelor" else "Bachelorette"
      
      # Clean and prepare data
      outcomes <- outcomes_raw %>%
        mutate(
          Proposal = trimws(Proposal),
          Still_Together = trimws(`Still.together.`),
          proposed = Proposal == "Yes",
          still_together = Still_Together == "Yes"
        ) %>%
        filter(!is.na(Season), Season != "") %>%
        group_by(Season) %>%
        slice(1) %>%
        ungroup()
      
      # Calculate percentages
      total <- nrow(outcomes)
      proposal_pct <- round(mean(outcomes$proposed, na.rm = TRUE) * 100)
      together_pct <- round(mean(outcomes$still_together, na.rm = TRUE) * 100)
      
      # Build summary dataframe
      summary_df <- data.frame(
        Category = c("Proposal Made", "Still Together"),
        Percentage = c(proposal_pct, together_pct)
      )
      
      ggplot(summary_df, aes(x = Category, y = Percentage, fill = Category)) +
        geom_bar(stat = "identity", width = 0.5, alpha = 0.85) +
        geom_text(aes(label = paste0(Percentage, "%")),
                  vjust = -0.5, size = 6, fontface = "bold", color = "#333") +
        scale_fill_manual(values = c("Proposal Made" = plot_color1, "Still Together" = plot_color2)) +
        scale_y_continuous(limits = c(0, 110), breaks = seq(0, 100, 20),
                           labels = function(x) paste0(x, "%")) +
        labs(
          title = paste0(show_label, " Success Rate"),
          subtitle = paste0("Based on ", total, " seasons"),
          x = NULL,
          y = "Percentage of Seasons"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 24, face = "bold", color = plot_color2,
                                    hjust = 0.5),
          plot.subtitle = element_text(size = 14, color = "#666", hjust = 0.5),
          axis.text.x = element_text(size = 14, face = "bold", color = "#333"),
          axis.text.y = element_text(size = 12),
          axis.title.y = element_text(size = 14, face = "bold"),
          legend.position = "none",
          panel.grid.major.x = element_blank()
        )
    })
    
    # ========== QUOTE GAME LOGIC ==========
    
    game_state <- reactiveValues(
      stage = "start",
      score = 0,
      round = 1,
      max_rounds = 5,
      funny_count = 0,
      sassy_count = 0,
      current_quotes = NULL,
      correct_type = NULL
    )
    
    # Render the game UI
    output$game_ui <- renderUI({
      
      if (game_state$stage == "start") {
        tags$div(
          style = "text-align: center; padding: 40px;",
          tags$img(src = "bachelor_rose.png", style = "width: 120px; margin-bottom: 20px; border-radius: 50%;"),
          h3("Welcome to the Rose Game!", 
             style = "font-family: 'Lobster', cursive; color: #DC143C; font-size: 40px;"),
          p("You're a contestant on The Bachelor. Each round, you'll be given a quote to say on your date. Choose wisely — romantic quotes win roses, but sassy and funny ones might send you home!",
            style = "font-size: 18px; color: #555; max-width: 600px; margin: 0 auto 30px auto; line-height: 1.6;"),
          actionButton("game_start", "Accept This Rose 🌹",
                       style = "background-color: #DC143C; color: white; font-family: 'Lobster', cursive;
                                font-size: 24px; padding: 15px 40px; border: none; border-radius: 10px;")
        )
        
      } else if (game_state$stage == "playing") {
        quotes_choices <- game_state$current_quotes
        
        tags$div(
          style = "padding: 20px;",
          
          # Progress bar
          tags$div(
            style = "margin-bottom: 20px;",
            tags$div(
              style = paste0("background: linear-gradient(90deg, #DC143C ", 
                             round((game_state$round - 1) / game_state$max_rounds * 100), 
                             "%, #FFE4E1 0%); height: 12px; border-radius: 6px;")
            ),
            p(paste0("Date ", game_state$round, " of ", game_state$max_rounds, 
                     " — Score: ", game_state$score),
              style = "color: #DC143C; font-family: 'Lobster', cursive; font-size: 20px; margin-top: 8px;")
          ),
          
          h3("You're on a date! Which quote do you use?",
             style = "font-family: 'Lobster', cursive; color: #DC143C; font-size: 28px; margin-bottom: 20px;"),
          
          # Quote buttons
          tags$div(
            style = "display: flex; flex-direction: column; gap: 15px;",
            lapply(1:nrow(quotes_choices), function(i) {
              actionButton(
                paste0("quote_choice_", i),
                tags$div(
                  tags$p(paste0('"', quotes_choices$Quote[i], '"'),
                         style = "font-size: 16px; font-style: italic; margin: 0 0 5px 0;"),
                  tags$small(paste0("— ", quotes_choices$Speaker[i]),
                             style = "color: #888;")
                ),
                style = "background-color: white; color: #333; border: 2px solid #DC143C;
                         padding: 15px 20px; text-align: left; width: 100%; border-radius: 10px;
                         font-size: 15px; line-height: 1.4;"
              )
            })
          )
        )
        
      } else if (game_state$stage == "result") {
        got_rose <- game_state$score >= 3
        mostly_funny <- game_state$funny_count >= 3
        mostly_sassy <- game_state$sassy_count >= 3
        
        # Determine outcome type
        outcome_type <- if (got_rose) {
          "winner"
        } else if (mostly_funny) {
          "funny"
        } else if (mostly_sassy) {
          "sassy"
        } else {
          "eliminated"
        }
        
        result_color <- switch(outcome_type,
                               "winner" = "#228B22",
                               "funny" = "#FF8C00",
                               "sassy" = "#8B008B",
                               "eliminated" = "#DC143C"
        )
        result_bg <- switch(outcome_type,
                            "winner" = "#E8F5E9",
                            "funny" = "#FFF8E1",
                            "sassy" = "#F3E5F5",
                            "eliminated" = "#FFE4E1"
        )
        result_emoji <- switch(outcome_type,
                               "winner" = "🌹",
                               "funny" = "😂",
                               "sassy" = "💅",
                               "eliminated" = "💔"
        )
        result_title <- switch(outcome_type,
                               "winner" = "You got the final rose!",
                               "funny" = "No rose... but you're everyone's favorite!",
                               "sassy" = "Eliminated, but iconic.",
                               "eliminated" = "No rose for you..."
        )
        result_msg <- switch(outcome_type,
                             "winner" = paste0("You scored ", game_state$score, " out of ", game_state$max_rounds, 
                                               " — your romantic side won them over! 💍"),
                             "funny" = paste0("You had the whole mansion laughing. You didn't get the rose, but America loves you. ",
                                              "You might just be chosen as the next lead! 🎉"),
                             "sassy" = paste0("You said what everyone was thinking. No rose, but you're the most memorable contestant ",
                                              "this season — producers are already calling. 📱"),
                             "eliminated" = paste0("You scored ", game_state$score, " out of ", game_state$max_rounds, 
                                                   " — the connection just wasn't there. Maybe next season!")
        )
        
        tags$div(
          style = paste0("text-align: center; padding: 40px; background-color: ", result_bg, 
                         "; border-radius: 15px;"),
          tags$h1(result_emoji, style = "font-size: 80px; margin: 0;"),
          h3(result_title,
             style = paste0("font-family: 'Lobster', cursive; color: ", result_color, 
                            "; font-size: 40px; margin: 20px 0 10px 0;")),
          p(result_msg,
            style = "font-size: 20px; color: #555; margin-bottom: 30px;"),
          tags$div(
            style = "display: flex; gap: 15px; justify-content: center;",
            actionButton("game_restart", "Play Again 🌹",
                         style = "background-color: #DC143C; color: white; font-family: 'Lobster', cursive;
                                  font-size: 20px; padding: 12px 30px; border: none; border-radius: 10px;")
          )
        )
      }
    })
    
    # Start game
    observeEvent(input$game_start, {
      game_state$stage <- "playing"
      game_state$score <- 0
      game_state$round <- 1
      
      # Pick 3 random quotes (one romantic, one funny, one sassy)
      romantic <- quotes %>% filter(Classification == "Romantic") %>% sample_n(1)
      funny <- quotes %>% filter(Classification == "Funny") %>% sample_n(1)
      sassy <- quotes %>% filter(Classification == "Sassy") %>% sample_n(1)
      game_state$current_quotes <- rbind(romantic, funny, sassy)[sample(3), ]
      game_state$correct_type <- "Romantic"
    })
    
    # Handle quote choices
    observe({
      lapply(1:3, function(i) {
        observeEvent(input[[paste0("quote_choice_", i)]], {
          chosen <- game_state$current_quotes[i, ]
          if (chosen$Classification == "Romantic") {
            game_state$score <- game_state$score + 1
          } else if (chosen$Classification == "Funny") {
            game_state$funny_count <- game_state$funny_count + 1
          } else if (chosen$Classification == "Sassy") {
            game_state$sassy_count <- game_state$sassy_count + 1
          }
          if (game_state$round >= game_state$max_rounds) {
            game_state$stage <- "result"
          } else {
            game_state$round <- game_state$round + 1
            romantic <- quotes %>% filter(Classification == "Romantic") %>% sample_n(1)
            funny <- quotes %>% filter(Classification == "Funny") %>% sample_n(1)
            sassy <- quotes %>% filter(Classification == "Sassy") %>% sample_n(1)
            game_state$current_quotes <- rbind(romantic, funny, sassy)[sample(3), ]
          }
        }, ignoreInit = TRUE)
      })
    })
    
    # Restart game
    observeEvent(input$game_restart, {
      game_state$stage <- "start"
      game_state$score <- 0
      game_state$round <- 1
      game_state$funny_count <- 0
      game_state$sassy_count <- 0
    })
    
    # Game home button
    observeEvent(input$game_home, {
      updateTabsetPanel(session, "tabs", selected = "Will you accept this rose?")
    })

}





