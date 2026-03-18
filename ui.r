library(shiny)
library(leaflet)

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet",
              href = "https://fonts.googleapis.com/css2?family=Lobster&family=Sloop+Script+One&display=swap"
    ),
    
    tags$style(HTML("
       /* Remove ALL default margins and padding */
      body, html {
        margin: 0 !important;
        padding: 0 !important;
        overflow: hidden;
        height: 100%;
      }
       .container-fluid {
        padding: 0 !important;
      }
      
      .center-button {
        position: absolute;
        top: 50%;
        left: 50%;
        transform: translate(-50%, -50%);
        z-index: 1000;
      }
      
      .full-screen {
        height: 100vh;
        width: 100vw;
        margin: 0;
        padding: 0;
        position: fixed;
        top: 0;
        left: 0;
      }
      
       /* Make the split screen divs clickable */
      .clickable-side {
        cursor: pointer;
        transition: opacity 0.3s;
      }

.clickable-side:hover {
        opacity: 0.9;
      }

      /* Tab styling */
         .nav-tabs {
        background-color: #FFE4E1;
        border-bottom: none;
        padding: 5px 10px;
        position: fixed;
        top: 0;
        left: 0;
        right: 0;
        z-index: 9999;
      }
      
      .nav-tabs > li > a {
        font-family: 'Lobster', cursive;
        font-size: 16px;
        color: #D21A00 !important;
        border: none !important;
        background-color: transparent !important;
        border-radius: 0 !important;
      }
      
      .nav-tabs > li > a:hover {
        color: white !important;
        background-color: rgba(255,255,255,0.2) !important;
        border: none !important;
      }
      
      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:hover,
      .nav-tabs > li.active > a:focus {
        font-family: 'Lobster', cursive;
        color: #DC143C !important;
        background-color: white !important;
        border: none !important;
        border-radius: 5px 5px 0 0 !important;
      }
      
      .nav.nav-tabs {
        border-bottom: 3px solid #8B0000;
      }
      
       .tab-content {
        height: calc(100vh - 42px);
        overflow-y: auto;
        margin-top: 42px;
      }

    "))
  ),

  
  
  tabsetPanel(
    id = "tabs",
    tabPanel("Will you accept this rose?",
             tags$div(
               class = "full-screen",
               style = "background-image: url('bachelor_rose.png'); 
            background-size: cover; 
            background-position: center; 
            display: flex; 
            justify-content: center; 
            align-items: center; 
            cursor: pointer;",
               onclick = "Shiny.setInputValue('splash_click', Math.random());",
               
               # Centered text on top with Lobster font
               tags$h1("Will you accept this rose?", 
                       style = "font-family: 'Lobster', cursive; 
                    font-size: 80px; 
                    color: #FFFFFF; 
                    z-index: 100; 
                    text-align: center;
                    text-shadow: 3px 3px 6px rgba(0,0,0,0.8);
                    margin: 0;
                    padding: 20px;")
             )
    ),
    
    # SPLIT SCREEN - RED & PINK
    tabPanel("Choose",
             tags$div(
               class = "full-screen",
               style = "display: flex; position: relative;",
               
    
               
               # LEFT SIDE - RED (Bachelor)
               tags$div(
                 class = "clickable-side",
                 style = "width: 50%; background-color: #D21A00; display: flex; flex-direction: column; justify-content: center; align-items: center;",
                 onclick = "Shiny.setInputValue('bachelor_click', Math.random());",
                 tags$h1("BACHELOR", 
                         style = "color: white; font-size: 72px; font-weight: bold; font-family: 'Georgia', serif; pointer-events:none; margin-bottom: 20px;"),
                 tags$p("Click here to find out where the Bachelor contestants are from", 
                        style = "color: white; font-size: 24px; font-family: 'Lobster', cursive; pointer-events: none; margin-top: 0;")
                 
                 ),
               
               # RIGHT SIDE - PINK (Bachelorette)
               tags$div(
                 class = "clickable-side",
                 style = "width: 50%; background-color: #F8DEE7; display: flex; flex-direction: column; justify-content: center; align-items: center;",
                 onclick = "Shiny.setInputValue('bachelorette_click', Math.random());",
                 tags$h1("BACHELORETTE", 
                         style = "color: white; font-size: 72px; font-weight: bold; font-family: 'Georgia', serif; pointer-events:none; margin-bottom: 20px;"),
                 tags$p("Click here to find out where the Bachelorette contestants are from", 
                        style = "color: white; font-size: 24px; font-family: 'Lobster', cursive; pointer-events: none; margin-top: 0;")
                 
                 ),
               
               # MIDDLE BUTTON (overlaid) - MOVED DOWN AND BIGGER
               tags$div(
                 style = "position: absolute; top: 75%; left: 50%; transform: translate(-50%, -50%); z-index: 1000; pointer-events: auto;",
                 actionButton("start_btn", 
                              "Click here to find out your chances of finding the one", 
                              class = "btn-lg",
                              style = "background-color: white; color: #DC143C; font-weight: bold; padding: 30px 60px; font-size: 24px; border: 4px solid #DC143C;")
               )
             )
          ),
    # BACHELOR MAP WITH DROPDOWN
    tabPanel("Bachelor Map",
             tags$div(
               style = "display: flex; height: 100vh;",
               
               # Map on the left (75% width)
               tags$div(
                 style = "width: 75%; height: 100%;",
                 leafletOutput("bachelor_map", width = "100%", height = "100%")
               ),
               
               # Sidebar on the right (25% width)
               tags$div(
                 style = "width: 25%; height: 100%; background-color: #FFF5F5; padding: 20px; display: flex; flex-direction: column;",
                 
                 h3("Filter by State", style = "color: #DC143C; font-family: 'Lobster', cursive;"),
                 
                 selectInput("bachelor_state", 
                             "Choose a state:",
                             choices = c("All States" = "all"),
                             selected = "all"),
                 
                 hr(),
                 
                 h4("Contestants", style = "color: #DC143C;"),
                 tags$div(
                   style = "max-height: 500px; overflow-y: auto;",
                   uiOutput("bachelor_contestants_list")
                 ),
                 
                 tags$div(
                   style = "display: flex; flex-direction: column; gap: 10px; padding-top: 15px; border-top: 1px solid #ddd; margin-top: 10px;",
                   actionButton("bach_map_home", "Back to Title",
                                style = "background-color: #8B0000; color: white; font-weight: bold;
                        padding: 15px; font-size: 15px; border: none;
                        font-family: 'Lobster', cursive; width: 100%;"),
                   actionButton("bach_map_analysis", "Explore More Data",
                                style = "background-color: white; color: #DC143C; font-weight: bold;
                        padding: 15px; font-size: 15px; border: 2px solid #DC143C;
                        font-family: 'Lobster', cursive; width: 100%;")
                 
                 )
               )
             )
    ),
    
    # BACHELORETTE MAP WITH DROPDOWN
    tabPanel("Bachelorette Map",
             tags$div(
               style = "display: flex; height: 100vh;",
               
               # Map on the left (75% width)
               tags$div(
                 style = "width: 75%; height: 100%;",
                 leafletOutput("bachelorette_map", width = "100%", height = "100%")
               ),
               
               # Sidebar on the right (25% width)
               tags$div(
                 style = "width: 25%; height: 100%; background-color: #FFF0F5; padding: 20px; display: flex; flex-direction: column;",
                 
                 h3("Filter by State", style = "color: #C71585; font-family: 'Lobster', cursive;"),
                 
                 selectInput("bachelorette_state", 
                             "Choose a state:",
                             choices = c("All States" = "all"),
                             selected = "all"),
                 
                 hr(),
                 
                 h4("Contestants", style = "color: #C71585;"),
                 tags$div(
                   style = "max-height: 500px; overflow-y: auto;",
                   uiOutput("bachelorette_contestants_list")
                 ),
                 
                 tags$div(
                   style = "display: flex; flex-direction: column; gap: 10px; padding-top: 15px; border-top: 1px solid #ddd; margin-top: 10px;",
                   actionButton("bach_ette_map_home", "Back to Title",
                                style = "background-color: #C71585; color: white; font-weight: bold;
                        padding: 15px; font-size: 15px; border: none;
                        font-family: 'Lobster', cursive; width: 100%;"),
                   actionButton("bach_ette_map_analysis", "Explore More Data",
                                style = "background-color: white; color: #C71585; font-weight: bold;
                        padding: 15px; font-size: 15px; border: 2px solid #C71585;
                        font-family: 'Lobster', cursive; width: 100%;")
                 )
               )
             )
    ),
    
    # SURVEY TAB
    tabPanel("Survey",
             tags$div(
               style = "height: 100vh; background: linear-gradient(135deg, #D21A00 0%, #F8DEE7 100%); display: flex; justify-content: center; align-items: center; overflow-y: auto;",
               
               tags$div(
                 style = "background-color: white; padding: 50px; border-radius: 20px; box-shadow: 0 10px 30px rgba(0,0,0,0.3); max-width: 600px; width: 90%;",
                 
                 h2("Find Your Chances!", 
                    style = "text-align: center; color: #DC143C; font-family: 'Lobster', cursive; font-size: 48px; margin-bottom: 30px;"),
                 
                 p("Answer a few questions to see your chances of finding love on The Bachelor or Bachelorette!",
                   style = "text-align: center; color: #666; font-size: 18px; margin-bottom: 40px;"),
                 
                 # Gender Selection
                 radioButtons("survey_gender",
                              "I am a:",
                              choices = c("Woman (Bachelor Contestant)" = "female",
                                          "Man (Bachelorette Contestant)" = "male"),
                              selected = character(0)),
                 
                 br(),
                 
                 # State Selection
                 selectizeInput("survey_state",
                                "What state are you from?",
                                choices = NULL,
                                options = list(placeholder = 'Select your state...')),
                 
                 br(),
                 
                 # Occupation Selection
                 selectizeInput("survey_occupation",
                                "What is your occupation?",
                                choices = NULL,
                                options = list(placeholder = 'Select your occupation...')),
                 
                 br(),
                 
                 # Age Input
                 numericInput("survey_age",
                              "How old are you?",
                              value = NULL,
                              min = 21,
                              max = 65,
                              step = 1),
                 
                 br(),
                 
                 # Submit Button
                 actionButton("submit_survey",
                              "Calculate My Chances!",
                              class = "btn-lg",
                              style = "width: 100%; background-color: #DC143C; color: white; font-weight: bold; padding: 15px; font-size: 24px; border: none; font-family: 'Lobster', cursive;"),
                 
                 br(), br(),
                 
                 # Results Display
                 uiOutput("survey_results")
               )
             )
    ),
    
    # ROSE GAME TAB
    tabPanel("Rose Game",
             tags$div(
               style = "min-height: calc(100vh - 42px); background: linear-gradient(135deg, #D21A00 0%, #F8DEE7 100%);
                        display: flex; justify-content: center; align-items: flex-start;
                        padding: 40px 20px; overflow-y: auto;",
               
               tags$div(
                 style = "background-color: white; padding: 40px; border-radius: 20px;
                          box-shadow: 0 10px 30px rgba(0,0,0,0.3); max-width: 700px; width: 100%;",
                 
                 uiOutput("game_ui")
               )
             )
    ),
    
    # DATA ANALYSIS TAB
    tabPanel("Data Analysis",
             tags$div(
               style = "min-height: 100vh; background-color: #FFF5F5; padding: 40px; overflow-y: auto;",
               
               h2("Contestant Performance Analysis", 
                  style = "text-align: center; color: #DC143C; font-family: 'Lobster', cursive; font-size: 48px; margin-bottom: 30px;"),
               
               # Show Selection Buttons
               tags$div(
                 style = "text-align: center; margin-bottom: 40px;",
                 
                 actionButton("analysis_bachelor",
                              "Bachelor Data",
                              class = "btn-lg",
                              style = "background-color: #D21A00; color: white; font-weight: bold; padding: 15px 40px; font-size: 20px; margin: 10px; border: none; font-family: 'Lobster', cursive;"),
                 
                 actionButton("analysis_bachelorette",
                              "Bachelorette Data",
                              class = "btn-lg",
                              style = "background-color: #F8DEE7; color: #DC143C; font-weight: bold; padding: 15px 40px; font-size: 20px; margin: 10px; border: 2px solid #DC143C; font-family: 'Lobster', cursive;")
               ),
               
               # Current selection display
               uiOutput("analysis_selection"),
               
               hr(),
               
               # Success Rate Section
               tags$div(
                 style = "background-color: white; padding: 30px; border-radius: 15px; margin-bottom: 30px; box-shadow: 0 4px 15px rgba(0,0,0,0.1);",
                 
                 h3("Success Rate",
                    style = "color: #DC143C; font-family: 'Lobster', cursive; font-size: 32px; margin-bottom: 20px;"),
                 
                 p("This chart shows what percentage of seasons ended in a proposal, and what percentage of couples are still together.",
                   style = "color: #666; font-size: 16px; margin-bottom: 20px;"),
                 
                 plotOutput("success_rate_plot", height = "400px")
               ),
               
               # Age Analysis Section
               tags$div(
                 style = "background-color: white; padding: 30px; border-radius: 15px; margin-bottom: 30px; box-shadow: 0 4px 15px rgba(0,0,0,0.1);",
                 
                 h3("Age Distribution", 
                    style = "color: #DC143C; font-family: 'Lobster', cursive; font-size: 32px; margin-bottom: 20px;"),
                 
                 p("This chart shows the number of contestants at each age.",
                   style = "color: #666; font-size: 16px; margin-bottom: 20px;"),
                 
                 plotOutput("age_performance_plot", height = "400px")
               ),
               
               # Occupation Analysis Section
               tags$div(
                 style = "background-color: white; padding: 30px; border-radius: 15px; margin-bottom: 30px; box-shadow: 0 4px 15px rgba(0,0,0,0.1);",
                 
                 h3("Top 15 Most Common Occupations", 
                    style = "color: #DC143C; font-family: 'Lobster', cursive; font-size: 32px; margin-bottom: 20px;"),
                 
                 p("This chart shows the most frequently appearing occupations on the show.",
                   style = "color: #666; font-size: 16px; margin-bottom: 20px;"),
                 
                 plotOutput("occupation_performance_plot", height = "500px")
               )
             )
    )
  )
)

