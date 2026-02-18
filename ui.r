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
      
      /* Map styling */
      .map-container {
        height: 100vh;
        width: 100vw;
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
               onclick = "Shiny.setInputValue('splash_click', Math.random());",
    
               
               # LEFT SIDE - RED (Bachelor)
               tags$div(
                 style = "width: 50%; background-color: #D21A00; display: flex; justify-content: center; align-items: center;",
                 onclick = "Shiny.setInputValue('bachelor_click', Math.random());",
                 tags$h1("BACHELOR", 
                         style = "color: white; font-size: 72px; font-weight: bold; font-family: 'Georgia', serif;")
               ),
               
               # RIGHT SIDE - PINK (Bachelorette)
               tags$div(
                 style = "width: 50%; background-color: #F8DEE7; display: flex; justify-content: center; align-items: center;",
                 onclick = "Shiny.setInputValue('bachelorette_click', Math.random());",
                 tags$h1("BACHELORETTE", 
                         style = "color: white; font-size: 72px; font-weight: bold; font-family: 'Georgia', serif;")
               ),
               
               # MIDDLE BUTTON (overlaid)
               tags$div(
                 class = "center-button",
                 actionButton("start_btn", 
                              "What are your chances of finding the one?", 
                              class = "btn-lg",
                              style = "background-color: white; color: #DC143C; font-weight: bold; padding: 20px 40px; font-size: 18px; border: 3px solid #DC143C;")
               )
             )
          ),
    
    # BACHELOR MAP
    tabPanel("Bachelor Map",
             tags$div(
               class = "map-container",
               leafletOutput("bachelor_map", width = "60%", height = "100%")
             )
    ),
    
    # BACHELORETTE MAP
    tabPanel("Bachelorette Map",
             tags$div(
               class = "map-container",
               leafletOutput("bachelorette_map", width = "60%", height = "100%")
             )
    ),
    
    # YOUR DATA UPLOAD TAB (or next tab)
    tabPanel("Upload Data",
             fileInput("file1", "Choose CSV File", accept = ".csv"),
             tableOutput("contents")
    )
  )
)

      