library(shiny)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .center-button {
        position: absolute;
        top: 50%;
        left: 50%;
        transform: translate(-50%, -50%);
        z-index: 1000;
      }
      .full-screen {
        height: 100vh;
        margin: -15px;
      }
    "))
  ),
  
  titlePanel("Bachelor/Bachelorette Analysis"),
  
  tabsetPanel(
    id = "tabs",
    tabPanel("Will you accept this rose?",
             tags$div(
               class = "full-screen",
               style = "background-color: #000000; display: flex; flex-direction: column; justify-content: center; align-items: center; cursor: pointer;",
               onclick = "Shiny.setInputValue('splash_click', Math.random());",
               
               tags$img(src = "bachelor_ring.png", width = "300px", style = "margin-bottom: 30px;"),
               tags$h1("Will you accept this rose?", 
                       style = "color: #FFB6C1; font-family: 'Georgia', serif; font-size: 48px;")
             )
    ),
    
    # SPLIT SCREEN - RED & PINK
    tabPanel("Choose",
             tags$div(
               class = "full-screen",
               style = "display: flex; position: relative;",
               
               # LEFT SIDE - RED (Bachelor)
               tags$div(
                 style = "width: 50%; background-color: #D21A00; display: flex; justify-content: center; align-items: center;",
                 tags$h1("BACHELOR", 
                         style = "color: white; font-size: 72px; font-weight: bold; font-family: 'Georgia', serif;")
               ),
               
               # RIGHT SIDE - PINK (Bachelorette)
               tags$div(
                 style = "width: 50%; background-color: #F8DEE7; display: flex; justify-content: center; align-items: center;",
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
    
    # YOUR DATA UPLOAD TAB (or next tab)
    tabPanel("Upload Data",
             fileInput("file1", "Choose CSV File", accept = ".csv"),
             tableOutput("contents")
    )
  )
)

      