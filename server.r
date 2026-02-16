library(shiny)
library(ggplot2)
library(tidyverse)

library(shiny)

server <- function(input, output, session) {
  
  # When splash screen is clicked, go to split screen
  observeEvent(input$splash_click, {
    updateTabsetPanel(session, "tabs", selected = "Choose")
  })
  
  # When button is clicked, go to data upload (or wherever you want)
  observeEvent(input$start_btn, {
    updateTabsetPanel(session, "tabs", selected = "Upload Data")
  })
  
  
  server <- function(input, output, session) {
  observeEvent(input$start_btn, { 
    updateTabsetPanel(session, "tabs", selected = "Upload Data") 
 }) 
  }
}



