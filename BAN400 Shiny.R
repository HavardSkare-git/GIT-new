rm(list = ls())

#### Libraries
require(tidyverse)
require(shiny)

# Building Shiny App

ui <- fluidPage(
  textInput(inputId = "text",
            label = "Search stock",
            value = "",
            width = NULL,
            placeholder = NULL),
  sliderInput(inputId = "RSI", 
              label = "Relative Strength Index (RSI)",
              value = 14,
              min = 1, 
              max = 30),
  sliderInput(inputId = "MA", 
              label = "Moving Average (MA)",
              value = 50,
              min = 1, 
              max = 200),
  actionButton(inputId = "calc", label = "Click here to meet milfs in your neighbourhood"),
  plotOutput("hist")
)

server <- function(input, output){
  output$hist <- renderPlot({
    hist(rnorm(input$MA),
         main = "Test")
    })
}

shinyApp(ui = ui, server = server)

#yolo 


