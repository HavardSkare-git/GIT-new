rm(list = ls())

#### Libraries
require(tidyverse)
require(shiny)
require(rvest)
require(XML)
require(httr)
require(margrittr)
require(ggplot2)
require(xtable)

OBX <- 
  read_html("https://no.wikipedia.org/wiki/OBX-indeksen") %>% 
  html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/table[1]') %>% 
  html_table() %>% 
  as.data.frame() %>% 
  select("Tickersymbol") %>% 
  set_colnames(c("Ticker")) %>% 
  map_df(~gsub("OSE: ", "",.)) %>% 
  map_df(~paste0(.,".OL")) 


price <- function(ticker){
  outdata <- getSymbols(ticker, 
                        from = Sys.Date() %m+% months(-12), 
                        to = Sys.Date(), 
                        warnings = FALSE,
                        auto.assign = FALSE) %>% 
    na.omit()
  
  outdata <- data.frame(dates = index(outdata), coredata(outdata)) %>% 
    select("dates", contains("Close"))
  
  return(outdata)
}

# ------------Building Shiny App 

ui <- fluidPage(
  selectInput(inputId = "stockname",
              label = "Search stocks",
              choices = c("",OBX$Ticker),
              selected = NULL,
              multiple = FALSE,
              selectize = TRUE),
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
  plotOutput("hist")
)

server <- function(input, output){
  
  data <- reactive({
    price(input$stockname)
  })
  
  output$hist <- renderText({
    print(data())
  })
}

shinyApp(ui = ui, server = server)

