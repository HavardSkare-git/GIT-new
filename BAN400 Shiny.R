rm(list = ls())

#### Libraries
library(tidyquant)

require(tidyverse)
require(shiny)
require(shinythemes)
require(rvest)
require(XML)
require(httr)
require(magrittr)
require(xtable)
require(ggplot2)
require(ggthemes)
install.packages("shinythemes")
install.packages("ggthemes")


OBX <- 
  read_html("https://no.wikipedia.org/wiki/OBX-indeksen") %>% 
  html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/table[1]') %>% 
  html_table() %>% 
  as.data.frame() %>% 
  select("Tickersymbol") %>% 
  set_colnames(c("Ticker")) %>% 
  map_df(~gsub("OSE: ", "",.)) %>% 
  map_df(~paste0(.,".OL")) 


price <- function(ticker, n_rsi, n_ma){
  outdata <- suppressWarnings(getSymbols(ticker, 
                        from = Sys.Date() %m+% months(-24), 
                        to = Sys.Date(), 
                        warnings = NULL,
                        auto.assign = FALSE))
  
  outdata <- data.frame(dates = index(outdata), coredata(outdata)) %>% 
    select("dates", contains("Close")) %>% 
    na.omit()

  outdata$rsi <- RSI(outdata[,2], 
                     n = n_rsi)
  
  outdata$ma <- rollmean(outdata[,2], 
                         k = n_ma, 
                         fill = list(NA, NULL, NA),
                         align = "right")
  
  return(outdata)
}

# ------------Building Shiny App 

ui <- navbarPage("BAN400 Project",
                 tabPanel("COMPANY SEARCH",
                          fluidPage(theme = shinytheme("superhero"),
                                    titlePanel("COMPANY SEARCH"),
                                    sidebarLayout(
                                      sidebarPanel(
                                        selectInput(inputId = "stockname",
                                                    label = "Search stocks",
                                                    choices = OBX$Ticker,
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
                                        dateRangeInput(inputId = "dates",
                                                       label = "Choose time period",
                                                       start = Sys.Date() %m+% months(-12),
                                                       end = Sys.Date(),
                                                       min = Sys.Date() %m+% months(-15),
                                                       max = Sys.Date())
                                        ),
                                      mainPanel(
                                        plotOutput("priceplot"),
                                        br(),
                                        plotOutput("rsiplot")
                                        )
                                      )
                                    )
                          ),
                 tabPanel("TRADING OPPORTUNITIES"),
                 tabPanel("ABOUT"))

server <- function(input, output){
  
  data <- reactive({
    suppressWarnings(price(input$stockname,input$RSI, input$MA))
  })
  
  output$priceplot <- renderPlot({
    data = data()
    ggplot(data)+
      geom_line(aes(
        x = data[,1],
        y = data[,2]))+
      geom_line(aes(
        x = data[,1],
        y = data[,4]),
        color = "green")+
      xlab("DATE")+
      ylab("CLOSING PRICE")+
      scale_x_date(limits = c(input$dates[1], input$dates[2]))+
      ggtitle(paste0("PRICE CHART: ",input$stockname))+
      theme_economist()
  })
  
  output$rsiplot <- renderPlot({
    data = data()
    ggplot(data)+
      geom_line(aes(
        x = data[,1],
        y = data[,3]))+
      geom_hline(yintercept = c(30,70), 
                 col = "red",
                 linetype = "dotted")+
      xlab("DATE")+
      ylab("RSI")+
      ggtitle(paste0("RSI CHART: ", input$stockname))+
      ylim(c(15,85))+
      scale_x_date(limits = c(input$dates[1], input$dates[2]))+
      theme_economist()
  })
}

shinyApp(ui = ui, server = server)

