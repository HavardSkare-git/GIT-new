rm()
install.packages("docstring")
install.packages("ggthemes")
install.packages("shinythemes")
install.packages("ggthemes")
install.packages("emojifont")
install.packages("ICON")

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
require(emojifont)
library(shinythemes)
library(ICON)
library(docstring)
library(purrr)


#-------------------------------TICKER----------------------------------------------
OBX <- 
  read_html("https://no.wikipedia.org/wiki/OBX-indeksen") %>% 
  html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/table[1]') %>% 
  html_table() %>% 
  as.data.frame() %>% 
  map_df(~gsub("OSE: ", "",.)) 

OBX <- as.character(paste0(OBX$Tickersymbol,".OL"))

get.ticker <- function(name){
  #' Ticker converter
  #' 
  #' @description Changing company name to ticker
  #' 
  #' @param name the company name
  ticker <- as.character(OBX[OBX$Selskap %in% name ,] %>% 
              .[,3])
}

#------------------------------- TRADING OPORTUNITIES ------------------------------------
OBX <- tq_index("SP500")

# Remove stocks with missing values and unobtainable stocks
OBX <- OBX[!OBX$symbol %in% c("BRK.B","BF.B","CARR","OTIS","VIAC","LUMN","VNT","AVGO"),] 


OBX <- as.data.frame(OBX) %>% 
  select("symbol") %>% 
  unlist(.) %>% 
  as.character(.)


today <- Sys.Date()

from.date <- today %m+% months(-12)

pricedata.OBX <- pblapply(OBX, function(x) {
  outdata_all <- getSymbols(x, 
                        from = from.date, 
                        to = today, 
                        warnings = FALSE,
                        auto.assign = F)
  outdata_all <- data.frame(dates = index(outdata_all), coredata(outdata_all)) %>% 
    select("dates", contains("Close")) %>% 
    na.omit(.) 

  return(outdata_all)
})
 
#--------------------------------- DATA TRANFORMATION ------------------------------------------

dates <- as.data.frame(pricedata.OBX[[1]]$dates) # Create date df

pricedata.OBX %>% 
  map(.,function(x) select(x,contains("Close"))) %>% # Keep only closing price
    flatten(.) %>%                             # Flatten list
      sapply(.,                                # Extract the list values
         '[', seq(max(sapply(., length)
             )
               )
                 ) %>% 
                  as.data.frame(.) %>%          # Transform to df
                    mutate(dates = dates$`pricedata.OBX[[1]]$dates`, .before = 1) -> SP500 # Add date column

colnames(SP500)[colSums(is.na(SP500)) > 0]


#-------------------------------------- MA AND RSI-------------------------------------

rsi.obx <- map_df(SP500[,2:ncol(SP500)],
                               function(x) RSI(x)) %>% 
                                 mutate(date=SP500$dates, .before = 1)
                  
ma.OBX <- map_df(SP500[,2:ncol(SP500)],
                                  function(x) rollmean(x, 100, fill = list(NA,NULL,NA),
                                    align = "right")) %>% 
                                      mutate(date=SP500$dates, .before = 1)

rsi_now <- rsi.obx[nrow(rsi.obx),2:ncol(rsi.obx)] %>%     # Latest rsi data
              t(.) %>%
                as.data.frame(.) 
                  
ma_now <- ma.OBX[nrow(ma.OBX),2:ncol(ma.OBX)] %>%         # Latest ma data
              t(.) %>%
                as.data.frame(.) 

latest <- SP500[nrow(SP500),2:ncol(SP500)] %>%            # Comparing latest price, ma and rsi
                t(.) %>%
                  as.data.frame(.) %>% 
                    mutate(ma=ma_now) %>% 
                      mutate(rsi=rsi_now) %>% 
                        `colnames<-`(c("Price", "MA", "RSI"))

 latest$signal <-  ifelse(latest$MA > latest$Price & latest$RSI < 30, # ifelse for salessignal
                          
                          c("buy"),
                          
                          ifelse(latest$MA < latest$Price & latest$RSI > 70, 
                                 
                                 c("sell"),
                                 
                                 c("hold")))      
 
 
 #-------------------------------PRICEDATA FUNCTION -----------------------------------------------

price <- function(name, n_rsi, n_ma){
  #price <- function(name, n_rsi, n_ma){
  #' RSI and MA calculation
  #' 
  #' @description This function collects stockprices using "getSymbols",
  #' transforms it into a dataframe and calculated RSI and moving average.
  #' 
  #' @param name the company name
  #' @param n_rsi number of periods for RSI
  #' @param n_ma number of periods for MA
  
  outdata <- suppressWarnings(getSymbols(name, 
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
  
  outdata$signal <-  ifelse(outdata$ma > outdata[,2] & outdata$rsi < 30, # ifelse for salgssignal
                            
                            c("buy"),
                            
                            ifelse(outdata$ma < outdata[,2] & outdata$rsi > 70, # må bruke [,2] for å hente pris for alle aksjer
                                   
                                   c("sell"),
                                   
                                   c("hold")))
  return(outdata)
  
}                             




# ------------Building Shiny App -------------------------------------------------------------

ui <- navbarPage("BAN400 Project",
                 tabPanel("COMPANY SEARCH",
                          fluidPage(theme = shinytheme("superhero"),
                                    titlePanel("COMPANY SEARCH"),
                                    sidebarLayout(
                                      sidebarPanel(
                                        selectInput(inputId = "stockname",
                                                    label = "Search stocks",
                                                    choices = OBX,
                                                    selected = NULL,
                                                    multiple = FALSE,
                                                    selectize = TRUE),
                                        sliderInput(inputId = "MA", 
                                                    label = "Moving Average (MA)",
                                                    value = 50,
                                                    min = 1, 
                                                    max = 200),
                                        sliderInput(inputId = "RSI", 
                                                    label = "Relative Strength Index (RSI)",
                                                    value = 14,
                                                    min = 1, 
                                                    max = 30),
                                        dateRangeInput(inputId = "dates",
                                                       label = "Choose time period",
                                                       start = Sys.Date() %m+% months(-12),
                                                       end = Sys.Date(),
                                                       min = Sys.Date() %m+% months(-15),
                                                       max = Sys.Date()),
                                        
                                        
                                      ),
                                      
                                      mainPanel(
                                        textOutput("signal"), # plassering av salgssignal i shiny
                                        br(),
                                        plotOutput("priceplot"),
                                        br(),
                                        plotOutput("rsiplot")
                                        )
                                    
                                      )
                                    ),
                                    
                          icon = icon("search")
                          ),
                 tabPanel("TRADING OPPORTUNITIES", icon = icon("info-circle")),
                 tabPanel("ABOUT"))

server <-  function(input, output){
  
  
  #output$signaltext <- renderUI({
   # str1 <- print(output$signal)
    #HTML(str1)
  #})
  
  data <- reactive({
    suppressWarnings(price(input$stockname,input$RSI, input$MA))
  })
  
  output$priceplot <- renderPlot({
    data = data()
    suppressWarnings(ggplot(data)+
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
      theme_economist())
  })
  
  output$rsiplot <- renderPlot({
    data = data()
    suppressWarnings(ggplot(data)+
      geom_line(aes(
        x = data[,1],
        y = data[,3]))+
      geom_hline(yintercept = c(30,70), 
                 col = "red",
                 linetype = "dotted")+
      xlab("DATE")+
      ylab("RSI")+
      ggtitle(paste0("RSI CHART: ", input$stockname))+
      ylim(c(5,95))+
      scale_x_date(limits = c(input$dates[1], input$dates[2]))+
      theme_economist())
  })
  
  output$signal <- renderText({     # Legger til rendertext med salgssignal
    data = data()
    paste("We recommend that you ",data[nrow(data),5]) # må bruke "data" da denne er koblet opp mot "outdata"
  })
}

shinyApp(ui = ui, server = server)
