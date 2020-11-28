
#------------------------------- Packages -----------------------
install.packages("docstring")
install.packages("ggthemes")
install.packages("shinythemes")
install.packages("ggthemes")
install.packages("emojifont")
install.packages("ICON")

#------------------------------- Libraries ------------------------
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
library(pbapply)


#------------------------------- TICKERS ---------------------------

stocks_df <- tq_index("SP500")

#TQ <- tq_get(stocks_df$symbol, get = "stock.prices", from = Sys.Date() %m+% months(-12), to = Sys.Date())

# Remove stocks with missing values and unobtainable stocks
stocks_df <- stocks_df[!stocks_df$symbol %in% c("BRK.B","BF.B","CARR","OTIS","VIAC","LUMN","VNT","AVGO"),] 

# Create tickers for trading opportunities
stocks <- as.data.frame(stocks_df) %>% 
  select("symbol") %>% 
  unlist(.) %>% 
  as.character(.)

#------------------------------- TRADING OPORTUNITIES ------------------------------------

today <- Sys.Date()

from.date <- today %m+% months(-12)

pricedata <- pblapply(stocks, function(x) {
  outdata_all <- getSymbols(x, 
                        from = from.date, 
                        to = today, 
                        warnings = FALSE,
                        auto.assign = F)
  outdata_all <- data.frame(dates = index(outdata_all), coredata(outdata_all)) 

  return(outdata_all)
})



#------------------------------- DATA TRANFORMATION ------------------------------------------

dates <- as.data.frame(pricedata[[1]]$dates) # Create date df

pricedata %>% 
  map(.,function(x) select(x,contains(c("Close")))) %>% # Keep only closing price
  flatten(.) %>%                             # Flatten list
  sapply(.,                                # Extract the list values
         '[', seq(max(sapply(., length)
         )
         )
  ) %>% 
  as.data.frame(.) %>%          # Transform to df
  mutate(dates = dates$`pricedata[[1]]$dates`, .before = 1) -> SP500 # Add add rsi, ma to this frame


#--------------------------------- DATA TRANSFORMATION FUNCTION ---------------------------------------
extracter <- function(data){
  
  data_chr <-  as.character(data) 
  pricedata %>% 
    map(.,function(x) select(x,contains(c(data_chr)))) %>% # Keep only closing price
      flatten(.) %>%                             # Flatten list
        sapply(.,                                # Extract the list values
           '[', seq(max(sapply(., length)
               )
                 )
                   ) %>% 
                    as.data.frame(.) %>%          # Transform to df
                      mutate(dates = dates$`pricedata[[1]]$dates`, .before = 1) -> SP500_data # Add add rsi, ma to this frame
  return(SP500_data)
}

colnames(SP500)[colSums(is.na(SP500)) > 0]

#------------------------------- MA, RSI and SIGNAL-------------------------------------
# Calculate RSI

all_price <- function(sector, n_rsi, n_ma){

  rsi.sp <- map_df(SP500[,2:ncol(SP500)],
                               function(x) RSI(x, n = n_rsi)) %>% 
                                 mutate(date=SP500$dates, .before = 1) %>% 
                                  .[nrow(.),2:ncol(.)] %>%     
                                    t(.) %>%
                                      as.data.frame(.) 
# Calculate MA                  
  ma.sp <- map_df(SP500[,2:ncol(SP500)],
                                  function(x) rollmean(x, k = n_ma, fill = list(NA,NULL,NA),
                                    align = "right")) %>% 
                                      mutate(date=SP500$dates, .before = 1) %>% 
                                        .[nrow(.),2:ncol(.)] %>%         
                                          t(.) %>%
                                            as.data.frame(.)

# Price, MA and RSI
  latest <- SP500[nrow(SP500),2:ncol(SP500)] %>%            # Comparing latest price, ma and rsi
                  t(.) %>%                                  # Transpose
                    as.data.frame(.) %>% 
                      mutate(stocks_df$sector, .before = 1) %>% 
                        mutate(stocks_df$company, .before = 1) %>%
                          mutate(ma=ma.sp) %>%                  # Add MA values
                            mutate(rsi=rsi.sp) %>%               # Add RSI values
                            `colnames<-`(c("Stocks","Sector", "Price", "MA","RSI"))


# Set column names
                           
# Sales signal
  latest$signal <-  ifelse(latest$MA > latest$Price & latest$RSI < 30, # ifelse for signal
                            
                            c("buy"),
                            
                              ifelse(latest$MA < latest$Price & latest$RSI > 70, 
                                   
                                 c("sell"),
                                   
                                   c("hold"))) 

# Keep only sell/buy recommendations
  latest <- latest[!latest$signal %in% c("hold"),] 
  
  if (length(sector) > 0){
    selected_sector <- as.character(sector) 
    latest <- subset(latest, latest$Sector == selected_sector)
  } else{
    sectors <- as.character(latest$Sector)
    latest <- subset(latest, latest$Sector == sectors )
  }
  
  
  
  return(latest)
}
 
#---------------------------------------- EXTRA INFO TRANSFORMATION ----------------------------











#------------------------------------- GET TICKER -----------------------

get.ticker <- function(name){
  #' Ticker converter
  #' 
  #' @description Changing company name to ticker
  #' 
  #' @param name the company name
  ticker <- as.character(stocks_df[stocks_df$company %in% name,] %>% 
                         .[,1])
}
  
#------------------------------- PRICEDATA FUNCTION -----------------------------------------------
  

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
  
  outdata <- suppressWarnings(getSymbols(get.ticker(name), 
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


# ------------------------------ Building Shiny App -------------------------------------------------------------

ui <- navbarPage("BAN400 Project",
                 tabPanel("COMPANY SEARCH",
                          fluidPage(theme = shinytheme("superhero"),
                                    titlePanel("COMPANY SEARCH"),
                                    sidebarLayout(
                                      sidebarPanel(
                                        selectInput(inputId = "stockname",
                                                    label = "Search stocks",
                                                    choices = stocks_df$company,
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
                 tabPanel("TRADING OPPORTUNITIES",
                          fluidPage(theme = shinytheme("superhero"),
                                    titlePanel("TRADING OPPORTUNITIES"),
                                      sidebarLayout(
                                        sidebarPanel(
                                        selectInput(inputId = "sector",
                                                    label = "Search sector",
                                                    choices = stocks_df$sector,
                                                    selected = NULL,
                                                    multiple = TRUE,
                                                    selectize = TRUE),
                                        sliderInput(inputId = "MA_all", 
                                                    label = "Moving Average (MA)",
                                                    value = 50,
                                                    min = 1, 
                                                    max = 200),
                                        sliderInput(inputId = "RSI_all", 
                                                    label = "Relative Strength Index (RSI)",
                                                    value = 14,
                                                    min = 1, 
                                                    max = 30),
                                        ),
                                        mainPanel(
                                          dataTableOutput("tradingOportunity")
                                        )
                          )),
                          icon = icon("info-circle")),
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
  
  
  all_data <- reactive({
    suppressWarnings(all_price(input$sector, input$RSI_all, input$MA_all))
  })
  # Adding all the signals
  output$tradingOportunity <- renderDataTable({
    data = all_data()
    data[,1:6]
  })
  
}

shinyApp(ui = ui, server = server)
