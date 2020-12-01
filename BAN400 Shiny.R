
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

stocks_df$symbol <- gsub("[[:punct:]]","-",stocks_df$symbol)

#------------------------------- LOADING STOCK DATA ------------------------------------

today <- Sys.Date()

from.date <- today %m+% months(-12)

pricedata <- pblapply(stocks_df$symbol, function(x) {
  outdata_all <- getSymbols(x, 
                        from = from.date, 
                        to = today, 
                        warnings = FALSE,
                        auto.assign = FALSE)
  outdata_all <- data.frame(dates = index(outdata_all), coredata(outdata_all)) 

  return(outdata_all)
})

if ()

#------------------------------- DATA TRANFORMATION FOR ALL STOCKS ------------------------------------------

dates <- as.data.frame(pricedata[[1]]$dates) # Create date df

pricedata %>% 
  map(.,function(x) select(x,contains(c("Close")))) %>% # Keep only closing price
    flatten(.) %>%                             # Flatten list
      sapply(.,                                # Extract the list values
        '[', seq(max(sapply(., length)
          )
            )
              ) %>% 
                as.data.frame(.) %>%  
                    mutate(dates = dates$`pricedata[[1]]$dates`, .before = 1) -> SP500 # Add add rsi, ma to this frame

NAs <- colnames(SP500)[colSums(is.na(SP500)) > 0] %>% # Create vector for NA stocks
        gsub(".Close","",.) 

SP500 <- SP500[ ,colSums(is.na(SP500)) == 0] # Remove NA columns from SP500

stocks_df <-  stocks_df[!stocks_df$symbol %in% NAs,] #Remove NA columns from stock_df


#------------------------------- DATA TRANSFORMATION FUNCTION FOR SINGLE STOCK ---------------------------------------
########################### OFFLINE ###############################
extracter <- function(name){
  #' Data extracter
  #' 
  #' @description Gets info from list based on ticker
  #' 
  #' @param name Company name
  pricedata %>% 
    map(.,function(x) select(x, matches(paste0("^",get.ticker(name),".Open")),
                             matches(paste0("^",get.ticker(name),".High")),
                                     matches(paste0("^",get.ticker(name),".Low")),
                                             matches(paste0("^",get.ticker(name),".Close")),
                                                     matches(paste0("^",get.ticker(name),".Volume")),
                                                             matches(paste0("^",get.ticker(name),".Adjusted")))) %>% 
      flatten(.) %>%                             # Flatten list
        sapply(.,                                # Extract the list values
           '[', seq(max(sapply(., length)
               )
                 )
                   ) %>% 
                    as.data.frame(.) %>%      # Transform to df
                      mutate(dates = dates$`pricedata[[1]]$dates`, .before = 1) -> SP500_data # Add add rsi, ma to this frame
  return(SP500_data)
}

extracter("Citigroup Inc.")



#------------------------------- RSI FUNCTION -------------------------------------
# PRICE-funksjonen henter RSI herfra 

rsi_func <- function(n_rsi){
    #' RSI function
    #' 
    #' @description Calculates the relative strength index for all stocks
    #' 
    #' @param n_rsi Number of days smoothed
    rsi.sp <- map_df(SP500[,2:ncol(SP500)],
                               function(x) RSI(x, n = n_rsi)) %>% 
      `colnames<-` (paste0(colnames(.),".rsi"))

}
#------------------------------- RSI TRANS -----------------------------------
 # ALL_PRICE henter RSI herfra via rsi_func

rsi_trans <- function(n_rsi){
  #' Transpose RSI
  #' 
  #' @description Finds and transposes the latest RSI values for each stock
  #' 
  #' @param n_rsi Number of days smoothed
  rsi_all <- rsi_func(n_rsi) %>% 
              .[nrow(.),] %>%     
                t(.) %>%
                  as.data.frame(.) %>% 
                    `colnames<-` (c("rsi"))
}

#------------------------------- MA FUNCTION --------------------------------    
# price funksjonen henter MA herfra 

ma_func <- function(n_ma){
    #' MA function
    #' 
    #' @description Calculates the moving average for all stocks
    #' 
    #' @param  n_ma Number of days smoothed
    ma.sp <- map_df(SP500[,2:ncol(SP500)],
                                  function(x) SMA(x, n = n_ma, fill = list(NA,NULL,NA),
                                    align = "right")) %>% 
                                      `colnames<-` (paste0(colnames(.),".ma"))
}
#------------------------------- MA TRANS -----------------------------------------------
# all_price funksjonen henter MA herfra via ma_func

ma_trans <- function(n_ma){
  #' Transpose MA
  #' 
  #' @description Finds and transposes the latest MA values for each stock
  ma_all <- ma_func(n_ma) %>% 
    .[nrow(.),] %>%     
    t(.) %>%
    as.data.frame(.) %>% 
    `colnames<-` (c("ma"))
}
#------------------------------- TRADING OPPOTUNITY FUNCTION -------------------------------
  
all_price <- function(sector, n_rsi, n_ma){
    #' Data table constructer
    #' 
    #' @description Creates a dataframe with information for the
    #' trading opportunity page
    #' 
    #' @param sector Market sector
    #' @param n_rsi Number of days smoothed for RSI
    #' @param n_ma  Number of days smoothed for MA
    latest <- SP500[nrow(SP500),2:ncol(SP500)] %>%   # Comparing latest price, ma and rsi
      t(.) %>%                                       # Transpose
        as.data.frame(.) %>% 
          mutate(stocks_df$sector, .before = 1) %>% 
            mutate(stocks_df$company, .before = 1) %>% 
              mutate(ma=ma_trans(n_ma)) %>%                  # Add MA values
                mutate(rsi=rsi_trans(n_rsi)) %>%               # Add RSI values
                  `colnames<-`(c("Stocks","Sector", "Price", "MA","RSI"))
    
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
#------------------------------- GET TICKER -----------------------

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
  
  outdata <- select(SP500,"dates", paste0(get.ticker(name),".Close")) %>% 
    mutate(select(rsi_func(n_rsi), paste0(get.ticker(name),".Close.rsi"))) %>% 
    mutate(select(ma_func(n_ma), paste0(get.ticker(name),".Close.ma")))
  
  outdata$signal <-  ifelse(outdata[,4] > outdata[,2] & outdata[,3] < 30, # ifelse for salgssignal
                            
                            c("buy"),
                            
                            ifelse(outdata[,4] < outdata[,2] & outdata[,3] > 70, # må bruke [,2] for å hente pris for alle aksjer
                                   
                                   c("sell"),
                                   
                                      c("hold")))
  return(outdata)
  
}          
# ------------------------------ Building Shiny App -------------------------------------------------------------

ui <- navbarPage("BAN400 Project",
                 
                 #------------------------------- FIRST PANEL------------------------------
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
                 
                 #--------------------------- SECOND PANEL------------------------------
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
                 
                 
                  #------------------- THIRD PANEL ------------------------
                 tabPanel("ABOUT"))

server <-  function(input, output){
  
  
 
  #------------------------------ FIRST PANEL ----------------------------
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
  
  #------------------------- SECOND PANEL--------------------------------
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
