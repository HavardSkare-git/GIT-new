
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
library(plotly)

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

#------------------------------- LOADING S&P 500 BENCHMARK DATA ------------------------------------

benchmark <- "^GSPC" %>%
  tq_get(get  = "stock.prices",
         from = from.date,
         to   = today) %>% 
  mutate(change = 100*close/first(close)) %>% 
  dplyr::select(date, change)

#------------------------------- DATA TRANFORMATION FOR ALL STOCKS ------------------------------------------

dates <- as.data.frame(pricedata[[1]]$dates) # Create date df

pricedata %>% 
  map(.,function(x) dplyr::select(x,contains(c("Close")))) %>% # Keep only closing price
    flatten(.) %>%                             # Flatten list
      sapply(.,                                # Extract the list values
        '[', seq(max(sapply(., length)
          )
            )
              ) %>% 
                as.data.frame(.) %>%  
                    mutate(dates = dates$`pricedata[[1]]$dates`, .before = 1) -> SP500 

NAs <- colnames(SP500)[colSums(is.na(SP500)) > 0] %>% # Create vector for NA stocks
        gsub(".Close","",.) 

SP500 <- SP500[ ,colSums(is.na(SP500)) == 0] # Remove NA columns from SP500

stocks_df <-  stocks_df[!stocks_df$symbol %in% NAs,] #Remove NA columns from stock_df

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

#------------------------------- DATA TRANSFORMATION FUNCTION FOR SINGLE STOCK ---------------------------------------
extracter <- function(name){
  #' Data extracter
  #' 
  #' @description Gets info from list based on ticker
  #' 
  #' @param name Company name
  pricedata %>% 
    map(.,function(x) dplyr::select(x, matches(paste0("^",get.ticker(name),".Open")),
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
                      mutate(dates = dates$`pricedata[[1]]$dates`, .before = 1) -> SP500_data 
  SP500_data$change <- 100*SP500_data[,5]/first(SP500_data[,5])
  return(SP500_data)
}

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
  
all_price <- function(sector, n_rsi, n_ma, n_mas, rsi_lwr, rsi_hgr){
    #' Data table constructer
    #' 
    #' @description Creates a dataframe with information for the
    #' trading opportunity page
    #' 
    #' @param sector Market sector
    #' @param n_rsi Number of days smoothed for RSI
    #' @param n_ma  Number of days smoothed for MA long
    #' @param n_mas Number of days for smoothed MA short
    latest <- SP500[nrow(SP500),2:ncol(SP500)] %>%   # Comparing latest price, ma short, ma long and rsi
      t(.) %>%                                       # Transpose
        as.data.frame(.) %>% 
          mutate(stocks_df$sector, .before = 1) %>% 
            mutate(stocks_df$company, .before = 1) %>% 
              mutate(ma=ma_trans(n_ma)) %>%                  # Add MA Long values
                mutate(mas=ma_trans(n_mas)) %>%              # Add MA Short values
                  mutate(rsi=rsi_trans(n_rsi)) %>%           # Add RSI values
                   `colnames<-`(c("Stocks","Sector", "Price", "MA_L", "MA_S", "RSI"))
    
     latest$signal <-  ifelse(latest$MA_S > latest$MA_L & latest$RSI < rsi_lwr, # ifelse for signal
                            
                            c("buy"),
                            
                              ifelse(latest$MA_S < latest$MA_L & latest$RSI > rsi_hgr, 
                                   
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
                            
                            ifelse(outdata[,4] < outdata[,2] & outdata[,3] > 70, # m? bruke [,2] for ? hente pris for alle aksjer
                                   
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
                                        numericInput(inputId = "SMAS", 
                                                    label = "Short Simple Moving Average (SMA)",
                                                    value = 20,
                                                    min = 10, 
                                                    max = 100),
                                        numericInput(inputId = "SMAL", 
                                                    label = "Long Simple Moving Average (SMA)",
                                                    value = 50,
                                                    min = 10, 
                                                    max = 200),
                                        sliderInput(inputId = "RSI", 
                                                    label = "Relative Strength Index (RSI)",
                                                    value = 14,
                                                    min = 5, 
                                                    max = 30),
                                        numericInput(inputId = "cRSI_lwr", 
                                                     label = "RSI lower cut-off",
                                                     value = 35,
                                                     min = 1, 
                                                     max = 50),
                                        numericInput(inputId = "cRSI_hgr", 
                                                     label = "RSI higher cut-off",
                                                     value = 65,
                                                     min = 51, 
                                                     max = 100),
                                      ),
                                      
                                      mainPanel(
                                        h3("STOCK ANALYSIS"),
                                        htmlOutput("compare"),
                                        h3("INTERACTIVE CHART"),
                                        plotlyOutput("priceplot")
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
                                        numericInput(inputId = "MA_L", 
                                                    label = "Long Simple Moving Average (SMA)",
                                                    value = 50,
                                                    min = 1, 
                                                    max = 200),
                                        numericInput(inputId = "MA_S", 
                                                    label = "Short Smple Moving Average (SMA)",
                                                    value = 20,
                                                    min = 1, 
                                                    max = 200),
                                        sliderInput(inputId = "RSI_all", 
                                                    label = "Relative Strength Index (RSI)",
                                                    value = 14,
                                                    min = 1, 
                                                    max = 30),
                                        numericInput(inputId = "RSI_lwr", 
                                                     label = "RSI lower cut-off",
                                                     value = 35,
                                                     min = 1, 
                                                     max = 50),
                                        numericInput(inputId = "RSI_hgr", 
                                                     label = "RSI higher cut-off",
                                                     value = 65,
                                                     min = 51, 
                                                     max = 100),
                                        ),
                                        mainPanel(
                                          dataTableOutput("tradingOportunity")
                                        )
                          )),
                          icon = icon("chart-line")),
                 
                 
                  #------------------- THIRD PANEL ------------------------
                 tabPanel("ABOUT",
                          h1("About this project"),
                          h6("Episode IV", align = "center"),
                          h6("A NEW HOPE", align = "center"),
                          h5("It is a period of civil war.", align = "center"),
                          h4("Rebel spaceships, striking", align = "center"),
                          h3("from a hidden base, have won", align = "center"),
                          h2("their first victory against the", align = "center"),
                          h1("evil Galactic Empire.", align = "center"),
                          icon = icon("info-circle")))

server <-  function(input, output){
  
  
 
  #------------------------------ FIRST PANEL ----------------------------
  data <- reactive({
    extracter(input$stockname)
  })
  
  output$priceplot <- renderPlotly({
    data = data()
    suppressWarnings(subplot(
      plot_ly(x = data[,1],
              type = "candlestick",
              open = data[,2],
              close = data[,5],
              high = data[,3], 
              low = data[,4], 
              name = input$stockname) %>%
        add_lines(x = data[,1], 
                  y = SMA(data[,5], n = input$SMAS), 
                  line = list(dash="solid", 
                              width = 1, 
                              color= "orange"), 
                  name = paste0("SMA",input$SMAS)) %>% 
        add_lines(x = data[,1], 
                  y = SMA(data[,5], n = input$SMAL), 
                  line = list(dash="solid", 
                              width = 1, 
                              color= "blue"), 
                  name = paste0("SMA",input$SMAL)) %>% 
        layout(title = paste(input$stockname),
               height = 1200,
               xaxis = list(
                 rangeselector = list(
                   buttons = list(
                     list(
                       count = 14,
                       label = "14D",
                       step = "day",
                       stepmode = "backward"),
                     list(
                       count = 1,
                       label = "1M",
                       step = "month",
                       stepmode = "backward"),
                     list(
                       count = 3,
                       label = "3M",
                       step = "month",
                       stepmode = "backward"),
                     list(
                       count = 6,
                       label = "6M",
                       step = "month",
                       stepmode = "backward"),
                     list(step = "all"))),
                 rangeslider = list(visible = F))),
      plot_ly(data,
              x = data[,1],
              y = RSI(data[,5], input$RSI),
              type = "scatter",
              mode = "line", 
              name = paste("RSI", input$RSI)) %>% 
        add_lines(y = 30, showlegend = F, line = list(color= "grey", widthh=0.2, dash="dot")) %>% 
        add_lines(y = 70, showlegend = F, line = list(color= "grey", widthh=0.2, dash="dot")) %>% 
        layout(yaxis = list(range=c(5,95)),
               height = 700), 
      plot_ly(data,
              x = data[,1],
              y = data[,6],
              type = "bar",
              name = "Volume",
              color = "black") %>% 
        layout(height = 700), 
      nrows = 3, shareX = TRUE, heights = c(0.7, 0.2, 0.1)))
  })
  
  output$compare <- renderText({
    data = data()
    compar <- paste0("With the inputs you have selected our recommendation is to ",
                    ifelse(
                      last(SMA(data[,5], n = input$SMAS)) > last(SMA(data[,5], n = input$SMAL)) &
                                                                   last(RSI(data[,5], n = input$RSI)) < input$cRSI_lwr,
                      "<B>buy </B>", ifelse(
                        last(SMA(data[,5], n = input$SMAS)) < last(SMA(data[,5], n = input$SMAL)) |
                          last(RSI(data[,5], n = input$RSI)) > input$cRSI_hgr,
                        "<B>sell </B>", "<B>hold </B>"
                      )
                    ),
                    input$stockname,
                    " stocks.",
                    "</p>",
                    " In the last 12 months, ", 
                    input$stockname, 
                    " has ", 
                    ifelse(last(data[,8]) > last(benchmark$change), "outperformed ", "been outperformed by "), 
                    "the S&P 500 index by ",
                    round(last(data[,8]) - last(benchmark$change), digits = 2),
                    "%. In the period the S&P 500 index ",
                    ifelse(last(benchmark$change) > first(benchmark$change), " rose by ", " fell with "),
                    round(last(benchmark$change)- 100, digits = 2),
                    "%, meanwhile ",
                    input$stockname,
                    ifelse(last(data[,8] > first(data[,8])), " rose by ", " fell with "),
                    round(last(data[,8])-100, digits = 2),
                    "%."
                    )
  })
  

  #------------------------- SECOND PANEL--------------------------------
  all_data <- reactive({
    suppressWarnings(all_price(input$sector, input$RSI_all, input$MA_L, input$MA_S, input$RSI_lwr, input$RSI_hgr))
  })
  # Adding all the signals
  output$tradingOportunity <- renderDataTable({
    data = all_data()
    data[,1:7]
  })
  
  
}

shinyApp(ui = ui, server = server)

  
