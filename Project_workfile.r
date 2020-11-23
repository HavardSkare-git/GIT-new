# Libraries ---- 
setwd("C:/Users/Havar/Documents/GIT-new")
install.packages("tidyquant")
install.packages("tidyverse")
install.packages("rvest") # To get table from Wikipedia
install.packages("XML") # To get table from Wikipedia
install.packages("httr")
install.packages("magrittr")
install.packages("xml")
install.packages("ggplot2")
library(tidyquant)
library(tidyverse)
library(rvest)
library(XML)
library(httr)
library(magrittr)
library(xts)
library(TTR)
library(ggplot2)

setwd("C:/Users/Havar/Documents/GIT-new")
#RMarkdown

install.packages("rmarkdown")
library(rmarkdown)




# Cleaning the data -----

# List of companies in OBX



OBX <- 
  read_html("https://no.wikipedia.org/wiki/OBX-indeksen") %>% 
  html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/table[1]') %>% 
  html_table() %>%
  as.data.frame(.) %>% 
  select("Tickersymbol") %>%
  map_df(~gsub("OSE: ", "",.)) %>% 
  map_df(~paste0(.,".OL")) %>% 
  subset(., .$Tickersymbol == "AKER.OL") %>% 
  unlist(.) %>% 
  as.vector(.)

# OBX stocks are listed as TICKERNAME.OL on Yahoo Finance. 
# Removing "OSE: "


# Adding ".OL"


# Using the tidyquant package to extract the latest pricedata 
# on the OBX stocks from Yahoo Finance

pricedata.OBX <- map_dfc(OBX, function(x) {
  outdata <- getSymbols(x, 
                        from = Sys.Date() %m+% months(-12), 
                        to = Sys.Date(), 
                        warnings = FALSE,
                        auto.assign = FALSE)
  
  outdata <- data.frame(dates = index(outdata), coredata(outdata)) %>% 
              select("dates", contains("Close")) %>% 
                na.omit(.)
  
  return(outdata)
}) 

#pricedata.OBX <- na.omit(pricedata.OBX) %>% 
#  select("dates...1", contains("Close"))

# ------
####################################################################
# RSI 


#rsi.obx <- map_df(pricedata.OBX[,2:ncol(pricedata.OBX)],
#             function(x) RSI(x)) %>% 
#               mutate(date=pricedata.OBX$dates, .before = 1)

pricedata.OBX$RSI <- RSI(pricedata.OBX[2])


# Moving average

#ma.OBX <- map_df(pricedata.OBX[,2:ncol(pricedata.OBX)],
#                 function(x) rollmean(x, 100, fill = list(NA,NULL,NA),
#                   align = "right")) %>% 
#                     mutate(date=pricedata.OBX$dates...1, .before = 1)


pricedata.OBX$MA <- rollmean(pricedata.OBX$AKER.OL.Close, 100,
                             fill = list(NA, NULL, NA),
                             align = "right")


  
if(pricedata.OBX[nrow(pricedata.OBX),2] > pricedata.OBX[nrow(pricedata.OBX),4] & pricedata.OBX[nrow(pricedata.OBX),3]< 10){
    a <- ggplot(pricedata.OBX) + 
      geom_line(aes(x = dates,
                    y = RSI)) +
      geom_hline(yintercept = c(30,70), 
                 col = "red", 
                 linetype = "dotted")+
      ggtitle(paste(OBX, "er en kjopskandidat"))+
      xlab("Dato")+
      ylab("Pris")+
      theme_bw()
    print(a)
    
    b <- ggplot(pricedata.OBX)+
      geom_line(aes(x = AKER.OL.Close,
                    y = dates))+
      geom_line(aes(y = MA),
    col = "green")+
    ggtitle(paste(OBX, "er en kjopskandidat"))+
    xlab("Dato")+
    ylab("Pris")+
    theme_bw()
    print(b)
  } else if (pricedata.OBX[nrow(pricedata.OBX),2] > pricedata.OBX[nrow(pricedata.OBX),4] & pricedata.OBX[nrow(pricedata.OBX),3] > 70){
    c <- ggplot(pricedata.OBX) + 
      geom_line(aes(x = dates,
                    y = RSI)) +
      geom_hline(yintercept = c(30,70), 
                 col = "red", 
                 linetype = "dotted")+
      ggtitle(paste(OBX, "er en salgskandidat"))+
      xlab("Dato")+
      ylab("Pris")+
      theme_bw()
    print(c)
    
    d <- ggplot(pricedata.OBX)+
      geom_line(aes(x = dates,
                    y = AKER.OL.Close))+
      geom_line(aes(x = dates,
                    y = MA),
                col = "green")+
      ggtitle(paste(OBX, "er en salgskandidat"))+
      xlab("Dato")+
      ylab("Pris")+
      theme_bw()
    print(d)
}
                              
  



