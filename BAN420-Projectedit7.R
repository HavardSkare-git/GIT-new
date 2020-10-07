# Libraries ---- 
setwd("C:/R")
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

#RMarkdown

install.packages("rmarkdown")
library(rmarkdown)




# Cleaning the data -----

# List of companies in OBX

url <- read_html("https://no.wikipedia.org/wiki/OBX-indeksen")

OBX <- url %>% 
  html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/table[1]') %>% 
  html_table()

# OSEBX ----
#urlOSEBX <- read_html("https://no.wikipedia.org/wiki/OSEBX-indeksen")

#OSEBX <- url %>% 
#  html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/table') %>% 
#  html_table()

#####  
OBX <- as.data.frame(OBX) %>% 
  select("Tickersymbol")

# OBX stocks are listed as TICKERNAME.OL on Yahoo Finance. 
# Removing "OSE: "

OBX <- gsub("OSE: ", "", OBX$Tickersymbol)

OBX <- as.data.frame(OBX) 

# Adding ".OL"
OBX$OL <- ".OL"

OBX <- paste0(OBX$OBX, OBX$OL)

# Using the tidyquant package to extract the latest pricedata 
# on the OBX stocks from Yahoo Finance

today <- Sys.Date()

from.date <- today %m+% months(-12)

pricedata.OBX <- lapply(OBX, function(x) {
  outdata <- getSymbols(x, 
                    from = from.date, 
                    to = today, 
                    warnings = FALSE,
                    auto.assign = FALSE)
  outdata <- data.frame(dates = index(outdata), coredata(outdata))
  return(outdata)
})

# Putting all data into same data frame and filtering

pricedata.OBXd <- na.omit(as.data.frame(pricedata.OBX))

pricedata.OBX <- pricedata.OBXd %>% 
  select("dates", 
         contains("Close"))

pricedata.OBX <- xts(pricedata.OBX[,-1], 
                     order.by=as.Date(pricedata.OBX[,1], 
                                      "%m/%d/%Y"))

# ------
####################################################################
# RSI 
rsi.OBX <- matrix(0, 
                  nrow = nrow(pricedata.OBX), 
                  ncol = length(OBX)+1, 
                  index(pricedata.OBX))

rsi.OBX <- as.data.frame(rsi.OBX)

rsi.OBX[,1] <- pricedata.OBXd$dates

for (i in 1:ncol(pricedata.OBX)) {
  rsi.OBX[,i+1] <- 
    RSI(pricedata.OBX[,i])
}

# Moving average

ma.OBX <- matrix(0, 
                 nrow = nrow(pricedata.OBX), 
                 ncol = length(OBX)+1, 
                 index(pricedata.OBX))

ma.OBX <- as.data.frame(ma.OBX)

ma.OBX[,1] <- pricedata.OBXd$dates

for (i in 1:ncol(pricedata.OBX)) {
  ma.OBX[,i+1] <-
    rollmean(pricedata.OBX[,i], 
                      100, 
                      fill = list(NA, NULL, NA), 
                      align = "right")
}

####

pricedata.OBX <- as.data.frame(pricedata.OBX)

#

rsi.list <- list()
price.ma.list <- list()

for(i in 1:length(OBX)){
  if(pricedata.OBX[nrow(pricedata.OBX),i] > ma.OBX[nrow(ma.OBX), i+1] & 
     rsi.OBX[nrow(rsi.OBX), i+1] < 10){
    a <- eval(substitute(ggplot(rsi.OBX)+
                           geom_line(data = rsi.OBX,
                                     aes(x = ma.OBX$V1,
                                         y = rsi.OBX[,i+1]))+
                           geom_hline(yintercept = c(30,70), 
                                      col = "red", 
                                      linetype = "dotted")+
                           ggtitle(paste(OBX[i], "er en kjøpskandidat"))+
                           xlab("Dato")+
                           ylab("Pris")+
                           theme_bw(),
                         list(i = i)))
    print(i)
    print(a)
    rsi.list[[i]] <- a
    b <- eval(substitute(ggplot(pricedata.OBX)+
                           geom_line(data = pricedata.OBX,
                                     aes(x = ma.OBX$V1,
                                         y = pricedata.OBX[,i]))+
                           geom_line(data = ma.OBX, 
                                     aes(x = ma.OBX$V1, 
                                         y = ma.OBX[,i+1]),
                                         col = "green")+
                           ggtitle(paste(OBX[i], "er en kjøpskandidat"))+
                           xlab("Dato")+
                           ylab("Pris")+
                           theme_bw(),
                         list(i = i)))
    print(i)
    print(b)
    price.ma.list[[i]] <- b
  } else if(pricedata.OBX[nrow(pricedata.OBX),i] > ma.OBX[nrow(ma.OBX), i+1] & 
            rsi.OBX[nrow(rsi.OBX), i+1] > 30) {
    c <- eval(substitute(ggplot(rsi.OBX)+
                           geom_line(data = rsi.OBX,
                                     aes(x = ma.OBX$V1,
                                         y = rsi.OBX[,i+1]))+
                           geom_hline(yintercept = c(30,70), 
                                      col = "red", 
                                      linetype = "dotted")+
                           ggtitle(paste(OBX[i], "er en salgskandidat"))+
                           xlab("Dato")+
                           ylab("Pris")+
                           theme_bw(),
                         list(i = i)))
    print(i)
    print(c)
    rsi.list[[i]] <- c
    d <- eval(substitute(ggplot(pricedata.OBX)+
                           geom_line(data = pricedata.OBX,
                                     aes(x = ma.OBX$V1,
                                         y = pricedata.OBX[,i]))+
                           geom_line(data = ma.OBX, 
                                     aes(x = ma.OBX$V1, 
                                         y = ma.OBX[,i+1]),
                                     col = "green")+
                           ggtitle(paste(OBX[i], "er en salgskandidat"))+
                           xlab("Dato")+
                           ylab("Pris")+
                           theme_bw(),
                         list(i = i)))
    print(i)
    print(d)
    price.ma.list[[i]] <- d
  }}










