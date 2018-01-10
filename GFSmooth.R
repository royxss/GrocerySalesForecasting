setwd("C:\\Users\\SROY\\Documents\\CodeBase\\Datasets\\GrocerySalesForecast")
rm(list=ls())
seedVal = 17869
options(warn=-1)
options(scipen=999)

# Libraries
library(dplyr)
library(missForest)
library(VIM)
library(lubridate)
library(ggplot2)
library(reshape2)
library(zoo)
library(dummies)
library(forecastHybrid)
library(tsoutliers)
library(caret)

load('GFDataDumpFormatted.RData')
rm(trainPromoMiss)
memory.limit(30000)

subdf <- read.csv2("submission123456_to_smooth.csv", header = TRUE, sep = ',')
subdf$smoothUnitSales <- NA
subdf <- subdf %>% arrange(item_nbr, store_nbr, date)
row.names(subdf) <- NULL
#subdf$unit_sales <- round(abs(as.integer(as.character(subdf$unit_sales))))
subdfbkp <- subdf

#check count
sum(as.integer(subdf$exceed100Flag))
sum(as.integer(subdf$exceed200Flag))
sum(as.integer(subdf$exceed300Flag))
#View(subdf[subdf$item_nbr==2054300 & subdf$store_nbr==37,])


#Filter based on flag. Get relevant item and stores
subdfss <- subdf %>% filter(exceed300Flag==1) %>% select(item_nbr, store_nbr) %>% distinct()
subdfss$item_nbr <- as.factor(subdfss$item_nbr)
subdfss$store_nbr <- as.factor(subdfss$store_nbr)
trainss <- inner_join(train, subdfss)

itemsTest <- unique(subdfss$item_nbr)
itemsTrain <- unique(trainss$item_nbr)
it=0
for (itm in itemsTest){
  it<-it+1
  #itm=2054300
  start_time <- Sys.time()
  print(paste0("Iteration ",it,": Starting Item ",itm))
  
  dftrain <- trainss %>% filter(item_nbr == itm)
  stores <- unique(dftrain$store_nbr)
  for (str in stores){
    #str=37
    df <- dftrain %>% filter(store_nbr == str)
    df <- df %>% arrange(date)
    tseries1 <- round(abs(df$unit_sales))
    
    dftest <- subdf %>% filter(item_nbr==itm & store_nbr==str)
    dftest <- dftest %>% arrange(date)
    tseries2 <- round(abs(as.integer(as.character(dftest$unit_sales))))
    
    tseries <- append(tseries1, tseries2)
    tseries <- round(abs(tsclean(tseries)))
    
    subdf[subdf$item_nbr==itm & subdf$store_nbr==str,'smoothUnitSales'] <- tseries[(length(tseries)-15):length(tseries)]
    tseries <- NULL
  }
  end_time <- Sys.time()
  print(paste0("Time Elapsed: ",end_time - start_time))
}

write.table(subdf, file = "submission123456_smooth300.csv", quote = FALSE, row.names=FALSE, sep=",")

subdfss$item_nbr <- as.integer(as.character(subdfss$item_nbr))
subdfss$store_nbr <- as.integer(as.character(subdfss$store_nbr))
temp <- inner_join(subdf, subdfss)
write.table(temp, file = "submission123456_smooth300Only.csv", quote = FALSE, row.names=FALSE, sep=",")
