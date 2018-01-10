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

# holidays_events <- read.csv2("holidays_events.csv", header = TRUE, sep = ',')
# items <- read.csv2("items.csv", header = TRUE, sep = ',')
# oil <- read.csv2("oil.csv", header = TRUE, sep = ',')
# stores <- read.csv2("stores.csv", header = TRUE, sep = ',')
# transactions <- read.csv2("transactions.csv", header = TRUE, sep = ',')
# test <- read.csv2("test.csv", header = TRUE, sep = ',')
# train <- read.csv2("train.csv", header = TRUE, sep = ',')
#save.image('GFDataDump.RData')

apply(oil, 2, function(x) length(which(x == "" | is.na(x) | x == "NA")))
# 43 dcoilwtico

# apply(train[len], 2, function(x) length(which(x == "" | is.na(x) | x == "NA")))
# apply(train, 2, function(x) length(which(x == "" | is.na(x) | x == "NA")))
# not working

str(holidays_events)
holidays_events$date <- as.Date(holidays_events$date)
apply(holidays_events, 2, function(x) length(which(x == "" | is.na(x) | x == "NA")))

str(items)
items$class <- as.factor(items$class)
items$perishable <- as.factor(items$perishable)
items$item_nbr <- as.factor(items$item_nbr)
apply(items, 2, function(x) length(which(x == "" | is.na(x) | x == "NA")))

str(oil)
oil$date <- as.character(oil$date)
oil$dcoilwtico <- as.numeric(as.character(oil$dcoilwtico))
apply(oil, 2, function(x) length(which(x == "" | is.na(x) | x == "NA")))
# Impute using KNN
oil <- kNN(oil[,c('date','dcoilwtico')], variable = c('dcoilwtico'), k=5)
oil <- oil[,-3]
oil$date <- as.Date(oil$date)

str(stores)
unique(stores$cluster)
stores$cluster <- as.factor(stores$cluster)
apply(stores, 2, function(x) length(which(x == "" | is.na(x) | x == "NA")))

str(transactions)
transactions$date <- as.Date(transactions$date)
unique(transactions$store_nbr)
transactions$store_nbr <- as.factor(transactions$store_nbr)
apply(transactions, 2, function(x) length(which(x == "" | is.na(x) | x == "NA")))

str(test)
test$date <- as.Date(test$date)
test$store_nbr <- as.factor(test$store_nbr)
test$item_nbr <- as.factor(test$item_nbr)

str(train)
train$date <- ymd(train$date)
train$store_nbr <- as.factor(train$store_nbr)
train$item_nbr <- as.factor(train$item_nbr)
train$unit_sales <- as.numeric(as.character(train$unit_sales))
train$onpromotion <- as.character(train$onpromotion)
train$onpromotion <- ifelse(train$onpromotion == "", NA, train$onpromotion)
train$onpromotion <- as.factor(train$onpromotion)
trainPromoMiss <- train %>% filter(is.na(onpromotion))
nrow(trainPromoMiss)*100/nrow(train)
# 17.25% missing Promo values
#save.image('GFDataDumpFormatted.RData')



############################## <= 50 mine #######################################

# find items <=50
train1 <- train %>% filter(!is.na(onpromotion))
freqitemstore <- train1 %>% group_by(item_nbr) %>% count()
freqitemstoreReduced <- freqitemstore %>% filter(n <= 50)

freqitemstoreReduced <- as.integer(as.character(freqitemstoreReduced$item_nbr))

dftrain <- train %>% filter(item_nbr %in% freqitemstoreReduced)
dftrain$itemstr <- paste0(dftrain$store_nbr,'|',dftrain$item_nbr)
dftrain <- dftrain %>% arrange(item_nbr,store_nbr,date)
row.names(dftrain) <- NULL

dftest <- test %>% filter(item_nbr %in% freqitemstoreReduced)
dftest$itemstr <- paste0(dftest$store_nbr,'|',dftest$item_nbr)
dftest <- dftest[dftest$itemstr %in% dftrain$itemstr,]
dftest <- dftest %>% arrange(item_nbr,store_nbr,date)
row.names(dftest) <- NULL
items <- freqitemstoreReduced

dftest$tunitsales <- NA
it=0
for (itm in items){
  it<-it+1
  #itm=2011437
  start_time <- Sys.time()
  print(paste0("Iteration ",it,": Starting Item ",itm))
  df <- dftrain %>% filter(item_nbr == itm)
  stores <- unique(as.integer(as.character(df$store_nbr)))
  for (str in stores){
    #str=7
    df1 <- df %>% filter(store_nbr == str)
    tseries <- round(abs(df1$unit_sales))
    tseries <- ifelse(tseries == 0, 1, tseries)
    tseries <- round(abs(tsclean(tseries)))
    model.2 <- auto.arima(tseries)
    #model.2 <- ets(tseries)
    #model.2 <- nnetar(tseries)
    frcst <- forecast(model.2, h = 16)

    #model.2 <- auto.arima(tseries[1:10]);frcst <- forecast(model.2, h = 7);frcst$mean
    #ensbl.1 <- hybridModel(tseries[1:10], models="aent"); frcst <- forecast(ensbl.1, h = 7);frcst$pointForecasts
    dftest[dftest$item_nbr == itm & dftest$store_nbr == str, 'tunitsales'] <- round(abs(frcst$mean))
  }
  end_time <- Sys.time()
  print(paste0("Time Elapsed: ",end_time - start_time))
}
apply(dftest, 2, function(x) length(which(is.na(x))))
dftest$tunitsales <- ifelse(dftest$tunitsales == 0, 1, dftest$tunitsales)
write.table(dftest, file = "resultForLessitem50ARIMAmine.csv", quote = FALSE, row.names=FALSE, sep=",")



# ############################### < 20 mishra #######################################
# freqitemstore <- train %>% group_by(item_nbr, store_nbr) %>% count()
# freqitemstoreReduced <- freqitemstore %>% filter(n < 20)
# 
# dftrain <- inner_join(train, freqitemstoreReduced)
# dftrain$itemstr <- paste0(dftrain$store_nbr,'|',dftrain$item_nbr)
# dftrain <- dftrain %>% arrange(item_nbr,store_nbr,date)
# row.names(dftrain) <- NULL
# 
# dftest <- inner_join(test, freqitemstoreReduced)
# dftest$itemstr <- paste0(dftest$store_nbr,'|',dftest$item_nbr)
# dftest <- dftest[dftest$itemstr %in% dftrain$itemstr,]
# dftest <- dftest %>% arrange(item_nbr,store_nbr,date)
# row.names(dftest) <- NULL
# 
# items <- unique(as.integer(as.character(dftrain$item_nbr)))
# 
# dftest$tunitsales <- NA
# it=0
# for (itm in items){
#   it<-it+1
#   #itm=1005458
#   start_time <- Sys.time()
#   print(paste0("Iteration ",it,": Starting Item ",itm))
#   df <- dftrain %>% filter(item_nbr == itm)
#   stores <- unique(as.integer(as.character(df$store_nbr)))
#   for (str in stores){
#       #str=37
#       df1 <- df %>% filter(store_nbr == str)
#       tseries <- round(abs(df1$unit_sales))
#       tseries <- ifelse(tseries == 0, 1, tseries)
#       tseries <- round(abs(tsclean(tseries)))
#       model.2 <- auto.arima(tseries)
#       #model.2 <- ets(tseries)
#       #model.2 <- nnetar(tseries)
#       frcst <- forecast(model.2, h = 16)
# 
#       #model.2 <- auto.arima(tseries[1:10]);frcst <- forecast(model.2, h = 7);frcst$mean
#       #ensbl.1 <- hybridModel(tseries[1:10], models="aent"); frcst <- forecast(ensbl.1, h = 7);frcst$pointForecasts
#       dftest[dftest$item_nbr == itm & dftest$store_nbr == str, 'tunitsales'] <- round(abs(frcst$mean))
#   }
#   end_time <- Sys.time()
#   print(paste0("Time Elapsed: ",end_time - start_time))
#   }
# apply(dftest, 2, function(x) length(which(is.na(x))))
# dftest$tunitsales <- ifelse(dftest$tunitsales == 0, 1, dftest$tunitsales)
# write.table(dftest, file = "resultForLess20ARIMA.csv", quote = FALSE, row.names=FALSE, sep=",")


############################### mishra check Inf #######################################
# 
# tt <- train %>% filter(item_nbr==2054300)
# 
# pred21 <- read.csv2("inf_after_123456.csv", header = TRUE, sep = ',')
# pred21$itemstr <- paste0(pred21$store_nbr,'|',pred21$item_nbr)
# pred21$item_nbr <- as.factor(pred21$item_nbr)
# pred21$store_nbr <- as.factor(pred21$store_nbr)
# 
# dftrain <- inner_join(train, pred21)
# dftrain <- dftrain %>% arrange(item_nbr,store_nbr,date)
# row.names(dftrain) <- NULL
# unique(dftrain$item_nbr)
# 
# # dftest <- inner_join(test, pred21)
# # dftest <- dftest[dftest$itemstr %in% dftrain$itemstr,]
# # dftest <- dftest %>% arrange(item_nbr,store_nbr,date)
# # row.names(dftest) <- NULL
# dftest <- dftrain[0,c(2,3,4,5)]
# names(dftest) <- c('date','item_nbr','store_nbr','unit_sales')
# items <- unique(as.integer(as.character(dftrain$item_nbr)))
# it=0
# testdates <- as.character(test[test$item_nbr==96995 & test$store_nbr==1, 'date'])
# for (itm in items){
#   it<-it+1
#   #itm=1001305
#   start_time <- Sys.time()
#   print(paste0("Iteration ",it,": Starting Item ",itm))
#   df <- dftrain %>% filter(item_nbr == itm)
#   stores <- unique(as.integer(as.character(df$store_nbr)))
#   for (str in stores){
#     #str=22
#     df1 <- df %>% filter(store_nbr == str)
#     tseries <- round(abs(df1$unit_sales))
#     tseries <- ifelse(tseries == 0, 1, tseries)
#     tseries <- round(abs(tsclean(tseries)))
#     #model.2 <- hybridModel(tseries, models="ae") #frcst$pointForecasts
#     model.2 <- auto.arima(tseries)
#     #model.2 <- ets(tseries)
#     #model.2 <- nnetar(tseries)
#     frcst <- forecast(model.2, h = 16)
#     
#     #model.2 <- auto.arima(tseries[1:10]);frcst <- forecast(model.2, h = 7);frcst$mean
#     #ensbl.1 <- hybridModel(tseries, models="aent"); frcst <- forecast(ensbl.1, h = 16); frcst$pointForecasts
#     #temp <- data.frame(cbind(rep(itm, 16), rep(str, 16), round(abs(frcst$mean))))
#     temp <- data.frame(cbind(testdates,rep(itm, 16), rep(str, 16), round(abs(frcst$mean))))
#     names(temp) <- c('date','item_nbr','store_nbr','unit_sales')
#     dftest <- rbind(dftest, temp)
#     #dftest[dftest$item_nbr == itm & dftest$store_nbr == str, 'tunitsales'] <- round(abs(frcst$mean))
#     temp <- temp[0,]
#   }
#   end_time <- Sys.time()
#   print(paste0("Time Elapsed: ",end_time - start_time))
# }
# names(dftest) <- c('date','item_nbr','store_nbr','unit_sales')
# apply(dftest, 2, function(x) length(which(is.na(x))))
# dftest$unit_sales <- ifelse(dftest$unit_sales == 0, 1, dftest$unit_sales)
# write.table(dftest, file = "resultInfExtraARIMA.csv", quote = FALSE, row.names=FALSE, sep=",")
# 


# ############################### mishra check CV #######################################
# 
# pred2 <- read.csv2("prediction2_left.csv", header = TRUE, sep = ',')
# pred21 <- pred2[,c(1,2)]
# pred21$itemstr <- paste0(pred21$store,'|',pred21$item)
# names(pred21) <- c('item_nbr','store_nbr','itemstr')
# pred21$item_nbr <- as.factor(pred21$item_nbr)
# pred21$store_nbr <- as.factor(pred21$store_nbr)
# 
# pred3 <- read.csv2("prediction3_left.csv", header = TRUE, sep = ',')
# pred31 <- pred3[,c(1,2)]
# pred31$itemstr <- paste0(pred31$store,'|',pred31$item)
# names(pred31) <- c('item_nbr','store_nbr','itemstr')
# pred31$item_nbr <- as.factor(pred31$item_nbr)
# pred31$store_nbr <- as.factor(pred31$store_nbr)
# 
# dftrain <- inner_join(train, pred21)
# dftrain <- dftrain %>% arrange(item_nbr,store_nbr,date)
# row.names(dftrain) <- NULL
# 
# # dftest <- inner_join(test, pred21)
# # dftest <- dftest[dftest$itemstr %in% dftrain$itemstr,]
# # dftest <- dftest %>% arrange(item_nbr,store_nbr,date)
# # row.names(dftest) <- NULL
# dftest <- dftrain[0,c(2,3,4,5)]
# names(dftest) <- c('date','item_nbr','store_nbr','unit_sales')
# items <- unique(as.integer(as.character(dftrain$item_nbr)))
# it=0
# testdates <- as.character(test[test$item_nbr==96995 & test$store_nbr==1, 'date'])
# for (itm in items){
#   it<-it+1
#   #itm=2013931
#   start_time <- Sys.time()
#   print(paste0("Iteration ",it,": Starting Item ",itm))
#   df <- dftrain %>% filter(item_nbr == itm)
#   stores <- unique(as.integer(as.character(df$store_nbr)))
#   for (str in stores){
#     #str=45
#     df1 <- df %>% filter(store_nbr == str)
#     tseries <- round(abs(df1$unit_sales))
#     tseries <- ifelse(tseries == 0, 1, tseries)
#     tseries <- round(abs(tsclean(tseries)))
#     #model.2 <- auto.arima(tseries)
#     model.2 <- ets(tseries)
#     #model.2 <- nnetar(tseries)
#     frcst <- forecast(model.2, h = 16)
#     
#     #model.2 <- auto.arima(tseries[1:10]);frcst <- forecast(model.2, h = 7);frcst$mean
#     #ensbl.1 <- hybridModel(tseries[1:10], models="aent"); frcst <- forecast(ensbl.1, h = 7);frcst$pointForecasts
#     #temp <- data.frame(cbind(rep(itm, 16), rep(str, 16), round(abs(frcst$mean))))
#     temp <- data.frame(cbind(testdates,rep(itm, 16), rep(str, 16), round(abs(frcst$mean))))
#     names(temp) <- c('date','item_nbr','store_nbr','unit_sales')
#     dftest <- rbind(dftest, temp)
#     #dftest[dftest$item_nbr == itm & dftest$store_nbr == str, 'tunitsales'] <- round(abs(frcst$mean))
#     temp <- temp[0,]
#   }
#   end_time <- Sys.time()
#   print(paste0("Time Elapsed: ",end_time - start_time))
# }
# names(dftest) <- c('date','item_nbr','store_nbr','unit_sales')
# apply(dftest, 2, function(x) length(which(is.na(x))))
# dftest$unit_sales <- ifelse(dftest$unit_sales == 0, 1, dftest$unit_sales)
# write.table(dftest, file = "resultFormorethan50ETS.csv", quote = FALSE, row.names=FALSE, sep=",")








