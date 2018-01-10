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

#4 arts: normal, <50, missing stores in items, missing items
# consolidate normal, <50
# KNN for missing stores, then replace/substitute missing items

resultLessitem50 <- read.csv2("resultForLessitem50ARIMAmine.csv", header = TRUE, sep = ',')
resultsP1.1 <- read.csv2("resultsP1.1.csv", header = TRUE, sep = ',')
resultsP1.2 <- read.csv2("resultsP1.2.csv", header = TRUE, sep = ',')
resultsP2.1 <- read.csv2("resultsP2.1.csv", header = TRUE, sep = ',')
resultsP2.2 <- read.csv2("resultsP2.2.csv", header = TRUE, sep = ',')
resultsP2.3 <- read.csv2("resultsP2.3.csv", header = TRUE, sep = ',')
resultsP3 <- read.csv2("resultsP3.csv", header = TRUE, sep = ',')
resultsP4 <- read.csv2("resultsP4.csv", header = TRUE, sep = ',')

df <- resultLessitem50
df <- df %>% mutate(SaleMonth = month(ymd(date)))
df <- df %>% mutate(SaleYear = year(ymd(date)))
df <- df %>% mutate(SaleWday = wday(ymd(date)))
df <- df %>% mutate(SaleDay = day(ymd(date)))
# df$SaleYear <- as.factor(df$SaleYear);df$SaleDay <- as.factor(df$SaleDay);
# df$SaleWday <- as.factor(df$SaleWday);df$SaleMonth <- as.factor(df$SaleMonth)
df$SalaryImpact <- ifelse(df$SaleDay %in% c(14,15,16,17,28,29,30,31,1,2), 'Yes', 'No')
df$SalaryImpact <- as.factor(df$SalaryImpact)
df$date <- as.Date(df$date)
df <- left_join(df, oil) #%>% left_join(., transactions)


apply(df, 2, function(x) length(which(is.na(x))))
#if date is 2017-08-19 and 2017-08-20 impute 48.59
#if date is 2017-08-26 and 2017-08-27 impute 47.65
df[df$date %in% c(as.Date('2017-08-19'),as.Date('2017-08-20')),'dcoilwtico'] <- 48.59
df[df$date %in% c(as.Date('2017-08-26'),as.Date('2017-08-27')),'dcoilwtico'] <- 47.65
df$dcoilwtico <- log(df$dcoilwtico,10)
df <- inner_join(df, stores)

df <- df[, names(resultsP1.1)]
#setdiff(names(resultsP1.1), names(df))

results <- bind_rows(resultsP1.1, resultsP1.2, resultsP2.1, resultsP2.2, resultsP2.3, resultsP3, resultsP4)
results$cluster <- as.factor(results$cluster)
results$dcoilwtico <- as.numeric(as.character(results$dcoilwtico))
results <- bind_rows(results, df)
results <- results %>% arrange(item_nbr, store_nbr, SaleDay)

#Clear 
rm(resultsP1.1, resultsP1.2, resultsP2.1, resultsP2.2, resultsP2.3, resultsP3, resultsP4, tt, df)

#fatorzie  SaleYear, SaleDay, store_nbr, SaleMonth, SaleWday, item_nbr
results$SaleYear <- as.factor(results$SaleYear)
results$SaleDay <- as.factor(results$SaleDay)
results$store_nbr <- as.factor(results$store_nbr)
results$SaleMonth <- as.factor(results$SaleMonth)
results$SaleWday <- as.factor(results$SaleWday)
results$item_nbr <- as.factor(results$item_nbr)

# Check missing
unqTrainItems <- sort(unique(as.integer(as.character(train$item_nbr))))
unqTestItems <- sort(unique(as.integer(as.character(test$item_nbr))))
missingInTrain <- setdiff(unqTestItems, unqTrainItems)

unqTestItemsStr <- test %>% distinct(item_nbr, store_nbr)
unqTestItemsStr <- unqTestItemsStr %>% filter(!item_nbr %in% missingInTrain)
unqResultItemsStr <- results %>% distinct(item_nbr, store_nbr)
unqTestItemsStr$itmstr <- paste0(unqTestItemsStr$item_nbr,'|',unqTestItemsStr$store_nbr)
unqResultItemsStr$itmstr <- paste0(unqResultItemsStr$item_nbr,'|',unqResultItemsStr$store_nbr)
extraMissTestItemStr <- unqTestItemsStr %>% filter(!itmstr %in% unqResultItemsStr$itmstr)

# View(unqTestItemsStr)
# View(unqResultItemsStr)
# View(extraMissTestItemStr[extraMissTestItemStr$item_nbr==96995,])
#length(unique(extraMissTestItemStr$item_nbr))

itemsToImpute <- unique(extraMissTestItemStr$item_nbr)
stores$store_nbr <- as.factor(stores$store_nbr)

it=0
imputeStoreReplace <- NULL
for (itm in itemsToImpute){
  it<-it+1
  print(paste0("starting iteration " ,it," for item ",itm))
  
  #itm = 103501
  df <- extraMissTestItemStr %>% filter(item_nbr==itm) %>% select(store_nbr)
  df$imputeStr <- -1
  df1 <- left_join(stores, df)
  df1$imputeStr <- ifelse(is.na(df1$imputeStr), -2, df1$imputeStr)
  df1$imputeStr <- ifelse(df1$imputeStr == -1, NA, df1$imputeStr)
  df1$imputeStr <- ifelse(df1$imputeStr == -2, df1$store_nbr, df1$imputeStr)
  
  # Impute using KNN
  df2 <- cbind(df1, dummy(df1$city), dummy(df1$state), dummy(df1$type), dummy(df1$cluster))
  df3 <- kNN(df2[,6:54], variable = c('imputeStr'), k=3)
  df4 <- cbind(df2[,1:6], df3[,1])
  names(df4)[7] <- 'imputedStore'
  df4$item_nbr <- itm
  imputeStoreReplace <- rbind(imputeStoreReplace, df4)
}
rm(df,df1,df2,df3,df4)
#save.image("GFConsolCheckpoint1.RData")

# memory.limit(50000)
# it=0
# resultsMissStore <- NULL
# for (itm in itemsToImpute){
#   start_time <- Sys.time()
#   #itm=567517
#   it<-it+1
#   print(paste0("starting iteration " ,it," for item ",itm))
#   
#   df <- imputeStoreReplace %>% filter(item_nbr==itm)
#   df <- df %>% filter(is.na(df$imputeStr))
#   dfStores <- unique(df$store_nbr)
#   resultsPart <- results %>% filter(item_nbr==itm)
#   for (stor in dfStores){
#     #stor=1
#     temp <- df %>% filter(store_nbr==stor)
#     replBy <- unique(temp$imputedStore)
#     imputNewStoreforItem <- resultsPart %>% filter(store_nbr==replBy)
#     imputNewStoreforItem$store_nbr <- stor
#     resultsMissStore <- bind_rows(resultsMissStore, imputNewStoreforItem)
#   }
#   end_time <- Sys.time()
#   print(paste0("Time Elapsed: ",end_time - start_time))
#   if(it %in% c(1000,2219)){
#     filename = paste0('resultsMissStore_',it, '.csv')
#     write.table(resultsMissStore, file = filename, quote = FALSE, row.names=FALSE, sep=",")
#     resultsMissStore <- NULL
#   }
# }
#Check
#apply(resultsMissStore, 2, function(x) length(which(is.na(x))))
#unique(resultsMissStore$store_nbr)
resultsMissStore_1000 <- read.csv2("resultsMissStore_1000.csv", header = TRUE, sep = ',')
resultsMissStore_2219 <- read.csv2("resultsMissStore_2219.csv", header = TRUE, sep = ',')
resultsMissStore <- bind_rows(resultsMissStore_1000, resultsMissStore_2219)

#item_nbr, Year, Day, store, cluster, month, wday     int dcoil
resultsMissStore$item_nbr <- as.factor(resultsMissStore$item_nbr)
resultsMissStore$SaleYear <- as.factor(resultsMissStore$SaleYear)
resultsMissStore$SaleDay <- as.factor(resultsMissStore$SaleDay)
resultsMissStore$store_nbr <- as.factor(resultsMissStore$store_nbr)
resultsMissStore$cluster <- as.factor(resultsMissStore$cluster)
resultsMissStore$SaleMonth <- as.factor(resultsMissStore$SaleMonth)
resultsMissStore$SaleWday <- as.factor(resultsMissStore$SaleWday)
resultsMissStore$dcoilwtico <- as.numeric(as.character(resultsMissStore$dcoilwtico))

results <- bind_rows(results, resultsMissStore)
results_bkp_1 <- results
View(results_bkp_1[results_bkp_1$item_nbr==2003392 & results_bkp_1$store_nbr==1,])
View(results_bkp_1[results_bkp_1$item_nbr==2056876 & results_bkp_1$store_nbr==39,])
View(results_bkp_1[results_bkp_1$item_nbr==1149069 & results_bkp_1$store_nbr==28,])

### Impute missing items
# load impute items
imputeItemsList <- read.csv2("resultImputeMissItems.csv", header = TRUE, sep = ',')
imputeItemsAllStores <- unique(imputeItemsList$replaceByItem)
imputeTargetItemsAllStores <- unique(imputeItemsList$missingTrainItems)
resultsPart <- results %>% filter(item_nbr %in% imputeItemsAllStores)

it=0
resultsMissWholeItem <- NULL
for (itm in imputeTargetItemsAllStores){
  #itm = 313094
  it<-it+1
  print(paste0("starting iteration " ,it," for item ",itm))
  
  temp <- imputeItemsList %>% filter(missingTrainItems==itm)
  replBy <- unique(temp$replaceByItem)
  imputNewStoreforItem <- resultsPart %>% filter(item_nbr == replBy)
  imputNewStoreforItem$item_nbr <- itm
  resultsMissWholeItem <- bind_rows(resultsMissWholeItem, imputNewStoreforItem)
}
resultsMissWholeItem$item_nbr <- as.factor(resultsMissWholeItem$item_nbr)
results <- bind_rows(results, resultsMissWholeItem)
results_bkp_2 <- results

#Create test type
results <- results %>% mutate(date = (paste0(SaleYear,'-',SaleMonth,'-',SaleDay)))
results$date <- as.Date(results$date)
resultsPart <- results[,c('item_nbr','store_nbr','date','tunitsales')]

resultsPart$item_nbr <- as.factor(resultsPart$item_nbr)
final <- inner_join(test, resultsPart)

submit <- final[,c(1,6)]
names(submit) <- c('id','unit_sales')
#submit$unit_sales <- ifelse(submit$unit_sales==0,1,submit$unit_sales)
submit$unit_sales <- ifelse(is.na(submit$unit_sales),0,submit$unit_sales)



write.table(submit, file = 'submit_roy_1.csv', quote = FALSE, row.names=FALSE, sep=",")
