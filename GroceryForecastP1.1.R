memory.limit(50000)
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

#******************************** all items run **************************

load('GFtemp3.RData')

#Define vars
test$SalaryImpact <- ifelse(test$SaleDay %in% c(14,15,16,17,28,29,30,31,1,2), 'Yes', 'No')
test$SalaryImpact <- as.factor(test$SalaryImpact)

dateExclusion <- c('SaleYear', 'SaleDay')
catvars <- c('store_nbr','onpromotion','type','cluster','SaleMonth', 'SaleWday','SalaryImpact')
numvars <- c('dcoilwtico')#,'tlag3UnitSales','tlag2UnitSales','tlag1UnitSales')
tarvar <- 'tunitsales'

it = 1
tt <- test[1, c('item_nbr', dateExclusion, catvars, 'dcoilwtico')]; tt$tunitsales <- NA
accumulatedResult <- tt[0,]

#####For multiprocessing
filename <- 'resultsP1.1.csv'
allItemsPart <- allItems[1:500]
trainPartPart <- trainPart %>% filter(item_nbr %in% allItemsPart)
testPartPart <- test %>% filter(item_nbr %in% allItemsPart)
#rm(train);rm(trainPart);rm(test)
#####End multiprocessing

for (itms in allItemsPart){
  #itms = 1084882
  start_time <- Sys.time()
  print(paste0("starting iteration " ,it," for item ",itms))

  #out of sample data is detected in test store numbers. Validate before proceeding.
  chkStorestrain <- trainPartPart %>% filter(item_nbr == itms)
  chkStorestest <- testPartPart %>% filter(item_nbr == itms)
  unqStorestrain <- sort(as.integer(unique(chkStorestrain$store_nbr)))
  unqStorestest <- sort(as.integer(unique(chkStorestest$store_nbr))) 
  outSampleStore <- setdiff(unqStorestest, unqStorestrain)
  
  # if (length(outSampleStore) > 0){
  #   print(paste0("skipping item ",itms," Out of sample record detected................................"))
  #   it <- it+1
  #   next}
  it <- it+1
  
  df <- trainPartPart %>% filter(item_nbr == itms)
  df$unit_sales <- round(abs(df$unit_sales))
  df$unit_sales <- ifelse(df$unit_sales == 0, 1, df$unit_sales)
  
  allstores <- as.integer(unique(df$store_nbr))
  allstoresTrans <- list()
  for (i in allstores){
    temp <- tsclean(df[df$store_nbr==i, 'unit_sales'])
    allstoresTrans <- unlist(append(allstoresTrans, temp))
  }
  tunitsales <- as.integer(round(allstoresTrans))
  
  #tryRegression
  df <- left_join(df, oil) #%>% left_join(., transactions)
  df <- cbind(df, tunitsales)
  # Split date
  df <- df %>% mutate(SaleMonth = month(ymd(date)))
  df <- df %>% mutate(SaleYear = year(ymd(date)))
  df <- df %>% mutate(SaleWday = wday(ymd(date)))
  df <- df %>% mutate(SaleDay = day(ymd(date)))
  df$SaleYear <- as.factor(df$SaleYear);df$SaleDay <- as.factor(df$SaleDay);
  df$SaleWday <- as.factor(df$SaleWday);df$SaleMonth <- as.factor(df$SaleMonth)
  df$SalaryImpact <- ifelse(df$SaleDay %in% c(14,15,16,17,28,29,30,31,1,2), 'Yes', 'No')
  df$SalaryImpact <- as.factor(df$SalaryImpact)
  excludeList <- c('id', 'city', 'state', 'family', 'class', 'perishable', 
                   'item_nbr', 'date', 'unit_sales')
  df <- df[,!names(df) %in% excludeList]
  
  dfTest <- testPartPart %>% filter(item_nbr == itms)
  if(length(outSampleStore)>0){
    print(paste0("Filtering out of sample stores for item ",itms,".............."))
    dfTest <- dfTest %>% filter(!store_nbr %in% outSampleStore)
  }
  dfTest <- dfTest[,!names(dfTest) %in% excludeList]
  
  #Oil prices
  df$rowID <- row.names(df)
  for (i in 2:nrow(df)){
    if (is.na(df[i,'dcoilwtico'])){
      df[i,'dcoilwtico'] <- df[df$rowID == i-1,'dcoilwtico']
    }
  }
  df <- subset(df, select = -c(ncol(df)))
  
  
  #if date is 2017-08-19 and 2017-08-20 impute 48.59
  #if date is 2017-08-26 and 2017-08-27 impute 47.65
  dfTest[dfTest$SaleDay %in% c(19, 20),'dcoilwtico'] <- 48.59
  dfTest[dfTest$SaleDay %in% c(26, 27),'dcoilwtico'] <- 47.65
  
  dfTest$rowID <- row.names(dfTest)
  # for (i in 2:nrow(dfTest)){
  #   if (is.na(dfTest[i,'dcoilwtico'])){
  #     dfTest[i,'dcoilwtico'] <- dfTest[dfTest$rowID == i-1,'dcoilwtico']
  #   }
  # }
  dfTest <- subset(dfTest, select = -c(ncol(dfTest)))
  
  #Define vars
  dateExclusion <- c('SaleYear', 'SaleDay')
  catvars <- c('store_nbr','onpromotion','type','cluster','SaleMonth', 'SaleWday','SalaryImpact')
  numvars <- c('dcoilwtico')#,'tlag3UnitSales','tlag2UnitSales','tlag1UnitSales')
  tarvar <- 'tunitsales'
  
  #arrage vars
  df <- df[,c(dateExclusion, catvars, numvars, tarvar)]
  dfTest <- dfTest[,c(dateExclusion, catvars, numvars)]
  dfTest$tunitsales <- NA
  
  #Combine to create dummy
  dfTemp <- rbind(df, dfTest)
  
  #dummy vars
  dfTemp <- cbind(dfTemp, dummy(dfTemp$store_nbr), dummy(dfTemp$onpromotion), dummy(dfTemp$type), dummy(dfTemp$cluster),
              dummy(dfTemp$SaleMonth), dummy(dfTemp$SaleWday), dummy(dfTemp$SalaryImpact))
  
  names(dfTemp) <- make.names(names(dfTemp), unique = TRUE)
  dummyvars <- names(dfTemp[!names(dfTemp) %in% c(dateExclusion,catvars,numvars,tarvar)])
  #reorder columns
  dfTemp <- dfTemp[,c(dateExclusion, catvars, dummyvars, numvars, tarvar)]
  
  #Resplit into test train
  cutIndex <- as.integer(row.names(dfTemp[is.na(dfTemp$tunitsales),])[1])
  df <- dfTemp[1:(cutIndex-1),]
  dfTest <- dfTemp[cutIndex:nrow(dfTemp),]
  
  #Add lags
  #should we lag stores wise? if not then first 3 entries will mess up. but it's already messed up.
  tlag1UnitSales <- lag(df$tunitsales, 1, default = round(mean(df$tunitsales)))
  tlag2UnitSales <- lag(df$tunitsales, 2, default = round(mean(df$tunitsales)))
  tlag3UnitSales <- lag(df$tunitsales, 3, default = round(mean(df$tunitsales)))
  df <- cbind(df, tlag3UnitSales, tlag2UnitSales, tlag1UnitSales)
  
  #log scale values
  df$dcoilwtico <- log(df$dcoilwtico,10)
  #df$transactions <- log(df$transactions,10)
  df$tlag1UnitSales <- log(df$tlag1UnitSales,10)
  df$tlag2UnitSales <- log(df$tlag2UnitSales,10)
  df$tlag3UnitSales <- log(df$tlag3UnitSales,10)
  dfTest$dcoilwtico <- log(dfTest$dcoilwtico,10)
  
  #Define vars
  dateExclusion <- c('SaleYear', 'SaleDay')
  catvars <- c('store_nbr','onpromotion','type','cluster','SaleMonth', 'SaleWday','SalaryImpact')
  numvars <- c('dcoilwtico','tlag3UnitSales','tlag2UnitSales','tlag1UnitSales')
  tarvar <- 'tunitsales'
  #Rearrange
  df <- df[,c(dateExclusion, catvars, dummyvars, numvars, tarvar)]
  
  #Start regression
  # Fit linear model with all variables
  modelForm <- createModelFormula(yVar = tarvar, xVars = c(dummyvars, numvars), includeIntercept = FALSE)
  model.1 <- lm(modelForm, data = df)
  
  #Start prediction in loop by creating lag features
  dfTest <- subset(dfTest, select = -c(ncol(dfTest)))
  result <- NULL
  #Start loop
  for (rw in 1:nrow(dfTest)){
    dfTestRow <- dfTest[rw,]
    if(is.null(result)){
      newstore <- as.integer(dfTestRow[1,'store_nbr'])
      #print(paste0("working on store ", newstore))
      result <- df %>% filter(store_nbr == newstore)
      
      if(nrow(result) < 3){
        print(paste0("skipping store ",newstore," of item ",itms))
        result <- NULL
        next
      }
      result <- result[(nrow(result)-2) : nrow(result), ]
    }
    # create lag features
    dfTestRow$tlag3UnitSales <- log(result[nrow(result)-2, 'tunitsales'],10)
    dfTestRow$tlag2UnitSales <- log(result[nrow(result)-1, 'tunitsales'],10)
    dfTestRow$tlag1UnitSales <- log(result[nrow(result), 'tunitsales'],10)
    dfTestRow$tunitsales <- NA
    result <- rbind(result, dfTestRow)
    
    #Predict
    predict.1 <- predict(model.1, newdata = dfTestRow[,c(dummyvars, numvars)])
    result[nrow(result),'tunitsales'] <- round(abs(10**predict.1[[1]]))

    if(rw != nrow(dfTest)){
      dfTestNextRow <- dfTest[rw+1,]
      if (as.integer(dfTestNextRow$store_nbr) != as.integer(dfTestRow$store_nbr)){
        tp <- result[4:nrow(result), c(dateExclusion, catvars, 'dcoilwtico' , tarvar)]
        tpi <- cbind(itms, tp)
        accumulatedResult <- rbind(accumulatedResult, tpi)
        result <- NULL
      }} 
    if(rw == nrow(dfTest)){
      tp <- result[4:nrow(result), c(dateExclusion, catvars, 'dcoilwtico' , tarvar)]
      tpi <- cbind(itms, tp)
      accumulatedResult <- rbind(accumulatedResult, tpi)
      }
  }  
  end_time <- Sys.time()
  print(paste0("Time Elapsed: ",end_time - start_time))
}
row.names(accumulatedResult) <- NULL
names(accumulatedResult)[1] <- 'item_nbr'

write.table(accumulatedResult, file = filename, quote = FALSE, row.names=FALSE, sep=",")
