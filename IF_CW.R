library(quantmod)
library(Quandl)
library(tidyverse)
library(rio)
library(xts)
library(lubridate)
library(forecast)
library(tseries)
library(urca)
library(TSstudio)
library(mgcv)
#######################DATA PREPARATION

#predictive modeling not expplanatory modeling
#do the model perform well on new data?
#test

#U.S. Energy Information Administration Data
WTI<-Quandl("EIA/PET_RWTC_D", api_key="ez5EYyD61vsAXsZ4okTD", start_date="1984-12-14", end_date="2020-10-30", type = "xts")
BRENT<-Quandl("EIA/PET_RBRTE_D", api_key="ez5EYyD61vsAXsZ4okTD", start_date="1984-12-14", end_date="2020-10-30", type = "xts")
#London Bullion Market Association
gold<-Quandl("LBMA/GOLD", api_key="ez5EYyD61vsAXsZ4okTD", start_date="1984-12-14", end_date="2020-10-30", type = "xts")

#Copper data
copper <- import("C:\\Users\\Sarvar\\Documents\\R\\copper-prices-historical-chart-data.csv")
copper1 <- copper[,2]
dates <- as.Date(copper[,1])
copper1 <- xts(copper1, order.by = dates)
gold <- gold[,1]
view(copper1)
length(WTI)
length(BRENT)
length(USDCAD)
length(copper)
length(gold1)
length(KRONERUSD)
length(RANDUSD)
length(USDAUD)

#Federal Reserve database
USDCAD<-Quandl("FED/RXI_N_B_CA", api_key="ez5EYyD61vsAXsZ4okTD", start_date="1984-12-14", end_date="2020-10-30", type = "xts")
USDAUD<-Quandl("FED/RXI_US_N_B_AL", api_key="ez5EYyD61vsAXsZ4okTD", start_date="1984-12-14", end_date="2020-10-30", type = "xts")
RANDUSD<-Quandl("FED/RXI_N_B_SF", api_key="ez5EYyD61vsAXsZ4okTD", start_date="1984-12-14", end_date="2020-10-30", type = "xts")
KRONERUSD<-Quandl("FED/RXI_N_B_NO", api_key="ez5EYyD61vsAXsZ4okTD", start_date="1984-12-14", end_date="2020-10-30", type = "xts")
summary(USDCAD)

mydata <- na.omit(merge(WTI, BRENT, USDCAD, copper1, gold, KRONERUSD, RANDUSD, USDAUD))
colnames(mydata) = c("WTI", "BRENT", "CADUSD", "Copper","Gold", "NOKUSD", "ZARUSD", "AUDUSD")
mydata$CADUSD<-1/mydata$CADUSD
mydata$NOKUSD<-1/mydata$NOKUSD
mydata$ZARUSD<-1/mydata$ZARUSD
write.csv(mydata, file="mydata.csv")


#######
mydata_monthly<-to.monthly(mydata)
mydata_monthly <- to.monthly(mydata, OHLC = F, drop.time = F)
mydata_quarterly <- to.period(mydata_monthly, k=2)


#differencing the data
diffdata <- diff(mydata)
diffdata = diffdata[-1,]
length(mydata$WTI)
length(diffdata$WTI)

#autocorrrelation function
ggAcf(diffdata$WTI) + ggtitle("ACF of WTI prices (Differenced)")
ggPacf(diffdata$WTI) + ggtitle("PACF of WTI prices (Differenced)")


#----- In Sample Forecasting and Validation
#Partition the data into test data and training data
tsdata<- as.ts(mydata)
split_data <- ts_split(tsdata, sample.out = 2000)
training <- split_data$train
testing <- split_data$test
length(training)
length(testing)



plot.zoo(mydata, main="WTI", col="blue") + xlab("abc")
length(diffdata)
#fitting model to a train set of the data
model <- gam(CADUSD ~ s(WTI, k = 20), data=diffdata[1:60, method = "REML" ] )
predicted <- predict(model, data=diffdata[6001:8147,], type = "response" )
actual <- mydata[5001:8147, "CADUSD"]
#out of sample RMSE
sqrt(mean((predicted - actual)^2))
tail(diffdata)

#Fit a model to the full dataset
model2 <- gam(CADUSD ~ WTI + BRENT, diffdata)
#predic in sample
predicted2 <- predict(model, diffdata, type = "response")
#Evaluate error
actual2 <- mydata[, "CADUSD"]
#in sample RMSE
sqrt(mean((predicted2 - actual2)^2))

view(predicted)

autoplot(inf) + ggtitle("Inflation Rate (Philippines), January 2000 to April 2020") + labs(x = "Time", y = "Inflation
Rate")


