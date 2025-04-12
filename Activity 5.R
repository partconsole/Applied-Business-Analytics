#Ethan, Minh

#Activity 5
library(ggplot2)
library(ggfortify)
library(forecast)
library(readxl)
cars <- read_excel("Documents/EM 150/cars.xlsx")

#1 creating time series object
cars_rev<-ts(cars$Cars,start=c(2014,1),
              end=c(2019,12),frequency=12)

#2 Time Series Plot
autoplot(cars_rev,main="Cars Sold 2014-2019",
         ylab="Cars Sold")

#4 Fit linear trend model with seasonality
#a
cars_reg<-tslm(cars_rev~trend+season)
summary(cars_reg)
#d predict Jan and Feb of 2020
cars_forecast<-forecast(cars_reg,h=2)
cars_forecast


#6 95% CI
confint(cars_reg)
