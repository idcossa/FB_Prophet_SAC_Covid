getwd()
setwd("C:\\Users\\Doctor Shandu\\OneDrive - University of Witwatersrand\\MODULES\\GEOG 7029A_Advanced_App_GIS\\Assignment\\Mini_Project\\Data\\Github\\South_Africa\\Data")

  #Libraries
library(rcompanion) #Histogram
library("xts") #Converting data frame to time series wit specific date
library(zoo)
library(rlang) #various reasons like: rlang::last_error()
library(ggplot2)
library(dplyr)
library(hrbrthemes) #Themegplot requires ->  extrafontdb, Rttf2pt1
library(Hmisc) #Statistic Summary
library(vtable)

  #Loading Data
sa_confirmedcases <- read.csv("./Dataset/D.SA_Confirmed_Cases.csv")
sa_confirmeddeath <- read.csv("./Dataset/D.SA_Confirmed_Death.csv")
sa_confirmedrecoveries <- read.csv("./Dataset/D.SA_Confirmed_Recoveries.csv")


  #Data Viewing and Query
View(sa_confirmedcases) 
class(sa_confirmedcases)

  #Assigning date to Date Format
sa_confirmedcases$Date <- as.Date(sa_confirmedcases$Date, format="%m/%d/%Y")
sa_confirmeddeath$Date <- as.Date(sa_confirmeddeath$Date, format="%m/%d/%Y")
sa_confirmedrecoveries$Date <- as.Date(sa_confirmedrecoveries$Date, format="%m/%d/%Y")

    #Declaring and Converting Data to time Series for all variables 
  #Daily Time Series
sa_confcases_daily_ts <- xts(sa_confirmedcases$Daily, sa_confirmedcases$Date)
sa_confdeath_daily_ts <- xts(sa_confirmeddeath$Daily, sa_confirmeddeath$Date)
sa_confrecove_daily_ts <- xts(sa_confirmedrecoveries$Daily, sa_confirmedrecoveries$Date)

  #Cumulative Time Series
sa_confcases_cum_ts <- xts(sa_confirmedcases$Cumulative, sa_confirmedcases$Date)
sa_confdeath_cum_ts <- xts(sa_confirmeddeath$Cumulative, sa_confirmeddeath$Date)
sa_confrecove_cum_ts <- xts(sa_confirmedrecoveries$Cumulative, sa_confirmedrecoveries$Date)

###################################################################################

    #EXPLORATORY DATA ANALYSIS 

  #Descriptive Statistics Summary
summary(sa_confirmedcases) # mean,median,25th and 75th quartiles,min,max
summary(sa_confirmeddeath)
summary(sa_confirmedrecoveries)

describe(sa_confirmedcases) #using Hmisc library

st(sa_confirmedcases) #Using vtable library (st = sumtable)
st(sa_confirmeddeath)
st(sa_confirmedrecoveries)

  #Ploting Area
plot.new()
frame()
par(mfcol=c(2,3))

    #Ploting Histogram
  #Histogram for Daily 
plotNormalHistogram(sa_confirmedcases$Daily, breaks = 10, main = "Daily Confirmed Cases",
                    labels=FALSE,col="green", linecol="red",xlab="", prob = FALSE)
plotNormalHistogram(sa_confirmedrecoveries$Daily, breaks = 10, main = "Daily Confirmed Recoveries",
                    labels=FALSE,col="green", linecol="red",xlab="", prob = FALSE)
plotNormalHistogram(sa_confirmeddeath$Daily, breaks = 10, main = "Daily Confirmed Death",
                    labels=FALSE,col="green", linecol="red",xlab="", prob = FALSE)
  
  #Histogram for Cumulative
plotNormalHistogram(sa_confirmedcases$Cumulative, breaks = 10, main = "Cumulative Confirmed Cases",
                    labels=FALSE,col="green", linecol="red",xlab="", prob = FALSE)
plotNormalHistogram(sa_confirmedrecoveries$Cumulative, breaks = 10, main = "Cumulative Confirmed Recoveries",
                    labels=FALSE,col="green", linecol="red",xlab="", prob = FALSE)
plotNormalHistogram(sa_confirmeddeath$Cumulative, breaks = 10, main = "Cumulative Confirmed Death",
                    labels=FALSE,col="green", linecol="red",xlab="", prob = FALSE)


    #Ploting QQ Plot
  #QQ Plot for Daily
qqnorm(sa_confirmedcases$Daily, main = "Daily Confirmed Cases")
  qqline(sa_confirmedcases$Daily, col = "red", lwd = 2)
qqnorm(sa_confirmedrecoveries$Daily, main = "Daily Confirmed Recoveries")
  qqline(sa_confirmedrecoveries$Daily, col = "red", lwd = 2)
qqnorm(sa_confirmeddeath$Daily, main = "Daily Confirmed Death")
  qqline(sa_confirmeddeath$Daily, col = "red", lwd = 2)
  
  #QQ for Cumulative
qqnorm(sa_confirmedcases$Cumulative, main = "Cumulative Confirmed Cases")
  qqline(sa_confirmedcases$Cumulative, col = "red", lwd = 2)
qqnorm(sa_confirmedrecoveries$Cumulative, main = "Cumulative Confirmed Recoveries")
  qqline(sa_confirmedrecoveries$Cumulative, col = "red", lwd = 2)
qqnorm(sa_confirmeddeath$Cumulative, main = "Cumulative Confirmed Death")
  qqline(sa_confirmeddeath$Cumulative, col = "red", lwd = 2)
  
  
    #Plots time series separate
  #TS for Daily
plot(sa_confcases_daily_ts, ylab=" ", xlab="", main="Daily Cases", 
     col="steelblue",lwd=2) 
plot(sa_confdeath_daily_ts, ylab="", xlab="", main="Daily Deaths", 
     col="steelblue",lwd=2) 
plot(sa_confrecove_daily_ts, ylab="", xlab="", main="Daily Recoveries", 
     col="steelblue",lwd=2)

  #TS for Cumulative
plot(sa_confcases_cum_ts, ylab="", xlab="Date", main="Cumulative Cases", 
     col="steelblue",lwd=2) 
plot(sa_confdeath_cum_ts, ylab="", xlab="Date", main="Cumulative Deaths", 
     col="steelblue",lwd=2) 
plot(sa_confrecove_cum_ts, ylab="", xlab="Date", main="Cumulative Recoveries", 
     col="steelblue",lwd=2)

  #Ploting Area
plot.new()
frame()
par(mfcol=c(2,3))

    #Line ploting Combining
  #Daily Line Plot
plot(sa_confirmedcases$Date, sa_confirmedcases$Daily, type="l", col="blue", xlab="", ylab=" Daily Covid 19",
     main="Daily Cases", lty=1)#type="o",pch="o",
plot(sa_confirmedrecoveries$Date,  sa_confirmedrecoveries$Daily, type="l", col="blue", xlab="", ylab="",
     main="Daily Recoveries", lty=1)
plot(sa_confirmeddeath$Date, sa_confirmeddeath$Daily, type="l", col="blue", xlab="", ylab="",
     main="Daily Death", lty=1)

  #Cumulative Line Plot
plot(sa_confirmedcases$Date, sa_confirmedcases$Cumulative, type="l", col="blue", xlab="Date", ylab="Daily Covid 19",
     main="Cumulative Cases", lty=1)#type="o",pch="o",
plot(sa_confirmedrecoveries$Date,  sa_confirmedrecoveries$Cumulative, type="l", col="blue", xlab="Date", ylab="",
     main="Cumulative Recoveries", lty=1)
plot(sa_confirmeddeath$Date, sa_confirmeddeath$Cumulative, type="l", col="blue", xlab="Date", ylab="",
     main="Cumulative Death", lty=1)

#lines(sa_confirmedrecoveries$Date, sa_confirmedrecoveries$Daily , type="l", col="red",lty=1) #Can be used to combine line plot
#title("Covid-19 Variability", line = -1)


#################################################################################

    #POINT PATTERN ANALYSIS FOR SPATIAL SCIENCE

library(rspatial)
library(sf) #Read shapefile

sa_provinces <- st_read("./Shapefiles/Provinces.shp")
sa_district <- st_read("./Shapefiles/District.shp")
commHealth <- read.csv("./Dataset/C.Health_Hospitals.csv")

  #Assigining Lat and Long to community health
coordinates(commHealth) = c("Longitude", "Latitude")
class(commHealth)

  #Setting out the plot areas many maps in one sheet
plot.new()
frame()
par(mfcol=c(1,1))

  #plot provinces,
plot(st_geometry(sa_provinces), border="#aaaaaa", main="South Africa Hospital Facilities")
#plot(st_geometry(kzn_localm), add=T, col="transparent")
points(commHealth, pch = 20, col="blue", lwd = 0.4, cex = 1)

  #Basic Statistics #https://rspatial.org/raster/analysis/8-pointpat.html
  # mean centre
xy_Clinic <- coordinates(commHealth)
mc <- apply(xy_Clinic, 2, mean)
mc # 27.28047 -28.56827 location

  # standard distance
sd <- sqrt(sum((xy_Clinic[,1] - mc[1])^2 + (xy_Clinic[,2] - mc[2])^2) / nrow(xy_Clinic))
sd #standard d 4.757119

  #adding basic stats to district and clinit plot
points(cbind(mc[1], mc[2]), pch='*', col='red', cex=5)

  # make a circle
bearing <- 1:360 * pi/180
cx <- mc[1] + sd * cos(bearing)
cy <- mc[2] + sd * sin(bearing)
circle <- cbind(cx, cy)
lines(circle, col='red', lwd=2)


#################################################################################
      #OTHER FORECAST  - THIS STUDY USES PROPHET

library(forecast)
library(tseries) #used by time series and adf test
library(fpp2)

    #Basic Univariate Forecasting: For Daily Cases
  #Test
  #Setting out the plot areas many maps in one sheet
plot.new()
frame()
par(mfcol=c(2,2))

  #Stationarity
plot(sa_dailycases_ts, ylab="Number of Covid-19 Cases", xlab="Date", main="Covid-19 Daily Cases Variability", 
     col="steelblue",lwd=2)
acf(sa_dailycases_ts, main="Daily Cases: ACF" )
pacf(sa_dailycases_ts, main="Daily Cases: PACF")
adf.test(sa_dailycases_ts) #Augumented Dickey Fuller, p-values and hypothesis -> p-value = 0.030

  #White Noise
set.seed(30)
whitenoise = ts(rnorm(862, mean = 1)) #ts from plot
plot(whitenoise, main="Whitenoise")
abline(h=1, col="red")

lag.plot(sa_dailycases_ts, lags = 9, do.line = FALSE)
lag.plot(whitenoise, lags = 9, do.line = FALSE)

    #Stationarity Above else need blow
  #Taking the first Diference to remove trend 
diferencing_dailycases <- diff(sa_dailycases_ts)

`#Analyse result of differencing -> Plot Time differenced 
autoplot(diferencing_dailycases) + 
  ggtitle("Daily Cases Variability Diffeneced") +
  ylab("Number of Covid-19 Cases") +
  xlab("Date")

  #Setting out the plot areas many maps in one sheet
plot.new()
frame()
par(mfcol=c(2,2))

    # Forecasting and Training Models and selection
  # Seasonal Naive Method as Benchmark ~ Using Differenced Data
  # y_t = y_{t-s} + e_t
fit <- snaive(diferencing_dailycases) 
print(summary(fit)) #Tell residual sd= small umber close to zero are better, RMSE, ME, MAE..
checkresiduals(fit) #ACF, All car should be inside 95% ci

    # ETS Exponential Smoothing Model Method as Benchmark ~ Using data before differencing
  #Some can allow for trend, we can use data before difference
fit_ets <- ets(diferencing_dailycases) #try several method and pick the best ~ Sigma = Residual
print(summary(fit_ets)) 
checkresiduals(fit_ets)

    # ARIMA Method as Benchmark ~ data need to stationarily or apply here d diffrencing 
fit_arima <- auto.arima(diferencing_dailycases, d=1, D=1, stepwise = FALSE, 
                        approximation = FALSE, trace = TRUE) #before fitting take diffence of 1, D= seasonal Differnce, stepwise = FALSE by defauslt try to make it best try all methods and approximation 
print(summary(fit_arima))  #Residual sd = 1.145
checkresiduals(fit_arima) #Resut in Square sigma and can take it square root for standard deviation

    #Forecast the data: must have training and testing dataset
  #ARIMA
fcst_arima <- forecast(fit_arima, h=100) #Fit ARIMA h=days
autoplot(fcst_arima, main ="Covid 19 : ARIMA 0,1,4 Model Forecast", 
         ylab="Number of Covid-19 Cases", xlab="Date")
autoplot(fcst_arima, include = 60, main ="Covid 19 : ARIMA 0,1,4  Model Forecast", 
         ylab="Number of Covid-19 Cases", xlab="Date ")
print(summary(fcst_arima))

  #ETS
fcst_ets <- forecast(fit_ets, h=48) #Fit Exponential ETS  ~ h =486 months
autoplot(fcst_ets, main="Covid" )
autoplot(fcst_ets, include = 60,main ="Precipitation: ETS Model Forecast",
         ylab="Precipitation (mm/day)", xlab="Date (Months)") #Including the last 60 months 
print(summary(fcst_ets))


##################################################################################

      # FORECASTING USING FACEBOOK PROPHET MODEL AND TIME SERIES PLOT 
  
  #Forecasting Using Cumulative Cases
#library(covid19.analytics)
library(dplyr)
library(prophet)
library(lubridate)
library(ggplot2)

  #Loading Data
sa_cases <- read.csv("./Dataset/D.SA_Confirmed_Cases.csv")
sa_cases

  #Assigning date to Date Format
sa_cases$Date <- as.Date(sa_cases$Date, format="%m/%d/%Y")
sa_cases

#Setting out the plot areas many maps in one sheet
plot.new()
frame()
par(mfcol=c(2,1))

  #Line ploting Cumulative and Daily cases
plot(sa_cases$Date, sa_cases$Cumulative, type="l", col="blue", xlab="Date", ylab="Covid 19",
     main="Cumulative Cases", lty=1,lwd=2)#type="o",pch="o",
lines(sa_cases$Date, sa_cases$Daily , type="l", col="red",lty=1)

  #Separate plot
plot(sa_cases$Date, sa_cases$Daily, type="l", col="blue", xlab="Date", ylab="Covid 19",
     main="Daily Cases", lty=1, lwd=2)
plot(sa_cases$Date, sa_cases$Cumulative, type="l", col="blue", xlab="Date", ylab="Covid 19",
     main="", lty=1)

  #Or Simple Line
qplot(Date, Cumulative, data =sa_cases)
qplot(Date, Daily, data =sa_cases)

  #Declaring and Converting Data to time Series for all variables 
sa_cumucases_ts <- xts(sa_cases$Cumulative, sa_cases$Date)
sa_dailycases_ts <- xts(sa_cases$Daily, sa_cases$Date)

plot(sa_cumucases_ts, ylab="Covid-19", xlab="", main="Covid-19 Cumulative Confirmed Cases", 
     col="steelblue",lwd=2) 
plot(sa_dailycases_ts, ylab="Covid-19", xlab="Date", main="Covid-19 Daily Confirmed Cases", 
     col="steelblue",lwd=2) 


#----------------------------------------------------------------------------------
 #Defining variable 
rm(y)

ds <- sa_cases$Date
y <- sa_cases$Cumulative
dfc <- data.frame(ds, y)

  #Forecasting for Cumulative cases
?prophet
mc <- prophet(dfc)

  #Prediction
future_c <- make_future_dataframe(mc, periods = 365) #h = lag = 365 days
tail(future_c)
forecast_c <- predict(mc, future_c)

  #Generic Plot forecast
plot(mc, forecast_c)
prophet_plot_components(mc, forecast_c) #to view trends, weekly and seasonality and yearly
forecast_c

  #interactive plot deploying
dyplot.prophet(mc, forecast_c, main="South African Cumulative Covid-19 Variability Forecast")

  #Model performance
  #Cross Validation
df.cv_c<- cross_validation(mc, initial = 600, period = 100, horizon = 50, units = 'days') # intial data 516=60% of data, period of 180 days between cutoff
head(df.cv_c)
  #Perfomance
df.p_c <- performance_metrics(df.cv_c)
head(df.p_c)
df.p_c
  #using plot to visualise cross validation 
plot_cross_validation_metric(df.cv_c, metric = 'mape')

#----------------------------------------------------------------------------------
    #Defining variable for Daily forecast
 #removing previous variables for Cumulative forecasting
rm(y)

  #defining new y using previous ds
ds <- sa_cases$Date
y <- sa_cases$Daily
dfd <- data.frame(ds, y)
  
  #Forecasting for daily cases
?prophet
md <- prophet(dfd)

  #Prediction
future_d <- make_future_dataframe(md, periods = 365) #h = lag = 365 days
tail(future_d)
forecast_d <- predict(md, future_d)

  #Generic Plot forecast
plot(md, forecast_d)
title("South African Daily Covid-19 Variability Forecast",adj = 0.75,line = 0.25)
prophet_plot_components(md,  forecast_d) #to view trends, weekly and seasonality and yearly
forecast_d

  #interactive plot deploying
dyplot.prophet(md, forecast_d, main="South African Daily Covid-19 Variability Forecast")

    #Model performance
  #Cross Validation
df.cv_d <- cross_validation(md, initial = 600, period = 100, horizon = 50, units = 'days') # intial data 516=60% of data, period of 180 days between cutoff
head(df.cv_d)

  #Perfomance
df.p_d <- performance_metrics(df.cv_d)
head(df.p_d)
df.p_d
  
  #using plot to visualise cross validation 
plot_cross_validation_metric(df.cv_d, metric = 'mape')

#--------------------------------------------------------------------------------

###########################################################################################

#PROPHET MODEL REASOURCE
  #https://facebook.github.io/prophet/docs/quick_start.html    >>>>>>>>>>>>>>>>>.>>More about Prophet Model
  #https://nextjournal.com/fb-prophet/facebook-prophet-diagnostics  >>>>>>>>>>>>>>> More about Cross Validation
    # forecat object is dataframe with "yhat" and additional column for uncertainty interval and seasonal components
    #Allows to view trends, weekly and seasonality and yearly
    #interactive plot of the foreact
      #forecast horizon is the length of time into the future for which forecasts are to be prepared
      #Forecast period is a defined period of time in the future that is taken for the preparation of an estimate of what is thought likely to happen during that period.
  #https://facebook.github.io/prophet/docs/quick_start.html

#OTHER
  #https://github.com/0xpranjal/Stock-Prediction-using-different-models
  #https://rspatial.org/raster/analysis/8-pointpat.html
  #https://spatstat.org/SSAI2017/slides/slides.pdf               >>>>>>>>>>>>>>>>>>>>Document
  #https://www.pluralsight.com/guides/time-series-forecasting-using-r

#SPATIAL PLOT
  #https://rpubs.com/wave1art/Geospatial1


#DATA SOURCE
  #https://github.com/dsfsi/covid19za/tree/master/data

