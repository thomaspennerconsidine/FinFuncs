
library(ggplot2)
library(xts)
library(zoo)
library(tseries)
library(forecast)
library(quantmod)
library(PerformanceAnalytics)
library(caret)
library(rugarch)
FREQ <- 100 ## frequency	===  the number of observations per unit of time.

LEN_MASTER <- 2000
LEN_TRAIN <- 1000

TCKR <-  data.frame(c(1:LEN_MASTER))
colnames(TCKR) = c("N")
TCKR$close <- (sin(TCKR$N/20)+10)+rnorm(LEN_MASTER, 0, .2)
#plot(TCKR$close, type = "l")



#___________________________________________

# Create Training and Test Data
TCKRTrain <- TCKR[1:LEN_TRAIN,]
TCKRTest <- TCKR[(LEN_TRAIN+1):LEN_MASTER,]

# Time Series Processing
start_date_train <- as.Date(0)
start_num_train <- as.numeric(noquote(strsplit(as.character(start_date_train) , "-")[[1]]))

end_date_train <- as.Date(LEN_TRAIN)
end_num_train <- as.numeric(noquote(strsplit(as.character(end_date_train) , "-")[[1]]))


TCKRTrainTS <- ts(TCKRTrain$close, start=c(start_num_train[1], start_num_train[2], start_num_train[3]),
                  end=c(end_num_train[1], end_num_train[2], end_num_train[3]), frequency=FREQ)
#___________________________________________



#__________________________________________________________________________
#Forecasting Model #1 - Holt Winters Filtering
#__________________________________________________________________________
FORCAST_FORWARD <- 5
# Fit Exponential Smoothing
fit_ES <- HoltWinters(TCKRTrainTS)
# Forecast 20 Days
TCKRForecast <- forecast(fit_ES, h=FORCAST_FORWARD)
# Accuracy of Exponential Smoothing Predictive Model
#accuracy(TCKRForecast, TCKRTestTS)

#__________________________________________________________________________
#base for compariason (num 1)
#__________________________________________________________________________
MOD <- 50
TCKR_Base <- TCKR[0:(LEN_TRAIN+50),]
start_date_base <-as.Date(0)
start_num_base <- as.numeric(noquote(strsplit(as.character(start_date_base) , "-")[[1]]))

#end_date_base <- as.Date(1080)
end_date_base <- as.Date(LEN_TRAIN+(LEN_TRAIN*(FORCAST_FORWARD/FREQ)))
end_num_base <- as.numeric(noquote(strsplit(as.character(end_date_base) , "-")[[1]]))

TCKR_TS_TRAIN_BASE <- ts(TCKR_Base$close, start=c(start_num_base[1], start_num_base[2], start_num_base[3]),
                         end=c(end_num_base[1], end_num_base[2], end_num_base[3]), frequency=FREQ)

#__________________________________________________________________________


# Exponential Smoothing Plot
plot(TCKRForecast, ylim = c(8.5,11.5))

# Realized Data Plot
plot(TCKR_TS_TRAIN_BASE[1:(length(TCKR_TS_TRAIN_BASE)+100 )], ylim = c(8.5,11.5), type = "l")


length(TCKRForecast)
