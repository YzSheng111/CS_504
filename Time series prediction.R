library(dplyr)
library(tidyr)
library(ggplot2)
library(zoo)
library(forecast)
library(tseries)
library(knitr)
library(rworldmap)

# load and subset the dataset
df1 <- read.csv("~/Desktop/CS 504/Project/Project2/project/Main.Data.csv")
df1 <- subset(df1, select = c(year,State))

# create time series
df <- count(df1, year)
df <- filter(df, year >=1970)
df <- zoo(df$n, order.by = df$year)

# Predictions 2014-2015 and Evaluation metrics
AR <- accuracy(f = predict(ar(df[1:20]), n.ahead = 2)$pred, x = df[21:22])
MA <- accuracy(f = predict(ma(df[1:20], order = 2), h = 2)$mean, x = df[21:22])
ARIMA <- accuracy(f = predict(auto.arima(df[1:20]), 
                              n.ahead = 2)$pred, x = df[21:22])
ETS <- accuracy(f = predict(ets(df[1:20])$fitted, h = 2)$mean, 
                x = df[21:22])
metrics  <- rbind(as.data.frame(AR, row.names = "AR"),
                  as.data.frame(MA, row.names = "MA"),
                  as.data.frame(ARIMA, row.names = "ARIMA"),
                  as.data.frame(ETS, row.names = "ETS"))
kable(metrics)

###
index <- c("AR" = min(abs(AR)), 
           "MA" = min(abs(MA)), 
           "ARIMA" = min(abs(ARIMA)), 
           "ETS" = min(abs(ETS)))
index <- names(index[index == min(index)])
View(index)

# Fitted and Predicted values
View(AR)
AR <- ar(df)$resid + as.vector(df)
MA <- ma(df, order = 2)
ARIMA <- auto.arima(df, stationary = F, seasonal = F)$resid + as.vector(df)
ETS <- ets(df)$fitted
pred_vls <- data.frame("AR" = c(AR, predict(ar(df), n.ahead = 2)$pred),
                       "MA" = c(MA, predict(ma(df, order = 2), h = 2)$mean),
                       "ARIMA" = c(ARIMA, predict(auto.arima(df, stationary = F, 
                                                             seasonal = F), 
                                                  n.ahead = 2)$pred),
                       "ETS" = c(ETS, 
                                 predict(ets(df)$fitted, h = 2)$mean),
                       "Real data" = c(as.vector(df), NA, NA),
                       "Year" = c(1994:2017))

# Predicted future terrorist activity
pred_vls <- gather(pred_vls, method, value, - Year)
pred_vls <- filter(pred_vls, method == index | method == "Real.data")
pred_vls$method <- as.factor(pred_vls$method)
levels(pred_vls$method) <- c("ARIMA", "Real data")