library(fpp3)
#install.packages("TSA")
library(TSA)
#update.packages("forecast")
library(forecast)
library(lmtest)
#install.packages("fUnitRoots")
library(fUnitRoots)
library(tseries)
library(knitr)
#install.packages("dLagM")
library(dLagM)
library(lattice)
library(bestglm)
library(leaps)
#install.packages("ltsa")
library(ltsa)
install.packages("FitAR")
#library(FitAR)
#install.packages("CombMSC")
#library(CombMSC)
library(lmtest)
#install.packages("fGarch")
library(fGarch)
require("timeSeries")
library(zoo)
#install.packages("astsa")
library(astsa)
library(dplyr)

# Read the 'cars.csv' file
cars <- read.csv("cars.csv",header=TRUE)
head(cars)

names(cars)
summary(cars)
sum(is.na(cars))
# Converting the 'Sales' column to a time series object
cars.ts <- matrix(cars$Sales,nrow=108,ncol=1)
cars.ts<- as.vector(t(cars.ts))
cars.ts <- ts(cars.ts,start=c(1960,1), end=c(1968,12), frequency=12)

# Ploting the time series of car sales
plot(cars.ts,type='o',ylab='Thousand of unit of retail new car sales', xlab='Year')
title(main = 'Monthly cars sales from 1960-1968')

# Calculating and plotting the ACF (Autocorrelation Function)
acf(cars.ts,lag.max = 36, main = "ACF (Autocorrelation Function)")

# Calculating and plotting the PACF (Partial Autocorrelation Function)
pacf(cars.ts, lag.max = 36, main = "PACF (Partial Autocorrelation Function)")

# Fiting an ARIMA(0,0,0)(0,1,0)[12] model (non-seasonal difference = 1)
m1.cars = arima(cars.ts,order=c(0,0,0),seasonal=list(order=c(0,1,0), period=12))
res.m1 = residuals(m1.cars);  
par(mfrow=c(1,1))
plot(res.m1,xlab='Time',ylab='Residuals')
title(main='Time series plot of 1st seasonal difference')

# Plottinglot the ACF and PACF of the residuals
par(mfrow=c(1,2))
acf(res.m1, lag.max = 36,main = "ACF 1st seasonal difference")
pacf(res.m1, lag.max = 36,main = "PACF 1st seasonal difference")

# Fiting an ARIMA(0,0,0)(1,1,1)[12] model
m2.cars = arima(cars.ts,order=c(0,0,0),seasonal=list(order=c(1,1,1), period=12))
res.m2 = residuals(m2.cars);  
par(mfrow=c(1,1))
plot(res.m2,xlab='Time',ylab='Residuals',main="Time series plot of the residuals")

# Plotting the ACF and PACF of the residuals
par(mfrow=c(1,2))
acf(res.m2, lag.max = 36,main = "ACF Res M2")
pacf(res.m2, lag.max = 36,main = "PACF Res M2")

# Fiting an ARIMA(0,0,0)(1,1,2)[12] model
m3.cars = arima(cars.ts,order=c(0,0,0),seasonal=list(order=c(1,1,2), period=12))
res.m3 = residuals(m3.cars)
par(mfrow=c(1,2))
acf(res.m3, lag.max = 36,main = "ACF Res M3")
pacf(res.m3, lag.max = 36,main = "PACF Res M3")

# Log-transform of the time series
log.cars.ts = log(cars.ts)
par(mfrow=c(1,1))
plot(log.cars.ts,ylab='log of sales count',xlab='Year',type='o')
title(main='Time series plot with transformed data')

# Fiting an ARIMA(0,0,0)(1,1,2)[12] model to the log-transformed series
m4.cars = arima(log.cars.ts,order=c(0,0,0),seasonal=list(order=c(1,1,2), period=12))
res.m4 = residuals(m4.cars)
plot(res.m4,xlab='Time',ylab='Residuals',main="Time series plot of the residuals")

# Plotting the ACF of the residuals
acf(res.m4, lag.max = 36, main = "Sample ACF plot of the residuals after transformation")

# Plotting the PACF of the residuals
pacf(res.m4, lag.max = 36, main = "Sample PACF plot of the residuals after transformation")

# Fiting a SARIMA(0,1,0)(1,1,2)[12] model to the log-transformed series
m5.cars = arima(log.cars.ts,order=c(0,1,0),seasonal=list(order=c(1,1,2), period=12))
res.m5 = residuals(m5.cars) 
plot(res.m5,xlab='Time',ylab='Residuals',main="Time series plot of the residuals")
par(mfrow=c(1,2))
acf(res.m5, lag.max = 36,main = "ACF Res M5")
pacf(res.m5, lag.max = 36,main = "PACF Res M5")

# Performing Augmented Dickey-Fuller test on the residuals
adf.test(res.m5)

# Plotting the EACF (Extended Autocorrelation Function) of the residuals
eacf(res.m5)

# Fiting an ARIMA(0,1,3)(1,1,2)[12] model to the log-transformed series
model2.cars = arima(log.cars.ts,order=c(0,1,3),seasonal=list(order=c(1,1,2), period=12),method = "ML")
coeftest(model2.cars)

# Obtain the residuals of the model
res.model2 = residuals(model2.cars) 
par(mfrow=c(1,2))
acf(res.model2, lag.max = 36)
pacf(res.model2, lag.max = 36)

# Fiting an ARIMA(0,1,4)(1,1,2)[12] model to the log-transformed series
model1.cars = arima(log.cars.ts,order=c(0,1,4),seasonal=list(order=c(1,1,2), period=12),method = "ML")
coeftest(model1.cars)

# Obtain the residuals of the model
res.model1 = residuals(model1.cars);  
par(mfrow=c(1,2))
acf(res.model1, lag.max = 36)
pacf(res.model1, lag.max = 36)

# Fiting an ARIMA(3,1,4)(1,1,2)[12] model to the log-transformed series
model3.cars = arima(log.cars.ts,order=c(3,1,4),seasonal=list(order=c(1,1,2), period=12),method = "ML")
coeftest(model3.cars)

# Obtain the residuals of the model
res.model3 = residuals(model3.cars)
par(mfrow=c(1,2))
acf(res.model3, lag.max = 36)
pacf(res.model3, lag.max = 36)

# Fiting an ARIMA(2,1,2)(1,1,2)[12] model to the log-transformed series
model4.cars = arima(log.cars.ts,order=c(2,1,1),seasonal=list(order=c(1,1,2), period=12),method = "ML")
coeftest(model4.cars)

# Obtain the residuals of the model
res.model4 = residuals(model4.cars)
par(mfrow=c(1,2))
acf(res.model4, lag.max = 36)
pacf(res.model4, lag.max = 36)

# Fiting an ARIMA(2,1,2)(1,1,2)[12] model to the log-transformed series
model5.cars = arima(log.cars.ts,order=c(2,1,2),seasonal=list(order=c(1,1,2), period=12),method = "ML")
coeftest(model5.cars)

# Obtain the residuals of the model
res.model5 = residuals(model5.cars) 
par(mfrow=c(1,2))
acf(res.model5, lag.max = 36)
pacf(res.model5, lag.max = 36)

# Fit an ARIMA(3,1,2)(1,1,2)[12] model to the log-transformed series using CSS method
model0.cars = arima(log.cars.ts,order=c(3,1,2),seasonal=list(order=c(1,1,2), period=12),method = "CSS")
coeftest(model0.cars)

# Obtain the residuals of the model
res.model0 = residuals(model0.cars)
par(mfrow=c(1,2))
acf(res.model0, lag.max = 36, main = "The sample ACF of the residuals")
pacf(res.model0, lag.max = 36, main = "The sample PACF of the residuals")

# Defining a function to sort models based on AIC or BIC scores
sort.score <- function(x, score = c("bic", "aic")){
  if (score == "aic"){
    x[with(x, order(AIC)),]
  } else if (score == "bic") {
    x[with(x, order(BIC)),]
  } else {
    warning('score = "x" only accepts valid arguments ("aic","bic")')
  }
}

# Calculating the AIC and BIC scores for the models
sc.AIC=AIC(model1.cars,model2.cars,model3.cars,model4.cars,model5.cars)
sc.BIC=AIC(model1.cars,model2.cars,model3.cars,model4.cars,model5.cars,k=log(length(cars.ts)))

# Sorting the models based on AIC and BIC scores
sort.score(sc.AIC, score = "aic")
sort.score(sc.BIC, score = "aic")

#function for residual analysis
residual.analysis <- function(model, std = TRUE){
  library(TSA)
  #library(FitAR)
  if (std == TRUE){
    res.model = rstandard(model)
  }else{
    res.model = residuals(model)
  }
  par(mfrow=c(3,2))
  plot(res.model,type='o',ylab='Standardized Residuals', main="Time series plot of standardized residuals")
  abline(h=0)
  hist(res.model,main="Histogram-standardized residuals")
  qqnorm(res.model,main="QQ plot-standardized residuals")
  qqline(res.model, col = 2)
  acf(res.model,main="ACF of standardized residuals")
  print(shapiro.test(res.model))
  k=0
}

residual.analysis(model=model2.cars)

residual.analysis(model=model0.cars)

residual.analysis(model=model4.cars)

residual.analysis(model=model5.cars)


#Forecasting
sarima.for(log.cars.ts,120,0,1,3,1,1,2,12)


# Fitting an ETS model to the log-transformed series
model_ets <- ets(log.cars.ts)

# Print the model summary
summary(model_ets)

# Forecasting for the next 10 years (120 periods)
forecast_ets <- fabletools::forecast(model_ets, h = 120)

# Print the point forecasts
print(forecast_ets)

# Plot the point forecasts
plot(forecast_ets, main = "Forecast using ETS", xlab = "Year", ylab = "Log of Sales Count")

aic_ets <- AIC(model_ets)
bic_ets <- BIC(model_ets)


# Transform the original time series
transformed_ts <- log(cars.ts)

# Create a holdout set with 10% data
holdout_size <- ceiling(length(transformed_ts) * 0.1)
train_data <- window(log.cars.ts, end = c(1967, 9))
test_data <- window(log.cars.ts, start = c(1967, 10))
# Fit the model and forecast
model <- Arima(train_data, order = c(0, 1, 3), seasonal = c(1, 1, 2), lambda = 0)
forecast_result <- fabletools::forecast(model, h = length(test_data))

# Retransform the forecasted values and actual values
forecasted_values <- exp(forecast_result$mean)
actual_values <- exp(test_data)

# Calculate accuracy metrics
accuracy_result <- accuracy(forecasted_values, actual_values)

# Print the forecasted values and accuracy metrics
print(forecasted_values)
print(accuracy_result)

# Plot the time series and forecasts
plot(cars.ts, main = "Forecast for Holdout Set", xlab = "Time", ylab = "Sales Count")
lines(actual_values, col = "blue")
lines(forecasted_values, col = "red")

# Add a legend
legend("bottomright", legend = c("Actual", "Forecast"), col = c("blue", "red"), lty = 1)

