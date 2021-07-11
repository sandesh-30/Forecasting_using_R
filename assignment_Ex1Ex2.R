library(readxl)
training_data <- read_excel('training data.xlsx')
test_data <- read_excel('test data.xlsx')
View(training_data)
View(test_data)
class(training_data$`Week Start Date`)
class(training_data$`Weekly Demand Quantity`)
class(training_data)
#creating time series object
min(training_data$`Week Start Date`)
max(training_data$`Week Start Date`)
time_series <- ts(data = training_data$`Weekly Demand Quantity`, start = c(2018,07,07) , end = c(2020,07,04) , frequency = 52)
View(time_series)
class(time_series)
library(ggplot2)
plot(time_series , main = "Time series analysis of demand" , xlab = "Time" , ylab = "Demand Quantity")
decom <- decompose(time_series)
plot(decom)
library(forecast)
arima_model <- auto.arima(time_series , trace = TRUE)
print(arima_model)
forecast_1 <- forecast(arima_model ,level = c(95), h = 8)
print(forecast_1)
plot(forecast_1 , xlab = "Time" , ylab = "demand Quantity")

#mape , wmape calculating
mape_formula <- mean(abs((test_data$`Weekly Demand Quantity` - forecast_1$mean)/test_data$`Weekly Demand Quantity`))*100
wmape_formula <- sum(abs(test_data$`Weekly Demand Quantity` - forecast_1$mean))/sum(abs(test_data$`Weekly Demand Quantity`))*100
print(wmape_formula)
print(mape_formula)
library(MetricsWeighted)
mape_package <- mape(test_data$`Weekly Demand Quantity`, forecast_1$mean)
wmape_package <- mape(test_data$`Weekly Demand Quantity` , forecast_1$mean , w = abs(test_data$`Weekly Demand Quantity`))
print(mape_package)
print(wmape_package)

#manually selecting p,d,q
library(ggplot2)
autoplot(time_series) #checking whether plot stationary
#performing adf test for stationarity
library(forecast)
library(tseries)
adf.test(time_series) 
#p value is greater than significance level of 1%
#setting difference d
ts_diff <- diff(time_series , differences = 1)
adf.test(ts_diff)
#we can see 1st difference is stationary. so , d = 1
autoplot(ts_diff)
#identifying AR term i.e. p
Pacf(ts_diff)
#we can see  p = 1  is of greater significance
#choosing MA term i.e. q
Acf(ts_diff)
print(Acf(ts_diff))
#q = 0 is greater significant than any other value
#we gert p = 1 , d = 1 , q = 0

new_model <- Arima(y = time_series , order = c(1 , 1 , 0))
print(new_model)
new_forecast <- forecast(new_model ,level = c(95) , h = 8)
autoplot(new_forecast , ylab = "Demand Quantity")
library(MetricsWeighted)
mape_package_new <- mape(test_data$`Weekly Demand Quantity`, new_forecast$mean)
wmape_package_new <- mape(test_data$`Weekly Demand Quantity` , new_forecast$mean , w = abs(test_data$`Weekly Demand Quantity`))
print(mape_package_new)
print(wmape_package_new)
