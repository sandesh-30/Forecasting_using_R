library(readxl)
training_data <- read_excel('training data.xlsx')
test_data <- read_excel('test data.xlsx')
new_train <- training_data[1:104 , 1:3]
holdout_data <- training_data[105 , 1:3] #keeping 1 datapoints in the end as holdout data as we require atleast 2*52 datapoints to define a season
View(new_train)
View(holdout_data)
View(training_data)
min(new_train$`Week Start Date`)
max(new_train$`Week Start Date`)
time_series <- ts(data = new_train$`Weekly Demand Quantity`, start = c(2018,07,07) , end = c(2020,06,27) , frequency = 52)

library(ggplot2)
plot(time_series , main = "Time series analysis of demand" , xlab = "Time" , ylab = "Demand Quantity")
plot(decompose(time_series))

library(forecast)
library(tseries)
arima_model <- auto.arima(time_series , trace = TRUE)
print(arima_model)
forecast_2 <- forecast(arima_model ,level = c(95), h = 6)
print(forecast_2)
plot(forecast_2 , xlab = "Time" , ylab = "demand Quantity")

library(MetricsWeighted)
mape_arima <- mape(holdout_data$`Weekly Demand Quantity`, forecast_2$mean[1])
wmape_arima <- mape(holdout_data$`Weekly Demand Quantity` , forecast_2$mean[1] , w = abs(holdout_data$`Weekly Demand Quantity`))
print(mape_arima)
print(wmape_arima)

library(forecast)
expo_model <- HoltWinters(time_series)
print(expo_model)

forecast_3 <- forecast(expo_model , h = 6)
plot(forecast_3 , ylab = "Demand Quantity" , xlab = "Time")
mape_hw <- mape(holdout_data$`Weekly Demand Quantity`, forecast_3$mean[1])
wmape_hw <- mape(holdout_data$`Weekly Demand Quantity` , forecast_3$mean[1] , w = abs(holdout_data$`Weekly Demand Quantity`))
print(mape_hw)
print(wmape_hw)


#Using MAPE as criteria the ARIMA works better
#recalculating parameters for ARIMA for entire data
new_ts <- ts(data = training_data$`Weekly Demand Quantity` , start = c(2018,07,07) , end = c(2020,07,04) , frequency = 52)
adf.test(new_ts)
ts_diff <- diff(new_ts , differences = 1)
adf.test(ts_diff)
#so d = 1 as it reaches significance of 1%
Pacf(ts_diff)
#p = 1 
Acf(ts_diff)
print(Acf(ts_diff))
#q = 0

######################################

arima_new_model <- Arima(y = new_ts , order = c(2,0,3)) #without recalculating parameters
forecast_new1 <- forecast(arima_new_model , h = 8)
plot(forecast_new1 , ylab = "Time" , xlab = "Demand Quantity")
mape_arima_wr <- mape(test_data$`Weekly Demand Quantity` , forecast_new1$mean)
wmape_arima_wr <- mape(test_data$`Weekly Demand Quantity` , forecast_new1$mean , w = abs(test_data$`Weekly Demand Quantity`))
print(mape_arima_wr)
print(wmape_arima_wr)

########################################with recalculated parameters (1,1,0)
arima_model_2 <- Arima(y = new_ts , order = c(1,1,0))
forecast_new2 <- forecast(arima_model_2 , h = 8)
autoplot(forecast_new2)
mape_arima_re <- mape(test_data$`Weekly Demand Quantity` , forecast_new2$mean)
wmape_arima_re <- mape(test_data$`Weekly Demand Quantity` , forecast_new2$mean , w = abs(test_data$`Weekly Demand Quantity`))
print(mape_arima_re)
print(wmape_arima_re)
