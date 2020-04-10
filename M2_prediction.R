library(sophisthse)
library(forecast)

data <- sophisthse('M2_M')
autoplot(data)
head(data)
m2 <- data[, 2]
model <- auto.arima(m2)
fc <- forecast(model, 5)
autoplot(fc)
summary(model)

model_ets <- ets(m2)
fc <- forecast(model, 5)
autoplot(fc)
summary(model)