#acf

library(fpp2)
plot.ts(monthly_car_sales_in_quebec_1960$`Monthly car sales in Quebec 1960-1968`)
Acf(monthly_car_sales_in_quebec_1960$`Monthly car sales in Quebec 1960-1968`)
Acf(monthly_car_sales_in_quebec_1960$`Monthly car sales in Quebec 1960-1968`, lag.max = 24)
y1 <- ts(monthly_car_sales_in_quebec_1960$`Monthly car sales in Quebec 1960-1968`, frequency = 1)
plot(y1)

#additive

plot.ts(monthly_car_sales_in_quebec_1960$`Monthly car sales in Quebec 1960-1968`)
Acf(monthly_car_sales_in_quebec_1960$`Monthly car sales in Quebec 1960-1968`)
y2 <- ts(monthly_car_sales_in_quebec_1960$`Monthly car sales in Quebec 1960-1968`,frequency = 12)
plot(y2)
model1 <- hw(y2, seasonal = "additive", h=12)
names(model1)
model1$model
model1$mean
accuracy(model1)
View(model1$mean)
autoplot(y2, series = "Data")+autolayer(model1$fitted, series = "fitted values")+autolayer(model1$mean, series = "forecasts")+xlab("Time")+ylab("Car Sales in Quebec")+ggtitle("Time Plot with Forecasts")
View(model1$residuals)
View(model1)

#multiple

plot.ts(monthly_car_sales_in_quebec_1960$`Monthly car sales in Quebec 1960-1968`)
Acf(monthly_car_sales_in_quebec_1960$`Monthly car sales in Quebec 1960-1968`)
y3 <- ts(monthly_car_sales_in_quebec_1960$`Monthly car sales in Quebec 1960-1968`,frequency = 12)
plot(y3)
model2 <- hw(y3, seasonal = "multiplicative", h=12)
names(model2)
model2$model
model2$mean
accuracy(model2)
View(model2$mean)
autoplot(y3, series = "Data")+autolayer(model2$fitted, series = "fitted values")+autolayer(model2$mean, series = "forecasts")+xlab("Time")+ylab("Car Sales in Quebec")+ggtitle("Time Plot with Forecasts")
View(model2$residuals)
View(model2)

#additive decomposition

y4 <- monthly_car_sales_in_quebec_1960$`Monthly car sales in Quebec 1960-1968`
plot.ts(y4) 
Acf(y4)
y4 <- ts(y4, frequency = 12)
#Classical Decomposition Additive
fit2 <- decompose(y4, type = "additive")
autoplot(fit2)
plot(fit2)
seasadj(fit2)
plot(seasadj(fit2))
trendcycle(fit2)
plot(trendcycle(fit2))
seasonal(fit2)
plot(seasonal(fit2))
remainder(fit2)
plot(remainder(fit2))
centeredma12_2 <- ma(y4, order = 12, centre=TRUE)
centeredma12_2
modeltrendcyle2 <- holt(seasadj(fit2), h=12, PI=FALSE)
accuracy(modeltrendcyle2)
names(modeltrendcyle2)
modeltrendcyle2$mean
names(fit2)
fit2$figure
forecast2 <- 0+modeltrendcyle2$mean+fit2$figure
forecast2
autoplot(y4, series = "Data")+autolayer(forecast2, series = "Forecasts")+ylab("Car Sales in Quebec")+xlab("Month")+ggtitle("Forecasts from Additive Decomposition")

#multiplicative decomposition

y5 <- monthly_car_sales_in_quebec_1960$`Monthly car sales in Quebec 1960-1968`
plot.ts(y5) 
Acf(y5)
y5 <- ts(y5, frequency = 12)
#Classical Decomposition multiplicative
fit2 <- decompose(y5, type = "multiplicative")
autoplot(fit2)
plot(fit2)
seasadj(fit2)
plot(seasadj(fit2))
trendcycle(fit2)
plot(trendcycle(fit2))
seasonal(fit2)
plot(seasonal(fit2))
remainder(fit2)
plot(remainder(fit2))
centeredma12_2 <- ma(y5, order = 12, centre=TRUE)
centeredma12_2
modeltrendcyle2 <- holt(seasadj(fit2), h=12, PI=FALSE)
accuracy(modeltrendcyle2)
names(modeltrendcyle2)
modeltrendcyle2$mean
names(fit2)
fit2$figure
forecast2 <- 1*modeltrendcyle2$mean*fit2$figure
forecast2
autoplot(y5, series = "Data")+autolayer(forecast2, series = "Forecasts")+ylab("Car Sales in Quebec")+xlab("Month")+ggtitle("Forecasts from multiplicative Decomposition")

#regression
Y <- quebec_regression$Y
t <- quebec_regression$t
tsq <- quebec_regression$tsq
D1 <- factor(quebec_regression$D1)
D2 <- factor(quebec_regression$D2)
D3 <- factor(quebec_regression$D3)
D4 <- factor(quebec_regression$D4)
D5 <- factor(quebec_regression$D5)
D6 <- factor(quebec_regression$D6)
D7 <- factor(quebec_regression$D7)
D8 <- factor(quebec_regression$D8)
D9 <- factor(quebec_regression$D9)
D10 <- factor(quebec_regression$D10)
D11 <- factor(quebec_regression$D11)
model3 <- lm(Y~t+tsq+D1+D2+D3+D4+D5+D6+D7+D8+D9+D10+D11, data = quebec_regression)
model3
summary(model3)
accuracy(model3)
checkresiduals(model3)
hist(model3$residuals)
Acf(model3$residuals)
shapiro.test(model3$residuals)
ks.test(model3$residuals, "pnorm", mean(model3$residuals), sd(model3$residuals))
plot(model3,1)
plot(model3,2)
tf <- quebec_regressionf$t
tsqf <- quebec_regressionf$tsq
D1f <- factor(quebec_regressionf$D1)
D2f <- factor(quebec_regressionf$D2)
D3f <- factor(quebec_regressionf$D3)
D4f <- factor(quebec_regressionf$D4)
D5f <- factor(quebec_regressionf$D5)
D6f <- factor(quebec_regressionf$D6)
D7f <- factor(quebec_regressionf$D7)
D8f <- factor(quebec_regressionf$D8)
D9f <- factor(quebec_regressionf$D9)
D10f <- factor(quebec_regressionf$D10)
D11f <- factor(quebec_regressionf$D11)
modelf <- predict(model3, newdata = quebec_regressionf)
modelf
plot.ts(modelf, main="Forecasts from Regression", xlab="Month", ylab="Number of cars sold", xlim=c(0,122), ylim=c(5000,27000))
lines(modelf, col="red")
lines(model3$fitted.values, col="blue")


#arima

tsdisplay(quebec_arima$QUEBEC_ARIMA)
y <- ts(quebec_arima$QUEBEC_ARIMA, frequency = 12)

lambda <- BoxCox.lambda(y)
lambda

transy <- BoxCox(y, lambda)
tsdisplay(transy)

par(mfrow=c(2,3))
plot(y)
Acf(y)
Pacf(y)
plot(transy)
Acf(transy)
Pacf(transy)
par(mfrow=c(1,1))

sdtransy <- diff(transy, 12)
tsdisplay(sdtransy)
dsdtrans <- diff(sdtransy, 1)
tsdisplay(dsdtrans)

fit1 <- auto.arima(transy, stepwise = FALSE, approximation = FALSE, trace = FALSE, lambda=lambda)
model4 <- Arima(transy, order = c(1,0,1), seasonal = c(0,1,1), include.drift = TRUE, lambda = lambda)
summary(model4)
checkresiduals(model4)
model4f <- forecast(model4, h=12,level = c(95))
model4f
model4f$mean
View(model4f$mean)
plot(model4f, main = "Forecasts from ARIMA(1,0,1)(0,1,1)[12] with drift", xlab = "Month", ylab = "Number of cars sold")

fit2 <- auto.arima(y, stepwise = FALSE, approximation = FALSE, trace = FALSE, lambda=lambda)
model5 <- Arima(y, order = c(1,0,1), seasonal = c(0,1,1), include.drift = TRUE, lambda = lambda)
summary(model5)
checkresiduals(model5)
model5f <- forecast(model5, h=12,level = c(95))
model5f
model5f$mean
View(model5f$mean)
plot(model5f, main = "Forecasts from ARIMA(1,0,1)(0,1,1)[12] with drift", xlab = "Month", ylab = "Number of cars sold")

library(Metrics)

modelf$mean
model4f$mean
model5f$mean

rf <-window(modelf[109:120])
af <-model5f$mean
bcaf <-model4f$mean
test2 <-quebec_test
er<-test2-rf
ea<-test2-af
ebca<-test2-bcaf
test2<-ts(test2,frequency = 1)
View(rf)
af
rf

