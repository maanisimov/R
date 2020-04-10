library(dplyr)
library(xts)
library(ggplot2)
library(gdata)
library(forecast)
library(tseries)
library(TSPred)

df <- read.xls('/Users/maxim_anisimov/Desktop/CB/data_S&P/Data_Princeton1.xlsx', head=T,
               na.strings=c(NaN, '#N/A N/A'))
glimpse(df)
colnames(df)

consexpt <- read.xls('/Users/maxim_anisimov/Desktop/CB/data_S&P/consexpt.xlsx', head=T)
glimpse(consexpt)
colnames(consexpt)[2] <- 'US.AVERAGE.CONSUMER.EXPECTATION'

# SPX - S&P 500 Index
# EHUPUS - US Unemployment Rate (%)
# GDP.CURY.Index - U.S. GDP in Nominal Dollars year-over-year
# CPI.INDX.Index - US CPI Index
# INJCJC - US Initial Jobless Claims SA
# TMNOCONS - US Manufacturers New orders consumer goods SA
# MTSL - US Manuf and trade sales in nominal dollars SA
# MTSLRL - US Manuf and trade sales in millions of chained (2009) dollars SA
# CPTICHNG - US Capacity Utilization % of Total Capacity SA
# NHCHATCH - Private Total Housing Authorized by Building Permits MoM SA
# PPI - US PPI Finished Goods SA MoM
# PPF.TOT - US PPI finished goods SA
# CPI.CHNG - US CPI Urban Consumers MoM SA
# CPURNSA - US CPI Urban Consumers NSA
# CONCCONF - Conference Board Consumer Confidence SA 1985=100
# M2.YOY - Fed money supply M2 YoY % change
# LEI.IRTE - Conference board US leading index interest spread 10 year Treasury less fed funds rate
# FDTR - Federal Funds Target Rate - Upper Bound
# USTBTOT - US Trade Balance of Goods and Services SA
# USTBIMP - US IMPORT
# DXY.CURNCY - DOLLAR INDEX SPOT
# GDP.CUR - GDP US nominal dollars SAAR
# AWH.MANU - US average weekly hours all employees manufacturing SA
# TMNOCHNG - US Manufacturers New Orders Total MoM SA
# LEICDIFF - Conference board US leading net contributions vendor performance diffusion
# MTIBCHNG - US Manufacturing & Trade Inventories Total MoM SA
# MTSLCHNG - US manuf and trade sales MoM SA
# CPMFTOT - US capacity utilization manuf total SIC SA
# LEI.BP - Conference board US leading index building permits
# PPI.CHNG - US PPI Finished Goods SA MoM%
# PPI.YOY - US PPI Finished Goods NSA YoY%
# COI.PI - Conference board Coincident personal income less transfer payments 
# M2 - Fed US money supply M2 SA
# ECORUSN - US money supply M2 (billion USD) NSA
# GT10.GOVT - generic US 10 year government note
# FEDL01 - Bloomberg Dollar Total Return Index
# USTBEXP - US EXPORTS
# USTBGOOD - US trade balance of goods SA

#  Нужны, но их нет
# OEUSU004 - OECD civilian unemployment rate SA

# ФУНКЦИЯ ДЛЯ ПРЕДОБРАБОТКИ ДАННЫХ ИЗ БЛУМБЕРГА
# 'Dates' %in% colnames(df)
SoundBloombergData <- function(df) {
  if (which(sapply(colnames(df), is.element, el = 'Dates')) != 1) {
    print('Error! Column with dates must be the first column and called "Dates".')
    
  df <- data.frame(lapply(df, function(x) {gsub("#N/A N/A", NaN, x)})) # missed values in approp. format
  df$Dates <- as.Date(df$Dates) # to date format
  df[, 2:length(df)] <- format(df[, 2:length(df)], decimal.mark=".") # change decimal mark from , to .
  df[, 2:length(df)] <- lapply(df[, 2:length(df)], as.numeric) # all var. to numeric (except dates)
  }
  return(df)
} 

# MODELLING

df <- data.frame(lapply(df, function(x) {gsub("#N/A N/A", NaN, x)})) # missed values in approp. format
glimpse(df)

df[, 2:length(df)] <- format(df[, 2:length(df)], decimal.mark=".") # change decimal mark from , to .
glimpse(df)
df$Dates <- as.Date(df$Dates) # to date format
df[, 2:length(df)] <- lapply(df[, 2:length(df)], as.numeric) # all var. to numeric (except dates)
glimpse(df)

consexpt <- data.frame(lapply(consexpt, function(x) {gsub("#N/A N/A", NaN, x)}))
consexpt[, 2] <- format(consexpt[,2], decimal.mark=".")
consexpt$Date <- as.Date(consexpt$Date)
consexpt[c('US.AVERAGE.CONSUMER.EXPECTATION')] <- lapply(consexpt[c('US.AVERAGE.CONSUMER.EXPECTATION')], as.numeric)
glimpse(consexpt)

head(consexpt)
tail(consexpt)
df <- filter(df, Dates >= '1978-01-01', Dates < '2018-03-01')
df$CONSUMER_EXPT <- consexpt$US.AVERAGE.CONSUMER.EXPECTATION # add US consumer expectations
glimpse(df)
colnames(df)



df_ts <- ts(dplyr::select(df, -Dates), start = c(1978, 1), frequency = 12)
head(df_ts)

decompose(df_ts)

adf.test(df_ts[,1]) # not stationary
adf.test(log(df_ts[,1]))
adf.test(diff(df_ts[,1])) # stationary
adf.test(diff(log(df_ts[,1])))

autoplot(df_ts[,1])
autoplot(log(df_ts[, 1]))
autoplot(diff(df_ts[, 1]))
autoplot(diff(log(df_ts[,1])))

gglagplot(df_ts[,1])

ggAcf(df_ts[,1])
maxlag_sp <- 1

ggAcf(diff(df_ts[,1]))
ggAcf(diff(log(df_ts[,1])))

ggPacf(diff(df_ts[,1]))
ggPacf(diff(log(df_ts[,1]))) # only lag = 5 needed

df$log_SPX.Index <- log(df$SPX.Index)
df_log <- dplyr::select(df, -SPX.Index)
glimpse(df_log)
df_log_ts <- ts(dplyr::select(df_log, -Dates), start = c(1978, 1), frequency = 12)

?Box.test
# The Ljung–Box test may be defined as:
# H0: The data are independently distributed (i.e. the correlations in the population from which the sample is taken are 0, 
# so that any observed correlations in the data result from randomness of the sampling process).
# Ha: The data are not independently distributed; they exhibit serial correlation.
Box.test(df_ts[,1], lag=24, type='Ljung')
Box.test(df_log_ts[, ncol(df_log_ts)], lag=24, type='Ljung')

# MOVING AVERAGES

df$SPX_ma_12 = ma(df$SPX.Index, order=12) 
df$SPX_ma_24 = ma(df$SPX.Index, order=24)

ggplot() +
  geom_line(data = df, aes(x = Dates, y = SPX.Index, colour = "Actual SPX Index")) +
  geom_line(data = df, aes(x = Dates, y = SPX_ma_12,   colour = "Yearly Moving Average"))  +
  geom_line(data = df, aes(x = Dates, y = SPX_ma_24, colour = "2-Yearly Moving Average"))  +
  ylab('SPX Index')


# Bad segmentation
df_ts_train <- window(df_ts, end=c(2001, 12))
tail(df_ts_train)
df_ts_test <- window(df_ts, start=c(2002, 1))
head(df_ts_test)

regressors_train <- cbind(M2 = df_ts_train[, 'M2.Index'], RatesSpread = df_ts_train[, 'LEI.IRTE.Index'], 
                          Unemployment = df_ts_train[,'EHUPUS.Index'], CapacityUtilisation = df_ts_train[, 'CPTICHNG.Index'],
                          ConsumerExpectations = df_ts_train[, 'CONSUMER_EXPT'], FedFundsRate = df_ts_train[, 'FDTR.Index'])
length(regressors_train)
head(regressors_train)
regressors_test <- cbind(M2 = df_ts_test[, 'M2.Index'], RatesSpread = df_ts_test[, 'LEI.IRTE.Index'], 
                          Unemployment = df_ts_test[,'EHUPUS.Index'], CapacityUtilisation = df_ts_test[, 'CPTICHNG.Index'],
                          ConsumerExpectations = df_ts_test[, 'CONSUMER_EXPT'], FedFundsRate = df_ts_test[, 'FDTR.Index'])
length(regressors_test)

regressors_diff_train <- cbind(diff_M2 = diff(df_ts_train[, 'M2.Index']), diff_RatesSpread = diff(df_ts_train[, 'LEI.IRTE.Index']), 
                          diff_Unemployment = diff(df_ts_train[,'EHUPUS.Index']), 
                          diff_CapacityUtilisation = diff(df_ts_train[, 'CPTICHNG.Index']),
                          diff_ConsumerExpectations = diff(df_ts_train[, 'CONSUMER_EXPT']),
                          diff_FedFundsRate = diff(df_ts_train[, 'FDTR.Index']))
regressors_diff_test <- cbind(diff_M2 = diff(df_ts_test[, 'M2.Index']), diff_RatesSpread = diff(df_ts_test[, 'LEI.IRTE.Index']), 
                         diff_Unemployment = diff(df_ts_test[,'EHUPUS.Index']), 
                         diff_CapacityUtilisation = diff(df_ts_test[, 'CPTICHNG.Index']),
                         diff_ConsumerExpectations = diff(df_ts_test[, 'CONSUMER_EXPT']), 
                         diff_FedFundsRate = diff(df_ts_test[, 'FDTR.Index']))

sp_fit <- auto.arima(df_ts_train[,1], xreg = regressors_train, lambda=BoxCox.lambda(df_ts_train[,1]))
sp_diff_fit <-  auto.arima(df_ts_train[2:nrow(df_ts_train), 1], xreg = regressors_diff_train,
                           lambda=BoxCox.lambda(df_ts_train[,1]))

checkresiduals(sp_fit)
checkresiduals(sp_diff_fit)

summary(sp_fit)
summary(sp_diff_fit)
coefficients(sp_fit)
coefficients(sp_diff_fit)

sp_fc <- forecast(sp_fit, xreg=regressors_test, h=nrow(df_ts_test))
tail(sp_fc)
autoplot(sp_fc) + autolayer(df_ts_test[,1], series="Actual S&P 500") + xlab('Year') + ylab('S&P Index Value')

sp_diff_fc <- forecast(sp_diff_fit, xreg=regressors_diff_test, h=nrow(df_ts_test))
autoplot(sp_diff_fc)
autoplot(sp_diff_fc) + autolayer(df_ts_test[,1], series="Actual S&P 500") 

accuracy(sp_fc, df_ts_test[1,])
accuracy(sp_diff_fc, df_ts_test[1,])

# New bad trial
df_ts_train <- window(df_ts, start=c(1990, 1), end=c(2016, 12))
tail(df_ts_train)
df_ts_test <- window(df_ts, start=c(2017, 1))
head(df_ts_test)

regressors_train <- cbind(M2 = df_ts_train[, 'M2.Index'], RatesSpread = df_ts_train[, 'LEI.IRTE.Index'], 
                          Unemployment = df_ts_train[,'EHUPUS.Index'], CapacityUtilisation = df_ts_train[, 'CPTICHNG.Index'],
                          ConsumerExpectations = df_ts_train[, 'CONSUMER_EXPT'], FedFundsRate = df_ts_train[, 'FDTR.Index'])
length(regressors_train)
regressors_test <- cbind(M2 = df_ts_test[, 'M2.Index'], RatesSpread = df_ts_test[, 'LEI.IRTE.Index'], 
                         Unemployment = df_ts_test[,'EHUPUS.Index'], CapacityUtilisation = df_ts_test[, 'CPTICHNG.Index'],
                         ConsumerExpectations = df_ts_test[, 'CONSUMER_EXPT'], FedFundsRate = df_ts_test[, 'FDTR.Index'])
length(regressors_test)

sp_fit <- auto.arima(df_ts_train[,1], xreg = regressors_train, lambda = BoxCox.lambda(df_ts_train[,1]))

summary(sp_fit)
coefficients(sp_fit)
checkresiduals(sp_fit)

sp_fc <- forecast(sp_fit, xreg=regressors_test, h=nrow(df_ts_test))
autoplot(sp_fc)
autoplot(sp_fc) + autolayer(df_ts_test[,1], series="Actual S&P 500") + xlab('Year') + ylab('S&P Index Value')

# TRIAL: BETTER TRAIN AND TEST DATA

df_ts_train <- window(df_ts, end=c(2014, 12))
tail(df_ts_train)
df_ts_test <- window(df_ts, start=c(2015, 1))
head(df_ts_test)

regressors_train <- cbind(M2 = df_ts_train[, 'M2.Index'], RatesSpread = df_ts_train[, 'LEI.IRTE.Index'], 
                          Unemployment = df_ts_train[,'EHUPUS.Index'], CapacityUtilisation = df_ts_train[, 'CPTICHNG.Index'],
                          ConsumerExpectations = df_ts_train[, 'CONSUMER_EXPT'], FedFundsRate = df_ts_train[, 'FDTR.Index'])
length(regressors_train)
regressors_test <- cbind(M2 = df_ts_test[, 'M2.Index'], RatesSpread = df_ts_test[, 'LEI.IRTE.Index'], 
                         Unemployment = df_ts_test[,'EHUPUS.Index'], CapacityUtilisation = df_ts_test[, 'CPTICHNG.Index'],
                         ConsumerExpectations = df_ts_test[, 'CONSUMER_EXPT'], FedFundsRate = df_ts_test[, 'FDTR.Index'])
length(regressors_test)

sp_fit <- auto.arima(df_ts_train[,1], xreg = regressors_train, lambda = BoxCox.lambda(df_ts_train[,1]))

summary(sp_fit)
coefficients(sp_fit)
checkresiduals(sp_fit)

sp_fc <- forecast(sp_fit, xreg=regressors_test, h=nrow(df_ts_test))
nrow(df_ts_test)
length(df_ts_test[, 1])
sp_fc
autoplot(sp_fc) + autolayer(df_ts_test[,1], series="Actual S&P 500") + xlab('Year') + ylab('S&P Index Value')

accuracy(sp_fc, df_ts_test[1,])

# SIMPLE ARIMA (NO OUTSIDE INFO)

df_ts_train <- window(df_ts[,1], end=c(2013, 12))
head(df_ts_train)
df_ts_test <- window(df_ts[,1], start=c(2014, 1))
head(df_ts_test)

sp_fit <- auto.arima(df_ts_train[], lambda = BoxCox.lambda(df_ts_train))
sp_fit
coefficients(sp_fit)
checkresiduals(sp_fit)
sp_fc <- forecast(sp_fit, length(df_ts_test))
autoplot(sp_fc) + autolayer(df_ts_test, series="Data")

# Check for Kirill

df_ts_train <- window(df_ts, end=c(2009, 12))
tail(df_ts_train)
df_ts_test <- window(df_ts, start=c(2010, 1))
head(df_ts_test)

regressors_train <- cbind(M2 = df_ts_train[, 'M2.Index'], RatesSpread = df_ts_train[, 'LEI.IRTE.Index'], 
                          Unemployment = df_ts_train[,'EHUPUS.Index'], CapacityUtilisation = df_ts_train[, 'CPTICHNG.Index'],
                          ConsumerExpectations = df_ts_train[, 'CONSUMER_EXPT'], FedFundsRate = df_ts_train[, 'FDTR.Index'])
length(regressors_train)
regressors_test <- cbind(M2 = df_ts_test[, 'M2.Index'], RatesSpread = df_ts_test[, 'LEI.IRTE.Index'], 
                         Unemployment = df_ts_test[,'EHUPUS.Index'], CapacityUtilisation = df_ts_test[, 'CPTICHNG.Index'],
                         ConsumerExpectations = df_ts_test[, 'CONSUMER_EXPT'], FedFundsRate = df_ts_test[, 'FDTR.Index'])
length(regressors_test)

sp_fit <- arima(df_ts_train[,1], xreg = regressors_train, order=c(2, 1, 2), seasonal=c(1, 0, 0))

summary(sp_fit)
coefficients(sp_fit)
checkresiduals(sp_fit)

sp_fc <- forecast(sp_fit, xreg=regressors_test, h=nrow(df_ts_test))
sp_fc
autoplot(sp_fc) + autolayer(df_ts_test[,1], series="Actual S&P 500") + xlab('Year') + ylab('S&P Index Value')

accuracy(sp_fc, df_ts_test[1,])

# add PMI
pmi <- read.xls('/Users/maxim_anisimov/Desktop/CB/data_S&P/pmi.xlsx', head=T)
glimpse(pmi)
pmi[, 2] <- format(pmi[, 2], decimal.mark=".") # change decimal mark from , to .
pmi$Date <- as.Date(pmi$Date) # to date format
glimpse(pmi)
pmi[, 2] <- gsub(",", ".", pmi[, 2])
pmi[, 2] <- as.double(pmi[, 2]) # all var. to numeric (except dates)
glimpse(pmi)
glimpse(pmi$Date)
pmi <- dplyr::filter(pmi, Date>='1978-01-31')

df$PMI <- pmi[1:(nrow(df)), 2]
glimpse(df)

colnames(df)
new_df <- dplyr::select(df, Dates, SPX.Index, M2.Index, LEI.IRTE.Index, EHUPUS.Index, CPTICHNG.Index,
                        CONSUMER_EXPT, FDTR.Index, GDP.CUR..Index, PMI)
abs(cor(dplyr::select(new_df, -Dates))) > 0.7

new_ts <- ts(dplyr::select(new_df, -Dates), start = c(1978, 1), frequency = 12)
new_ts_train <- window(new_ts, end=c(2015, 12))
tail(new_ts_train)
new_ts_test <- window(new_ts, start=c(2016, 1))
head(new_ts_test)

reg_train <- cbind(M2 = new_ts_train[, 'M2.Index'], RatesSpread = new_ts_train[, 'LEI.IRTE.Index'], 
                   Unemployment = new_ts_train[,'EHUPUS.Index'], CapacityUtilisation = new_ts_train[, 'CPTICHNG.Index'],
                   ConsumerExpectations = new_ts_train[, 'CONSUMER_EXPT'], FedFundsRate = new_ts_train[, 'FDTR.Index'],
                   GDP = new_ts_train[, 'GDP.CUR..Index'], PMI = new_ts_train[, 'PMI'])
reg_test <- cbind(M2 = new_ts_test[, 'M2.Index'], RatesSpread = new_ts_test[, 'LEI.IRTE.Index'], 
                  Unemployment = new_ts_test[,'EHUPUS.Index'], CapacityUtilisation = new_ts_test[, 'CPTICHNG.Index'],
                  ConsumerExpectations = new_ts_test[, 'CONSUMER_EXPT'], FedFundsRate = new_ts_test[, 'FDTR.Index'],
                  GDP = new_ts_test[, 'GDP.CUR..Index'], PMI = new_ts_test[, 'PMI'])

sp_fit <- auto.arima(new_ts_train[,1], xreg = reg_train, lambda = BoxCox.lambda(new_ts_train[,1]))

summary(sp_fit)
coefficients(sp_fit)
checkresiduals(sp_fit)

sp_fc <- forecast(sp_fit, xreg=reg_test, h=nrow(new_ts_test))
sp_fc
autoplot(sp_fc) + autolayer(new_ts_test[,1], series="Actual S&P 500") + xlab('Year') + ylab('S&P Index Value')

accuracy(sp_fc, new_ts_test[1,])


# VECM
library(urca)
library(vars)

df_train <- dplyr::filter(new_df, Dates <= '2015-12-31')
glimpse(df_train)
df_test <- dplyr::filter(new_df, Dates > '2015-12-31')
glimpse(df_test)

# series must be non-stationary
attach(df_train) # применить к
sp_gdp_train <- cbind(SPX.Index, GDP.CUR..Index)
View(sp_gdp_train)

# fit a VAR model with appropriate lags
VARselect(sp_gdp_train, lag.max = 12, type='const')
VARselect(sp_gdp_train, lag.max = 12, type='const')$selection

# Eigen test (cointegreation test)
cointest <- ca.jo(sp_gdp_train, K=10, type='eigen',
                  ecdet = 'const', spec='transitory')
cointest
cointest@teststat[2] # H0: r = 0 to be rejected
cointest@teststat[1] # H0: r = 1 should not be rejected
cointest@cval # crit values
# OK!

# run first VECM
vecm <- cajorls(cointest) # convert in VECM
cajorls(cointest)

# extract error correction term coefficients (ECT)
vecm$rlm$coefficients[1,1] # fot the first variable
vecm$rlm$coefficients[1,2] # fot the second variable

# extract cointegrating vector
vecm$beta[1, 1] # first variable
vecm$beta[2, 1] # first variable

attach(df_test)
sp_gdp_test <- cbind(SPX.Index, GDP.CUR..Index)

# new VECM
library(tsDyn)

ts_train <- ts(dplyr::select(df_train, -Dates), start = c(1978, 1), frequency = 12)
ts_test <- ts(dplyr::select(df_test, -Dates), start = c(2016, 1), frequency = 12)

vecm <-VECM(data = ts_train, lag=9, r=1)
vecm$coefficients
summary(vecm)
vecm$fitted.values
vecm[7]
predict(vecm, newdata=df_test[1:8,])

df_test[1:9,]




# # add GDP QOQ SAAR
# 
# df_other <- read.xls('/Users/maxim_anisimov/Desktop/CB/data_S&P/Data_other.xlsx', head=T)
# head(df_other)
# df_gdp <- dplyr::select(df_other, Dates, GDP.CQOQ.Index)
# df_gdp[, 2] <- format(df_gdp[, 2], decimal.mark=".") # change decimal mark from , to .
# glimpse(df_gdp)
# df_gdp$Dates <- as.Date(df_gdp$Dates) # to date format
# df_gdp[, 2] <- as.numeric(df_gdp[, 2]) # all var. to numeric (except dates)
# glimpse(df_gdp)
# glimpse(df$Dates)
# df_gdp <- dplyr::filter(df_gdp, Dates>='1978-01-31')
# tail(df_gdp)
# df$GDP_CQOQ <- df_gdp[1:nrow(df), 2]
# glimpse(df)
# 
# new_df <- dplyr::select(df, Dates, SPX.Index, M2.Index, LEI.IRTE.Index, EHUPUS.Index, CPTICHNG.Index,
#                         CONSUMER_EXPT, FDTR.Index, GDP_CQOQ)
# abs(cor(dplyr::select(new_df, -Dates))) > 0.8
# 
# 
# new_ts <- ts(dplyr::select(new_df, -Dates), start = c(1978, 1), frequency = 12)
# new_ts_train <- window(new_ts, end=c(2015, 12))
# tail(new_ts_train)
# new_ts_test <- window(new_ts, start=c(2016, 1))
# head(new_ts_test)
# 
# reg_train <- cbind(M2 = new_ts_train[, 'M2.Index'], RatesSpread = new_ts_train[, 'LEI.IRTE.Index'], 
#                    Unemployment = new_ts_train[,'EHUPUS.Index'], CapacityUtilisation = new_ts_train[, 'CPTICHNG.Index'],
#                    ConsumerExpectations = new_ts_train[, 'CONSUMER_EXPT'], FedFundsRate = new_ts_train[, 'FDTR.Index'],
#                    GDPGrowth = new_ts_train[, 'GDP_CQOQ'])
# reg_test <- cbind(M2 = new_ts_test[, 'M2.Index'], RatesSpread = new_ts_test[, 'LEI.IRTE.Index'], 
#                    Unemployment = new_ts_test[,'EHUPUS.Index'], CapacityUtilisation = new_ts_test[, 'CPTICHNG.Index'],
#                    ConsumerExpectations = new_ts_test[, 'CONSUMER_EXPT'], FedFundsRate = new_ts_test[, 'FDTR.Index'],
#                    GDPGrowth = new_ts_test[, 'GDP_CQOQ'])
# 
# sp_fit <- auto.arima(new_ts_train[,1], xreg = reg_train, lambda = BoxCox.lambda(new_ts_train[,1]))
# 
# summary(sp_fit)
# coefficients(sp_fit)
# checkresiduals(sp_fit)
# 
# sp_fc <- forecast(sp_fit, xreg=reg_test, h=nrow(new_ts_test))
# sp_fc
# autoplot(sp_fc) + autolayer(new_ts_test[,1], series="Actual S&P 500") + xlab('Year') + ylab('S&P Index Value')
# 
# accuracy(sp_fc, new_ts_test[1,])