library(dplyr)
library(memisc)
library(gdata) # for xlsx files
library(GGally)
library(lmtest) # dor dwtest
library(sandwich) # for HC anf HAC
library(ggplot2) # for beautiful graphs

df <- read.xls('/Users/maxim_anisimov/Desktop/CB/data_S&P/Data_Princeton1.xlsx', head=T)
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

df_filt <- df %>% filter(Dates >= '1979-01-01', Dates < '1999-01-01')
consexpt_filt <- consexpt %>% filter(Date >= '1979-01-01', Date < '1999-01-01') 
# time interval used in the paper: 
# " This period was chosen because of the relative stability of the economy,
# the nation’s minimal exposure to severe external shock (i.e. wars), and the comprehensiveness of the data".
# They used TCB 500 instead of SP 500 but admitted that they are highly correlated

glimpse(df_filt)
glimpse(consexpt_filt)

df_filt$CONSUMER_EXPT <- consexpt$US.AVERAGE.CONSUMER.EXPECTATION # add US consumer expectations
glimpse(df_filt)

# ASSUMPTIONS

# 1. The basic assumption made on the data set was that the chosen economic indicators exert significant, 
# observable influence on the price changes in the stock market. 
# The relationship between the TCB 500 stock index level and the chosen indicators was assumed to be linear 
# and subject to random error.

# 2. The different economic variables chosen are not released on the same day within a given month.
# For example, the employment survey is released the first Friday of every month, 
# while the CPI is released the Tuesday of the third week. We have assumed that this difference in timing
# does not affect our correlation model.

# 3. The quarterly released indicator such as GDP and productivity were not included in our model since 
# the time series data is on a monthly scale. While figures such as GDP undoubtedly play an important role
# in affecting stock prices, their inclusion in the model would most likely produce inconsistencies.

# 4. We have assumed that the TCB 500 index is a good proxy for the equity market.
# From the earlier discussion, we found that it does represent the S&P 500 index well. 
# However, it is often argued that the S&P 500 is not the best measure of equity market movements 
# since it is not mean and variance sufficient.

# 5. The Gauss-Markov model was not automatically assumed.
# Unique tests were conducted to examine the Gauss-Markov assumptions as well
# as heteroscedasticity and autocorrelation in order to derive an acceptable model.



# Skipped: 10 Leading Index (then will be eliminated),
# Cntrct & Orders, Consumer Expt, FF Rate, Comd Prices
colnames(df_filt)
model_preliminary <- lm(data=df_filt, SPX.Index ~ AWH.MANU.Index + INJCJC4.Index + TMNOCHNG.Index + NHCHATCH.Index +  
                          M2.Index + LEI.IRTE.Index + EHUPUS.Index + CPTICHNG.Index + 
                          MTSLRL..Index + PPI.INDX.Index + CPI.CHNG.Index + COI.PI.Index + 
                          CONCCONF.Index + CONSUMER_EXPT + FDTR.Index + DXY.Curncy + 
                          LEICDIFF.Index)
summary(model_preliminary)

dplyr::select(df_filt, Dates, AWH.MANU.Index)
# Multicollineraity check
data_model <- df_filt[c('SPX.Index', 'AWH.MANU.Index', 'INJCJC4.Index', 'TMNOCHNG.Index', 'NHCHATCH.Index', 'M2.Index', 
                     'LEI.IRTE.Index', 'EHUPUS.Index', 'CPTICHNG.Index', 
                     'MTSLRL..Index', 'PPI.INDX.Index', 'CPI.CHNG.Index', 'COI.PI.Index', 'CONCCONF.Index', 'USTBTOT.Index',
                     'DXY.Curncy', 'LEICDIFF.Index')]
glimpse(data_model)
cor_prelim <- cor(as.matrix(data_model))
abs(cor_prelim) > 0.85 # check if absolute value of correlation is greater than hurdle = 0.85

# In the paper index of leading indicators, manufacturing new orders, manufacturing and trade sales, personal income, and PPI were dropped
# to eliminate multicollinearity

nomcol_data <- data_model %>% dplyr::select(-TMNOCHNG.Index, -MTSLRL..Index, -COI.PI.Index, -PPI.INDX.Index)
abs(cor(as.matrix(nomcol_data))) > 0.85 # check if it is OK

null_model <- lm(data=nomcol_data, SPX.Index ~ 1)
full_model <- lm(data=nomcol_data, SPX.Index ~ .)
step(null_model, scope=list(lower=null_model, upper=full_model), direction="forward")


# BEST MODEL FROM THE PAPER
df_filt_a <- df %>% dplyr::filter(Dates >= min(consexpt$Date) + 18, Dates <= max(consexpt$Date) + 18) 
df_filt_a$CONSUMER_EXPT <- consexpt$US.AVERAGE.CONSUMER.EXPECTATION

df_a <- df_filt_a[c('Dates', 'SPX.Index', 'M2.Index', 'LEI.IRTE.Index', 'EHUPUS.Index', 'CPTICHNG.Index', 'CPI.CHNG.Index',
             'CONSUMER_EXPT', 'FDTR.Index')]
glimpse(df_a)

# Skipped: contracts & orders, Comd prices
model_a <- lm(data=filter(df_a, Dates >= '1979-01-01', Dates < '1999-01-01'), SPX.Index ~ . - Dates)
summary(model_a)
                              
# Gauss-Markov Assumptions: Heteroscedasticity and Autocorrelation

par(mfrow=c(2,2)) # init 4 charts in 1 panel
plot(model_a) # check for normal distribution of residuals

lmtest::bptest(model_a)  # Breusch-Pagan test 

dwtest(model_a) # Durbin-Watson test
bgtest(model_a) # Breusch-Godfrey test

df_a_lag <- df_a
df_a_lag$SPX_lagged <- lag(df_a_lag$SPX.Index, 4)

model_b <- lm(data=filter(df_a_lag, Dates >= '1979-01-01', Dates < '1999-01-01'), SPX.Index ~ . - Dates)
summary(model_b)

lmtest::bptest(model_b)  # Breusch-Pagan test 

dwtest(model_b)
bgtest(model_b)

# Out of sample quality test

glimpse(df_a)
cor_matrix_a <- cor(as.matrix(dplyr::select(df_a, -Dates, -SPX.Index)))
cor_matrix_a
abs(cor_matrix_a) > 0.85

library(WriteXLS)

WriteXLS(df_a, ExcelFileName = "SPX_data0.xls") # save data for forecasting

df_train <- filter(df_a, Dates >= '1979-01-01', Dates < '2010-01-01')
df_test <- filter(df_a, Dates >= '2010-01-01', Dates < '2018-01-01')
glimpse(df_train)
glimpse(df_test)

# 0 model 

ggpairs(dplyr::select(df_train, SPX.Index, M2.Index, LEI.IRTE.Index, EHUPUS.Index))
ggpairs(dplyr::select(df_train, SPX.Index, CPTICHNG.Index, CPI.CHNG.Index, CONSUMER_EXPT, FDTR.Index))


model_sp0 <- lm(data=df_train, SPX.Index ~ . - Dates)
summary(model_sp0)
pred_train <- predict(model_sp0, df_train, type='response')
predicted_sp0 <- predict(model_sp0, df_test, type='response')
predicted_sp0

actual_sp <- df_test$SPX.Index

sqrt(mean(actual_sp - predicted_sp0)**2) # MSE
mean(abs(actual_sp - predicted_sp0))

lmtest::bptest(model_sp0)  # Breusch-Pagan test 

dwtest(model_sp0) # Durbin-Watson test
bgtest(model_sp0) # Breusch-Godfrey test

coeftest(model_sp0, vcov. = vcovHAC)

ggplot(data=df_train, aes(x=Dates, y=SPX.Index)) + geom_line() +
  geom_line(aes(x=df_train$Dates, y=pred_train, color='red')) + labs(title="")

ggplot(data=df_test, aes(x=Dates, y=SPX.Index)) + geom_line() +
  geom_line(aes(x=df_test$Dates, y=predicted_sp0, color='red')) + labs(title="Out of sample check 0")

# 1 model

model_sp1 <- lm(data=df_train, SPX.Index ~ . - Dates - CPI.CHNG.Index)
predicted_sp1 <- predict(model_sp1, df_test, type='response')

sqrt(mean(actual_sp - predicted_sp1)**2) # # sqrt(MSE)

lmtest::bptest(model_sp1)  # Breusch-Pagan test 
dwtest(model_sp1) # Durbin-Watson test
bgtest(model_sp1) # Breusch-Godfrey test

coeftest(model_sp1, vcov. = vcovHAC)

# 2 model 

model_sp2 <- lm(data=df_train, SPX.Index ~ . - Dates - CPI.CHNG.Index - FDTR.Index)
predicted_sp2 <- predict(model_sp2, df_test, type='response')

sqrt(mean(actual_sp - predicted_sp2)**2) # # sqrt(MSE)
mean(abs(actual_sp - predicted_sp2)) # MAE

lmtest::bptest(model_sp2)  # Breusch-Pagan test 
dwtest(model_sp2) # Durbin-Watson test
bgtest(model_sp2) # Breusch-Godfrey test

coeftest(model_sp2, vcov. = vcovHAC)


ggplot(data=df_test, aes(x=Dates, y=SPX.Index)) + geom_line() +
  geom_line(aes(x=df_test$Dates, y=predicted_sp2, color='red')) + labs(title="Out of sample check 2")

# log-models

df_train_log <- dplyr::mutate(df_train, log_SPX.Index = log(SPX.Index), log_M2.Index = log(M2.Index), 
                              log_EHUPUS.Index = log(EHUPUS.Index), log_CPTICHNG.Index = log(CPTICHNG.Index),
                              log_FDTR.Index = log(FDTR.Index))
df_test_log <- dplyr::mutate(df_test, log_SPX.Index = log(SPX.Index), log_M2.Index = log(M2.Index), 
                             log_EHUPUS.Index = log(EHUPUS.Index), log_CPTICHNG.Index = log(CPTICHNG.Index),
                             log_FDTR.Index = log(FDTR.Index))
glimpse(df_train_log)

ggpairs(dplyr::select(df_train_log, log_SPX.Index, log_M2.Index, log_EHUPUS.Index))
ggpairs(dplyr::select(df_train_log, log_SPX.Index, log_CPTICHNG.Index, log_FDTR.Index))

model_log_sp0 <- lm(data=df_train_log, log_SPX.Index ~ log_M2.Index + log_EHUPUS.Index +
                      log_CPTICHNG.Index + log_FDTR.Index + LEI.IRTE.Index + CPI.CHNG.Index + CONSUMER_EXPT)
summary(model_log_sp0)
pred_log_train <- predict(model_log_sp0, df_train_log, type='response')
predicted_sp0_log <- exp(predict(model_log_sp0, df_test_log, type='response'))
predicted_sp0_log

mean(abs(actual_sp - predicted_sp0_log)) # MAE

ggplot(data=df_test_log, aes(x=Dates, y=SPX.Index)) + geom_line() +
  geom_line(aes(x=df_test_log$Dates, y=predicted_sp0_log, color='red')) + labs(title="Out of sample check 0log")

# models with differences

dSPX <- diff(df_a[,"SPX.Index"])
ggplot(data=df_a, aes(df_a$Dates, df_a$SPX.Index, color='blue')) + geom_line()
  geom_line(aes(x=df_a$Dates[2:length(df_a$Dates)], y=dSPX, color='red'))
length(dSPX)
length(df_a$Dates[2:length(df_a$Dates)])

# models with lags

df_lagged  <- df_a
df_lagged$SPX_lagged <- lag(df_lagged$SPX.Index, 1)

df_lagged_train <- filter(df_lagged, Dates >= '1979-01-01', Dates < '2010-01-01')
df_lagged_test <- filter(df_lagged, Dates >= '2010-01-01', Dates < '2018-01-01')
glimpse(df_lagged_test)

model_lagged_sp <- lm(data=df_lagged_train, SPX.Index ~ . - Dates)
predicted_lagged_sp <- predict(model_lagged_sp, df_lagged_test, type='response')
predicted_lagged_sp

actual_lagged_sp <- df_lagged_test$SPX.Index

sqrt(mean(actual_lagged_sp - predicted_lagged_sp)**2) # sqrt(MSE)

bptest(model_lagged_sp)
dwtest(model_lagged_sp) # Durbin-Watson test
bgtest(model_lagged_sp) # Breusch-Godfrey test

coeftest(model_lagged_sp, vcov. = vcovHC)

ggplot(data=df_lagged_test, aes(x=Dates, y=SPX.Index)) + geom_line() +
  geom_line(aes(x=df_lagged_test$Dates, y=predicted_lagged_sp, color='red'))

df_lagged_test$actual_lagged_sp_lagcheck <- lag(df_lagged_test$SPX.Index, 1)

ggplot(data=df_lagged_test, aes(x=Dates, y=actual_lagged_sp_lagcheck )) + geom_line() +
  geom_line(aes(x=df_lagged_test$Dates, y=predicted_lagged_sp, color='red')) + labs(title="Check for overlearning")

glimpse(df_train)

library(gets)

spx_train <- dplyr::select(df_train, SPX.Index)
regressors_train <- dplyr::select(df_train, - SPX.Index, - Dates)

model_arx0 <- arx(spx_train, mc=TRUE, ar=1, mxreg=unlist(regressors_train), vcov.type="white") # unlist to coerce mxreg to type 'double'
coef(model_arx0)
print(model_arx0)
summary(model_arx0)
plot(model_arx0)
