library(hydroGOF)
library(data.table)
library(vars)
library(useful) # for columns shift
library(latex2exp)
library(stats)
library(YieldCurve)
library(dplyr)
library(Metrics)
library(multDM) # for Diebold-Mariano test

################################
fnaive <- function(y, h){
  model <- naive(y)
  forecast <- forecast(model, h)
  return(forecast)
}

################################
# AR(1) forecasting model of class 'forecast'
far1 <- function(y, h, xreg=NULL){
  model <- Arima(y, order=c(1,0,0), include.constant=TRUE, xreg=xreg)
  forecast <- forecast(model, h)
  return(forecast)
}

far2 <- function(y, h){
  model <- Arima(y, order=c(2,0,0), include.constant=TRUE)
  forecast <- forecast(model, h)
  return(forecast)
}

# MA(3)
fma3 <- function(y, h){
  model <- Arima(y, order=c(0,0,3), include.constant=TRUE)
  forecast <- forecast(model, h)
  return(forecast)
}

# ARMA (2,2)
farma22 <- function(y, h){
  model <- Arima(y, order = c(2, 0, 2), include.drift = TRUE)
  forecast <- forecast(model, h)
  return(forecast)
}

# ARIMA(0,1,0)
farima010 <- function(y, h){
  model <- Arima(y, order = c(0, 1, 0), include.drift = TRUE)
  forecast <- forecast(model, h)
  return(forecast)
}

# ARIMA (1,1,1)
farima111 <- function(y, h){
  model <- Arima(y, order = c(1, 1, 1), include.drift = TRUE)
  forecast <- forecast(model, h)
  return(forecast)
}

# ARIMA(0,1,1)
farima011 <- function(y, h){
  model <- Arima(y, order = c(0, 1, 1), include.drift = TRUE)
  forecast <- forecast(model, h)
  return(forecast)
}

# ARIMA(1,1,0)
farima110 <- function(y, h){
  model <- Arima(y, order = c(1, 1, 0), include.drift = TRUE)
  forecast <- forecast(model, h)
  return(forecast)
}



# Auto ARIMA forecasting function ####
# default parameters are parameters used in the thesis for model selection
# TODO: check whether it works
fautoarima <- function(y, h){
  model <- auto.arima(y,
                      ic='bic',
                      d = NA, D = NA,
                      max.p = 12, max.q = 12, max.order = 28, max.d = 2,
                      start.p = 1, start.q = 1,
                      stationary = FALSE, seasonal = FALSE,
                      stepwise=TRUE)
  forecast <- forecast(model, h)
  return(forecast)
}


# ARIMA ####
# TODO: check whether it works
farima <- function(y, p, d, q, h){
  model <- Arima(y, order=c(p,d,q), include.constant=TRUE)
  forecast <- forecast(model, h)
  return(forecast)
}

# Any AR ####
# TODO: check whether it works
far <- function(y, p, h){
  model <- Arima(y, order=c(p,0,0), include.constant=TRUE)
  forecast <- forecast(model, h)
  return(forecast)
}

#######################################
tsCV_forecast <- function(y, forecast_func, h=1, fcst_share=0.2,
                          window=NULL, xreg=NULL){
  
  # если нужен прогноз только на один горизонт
  
  # y - Univariate time series
  # forecastfunction - function to return an object of class forecast.
  # Its first argument must be a univariate time series,
  # and it must have an argument h for the forecast horizon.
  # h - Forecast horizon
  # fcst_share - share of sample to be forecasted (always last observations),
  # ceiling is used if the first number of obs is not int using this share
  # window - Length of the rolling window, if NULL, a rolling window will not be used.
  # xreg - Exogeneous predictor variables passed to the forecast function if required.
  # na.rm - whether to remove NA forecasts (depreciated)
  
  # число наблюдений (из конца), используемых для кросс-валид. тестирования
  test_size <- ceiling(fcst_share*length(y)) 
  
  # ошибки прогнозов на кросс-валидации
  if (h == 1){
    tsCV_errors <- tsCV(y, forecastfunction=forecast_func, h=h,
                            window=window, xreg=xreg)
  } else {
    tsCV_errors <- tsCV(y, forecastfunction=forecast_func, h=h,
                          window=window, xreg=xreg)[,h] 
  }
  
  tsCV_errors <- head(tsCV_errors, -h) # убираю h последних недоступных ошибок
  #y <- tail(y, -h) # убираю значения в начале, для которых нет прогнозов (зачем резать начало, если оно мне не понадобится для прогнозов?)
  # берем прогнозы (только на тестовой выборке)
  tsCV_fcsts <- tail(y, test_size) - c(tail(tsCV_errors, test_size))
  # делаем прогнозы объектом xts
  tsCV_fcsts <- xts(tsCV_fcsts, order.by=index(tail(y, test_size)))
  
  # deprecitated (no NaNs expected now)
  #if (na.rm == T) {
  #  tsCV_forecast <- tsCV_forecast[!is.na(tsCV_forecast)]}
  
  return (tsCV_fcsts)
}


###############################################
# TODO: можно лексически задать любой тип модели и внутри функции создавать forecast function
tsCV_forecast.data <- function(data, forecast_func, h=1, fcst_share=0.2,
                          window=NULL, xreg=NULL){
  
  # если нужен прогноз для всех колонок data
  
  # data - xts / data.frame
  # 
  # forecast_func - function to return an object of class forecast.
  # if class == function: one function used for all data
  # if class == list (of model types of class string): specific models used for specif columns
  # ONLY LIMITED NUMBER OF MODEL TYPES IN LIST IS SUPPORTED FOR NOW
  # 
  # Its first argument must be a univariate time series,
  # and it must have an argument h for the forecast horizon.
  # h - Forecast horizon
  # fcst_share - share of sample to be forecasted (always last observations),
  # ceiling is used if the first number of obs is not int with this share
  # window - Length of the rolling window, if NULL, a rolling window will not be used.
  # na.rm - whether to remove NA forecasts (depreciated)
  
  # число наблюдений (из конца), используемых для кросс-валид. тестирования
  test_size <- ceiling(fcst_share*nrow(data)) 
  
  # создаем пустую матрицу, куда будем вставлять прогнозы каждой колонки
  tsCV_fcsts <- matrix(NA_real_, nrow = test_size, ncol = ncol(data))
  colnames(tsCV_fcsts) <- colnames(data)
  
  # все прогнозы на кросс-валидации (для всех колонок)
  for (col in 1:ncol(tsCV_fcsts)){
    
    # определяем функцию прогнозирования для колонки данных
    # если forecast_func = list, для каждой колонки своя функция прозгнозирования
    if (class(forecast_func) == 'list'){
      forecast_function_type <- forecast_func[col]
      if (forecast_function_type == 'AR1'){
        forecast_function <- far1
      } else if (forecast_function_type == 'AR2'){
        forecast_function <- far2
      } else if (forecast_function_type == 'MA3'){
        forecast_function <- fma3
      } else if (forecast_function_type == 'ARMA22'){
        forecast_function <- farma22
      } else if (forecast_function_type == 'ARIMA010'){
        forecast_function <- farima010
      } else if (forecast_function_type == 'ARIMA111'){
        forecast_function <- farima111
      } else if (forecast_function_type == 'ARIMA011'){
        forecast_function <- farima011
      } else if (forecast_function_type == 'ARIMA110'){
        forecast_function <- farima110
      }
    } else if (class(forecast_func) == 'function'){
      forecast_function <- forecast_func
    }
    
    tsCV_fcsts[,col] <- tsCV_forecast(data[,col],
                                      forecast_func=forecast_function,
                                      h=h, fcst_share=forecast_share,
                                      window=window, xreg=xreg)
  }
  
  # преобразуем в объект класса xts 
  tsCV_fcsts <- xts(tsCV_fcsts, order.by = index(tail(data, test_size)))
  
  return (tsCV_fcsts)
}


####################################
# VAR cross-validation (rolling origin)
tsCV_forecast.var <- function(data, h=1, p=1, fcst_share=0.2, type='const'){
  ### Прогнозирование с помощью VAR расширяющимся окном
  
  # число наблюдений для прогнозов
  test_size <- ceiling(fcst_share*nrow(data))
  # инициализация матрицы прогнозов модели
  var_fcsts <- matrix(NA_real_, nrow=test_size, ncol = ncol(data)) 
  
  for (row in 1:test_size){ # итерируемся по нужным наблюдениям
    last_train <- nrow(data) - test_size + (row - 1)
    train_data <- data[1:last_train,]
    var_model <- vars::VAR(as.ts(train_data), p=p, type=type, exogen=NULL)
    var_model_fcst <- forecast(var_model, h=h)
    
    for (col in 1:ncol(data)){ # добавляем прогнозы для текущего окна
      var_fcsts[row,col] <- var_model_fcst$forecast[[col]]$mean[h]
    }
  }
  
  var_fcsts <- xts(var_fcsts, order.by=index(tail(data, test_size)))
  colnames(var_fcsts) <- colnames(data)
  
  return(var_fcsts)
}


## BVAR  with with conjugate conjugate Normal-Inverse Wishart prior ####
## with CV (rolling origin) #
# Прогнозирование с помощью BVAR расширяющимся окном
tsCV_forecast.bvar <- function(data, horizon=1, p=1, fcst_share=0.2, fast=TRUE,
                               exogen = NULL, constant = TRUE,
                               n_simulations=0,
                               lambda = c(0.2, 1, 1, 1, 100, 100), delta = 1,
                               s2_lag = NULL,
                               v_prior = NULL, carriero_hack = FALSE){
  
  # число наблюдений для прогнозов
  test_size <- ceiling(fcst_share*nrow(data))
  # инициализация матрицы прогнозов модели
  bvar_fcsts <- matrix(NA_real_, nrow=test_size, ncol = ncol(data)) 
  colnames(bvar_fcsts) <- colnames(data)
  
  for (row in 1:test_size){ # итерируемся по нужным наблюдениям
    last_train <- nrow(data) - test_size + (row - 1)
    train_data <- data[1:last_train,]
    
    setup <- bvar_conj_setup(as.matrix(train_data), p = p,
                             Z_in = NULL, constant = constant,
                             lambda = lambda, delta = delta,
                             s2_lag = s2_lag,
                             v_prior = v_prior, carriero_hack = carriero_hack)
    bvar_model <- bvar_conj_estimate(setup, keep=n_simulations)
    bvar_model_fcst <- bvar_conj_forecast(bvar_model, out_of_sample=TRUE,
                                h=horizon, fast_forecast=fast) %>% dplyr::filter(h==horizon)
    
    for (col in colnames(data)){ # добавляем прогнозы для текущего окна
      bvar_fcsts[row, col] <- bvar_model_fcst %>% filter(variable == col) %>% .$value
    }
  }
  
  bvar_fcsts <- xts(bvar_fcsts, order.by=index(tail(data, test_size)))
  
  return(bvar_fcsts)
}


## Forecast averaging ####
# forecasts - data.frame of forecasts
#avg_forecasts <- function(forecasts_list){
#  
#  average_forecasts <- matrix(NA_real_, nrow = nrow(forecasts_list[[1]]),
#                                        ncol = ncol(forecasts_list[[1]]))
#  
#  if (ncol(forecasts) != 1){
#     
#    for col in ncol(forecasts){
#      average_forecasts[,col] < rowMeans(X_h1)
#    }
#    
#  }
#}

###################
# FAST SOLUTION
# if var_model=TRUE, ignores content provided in model
get_model_report <- function(data, model, h_vector, fcst_share, maturity_vector,
                             var_model=FALSE, var_p=1,
                             include_init_data=FALSE, window=NULL, xreg=NULL,
                             by_horizon=TRUE, by_stats=FALSE){
  
  results <- list()
  
  if (include_init_data == TRUE){
    results[['data']] <- data
  }

for (horizon in h_vector){
  
  # прогнозы для текущего горизонта
  if (var_model == FALSE){
    cur_forecasts <- tsCV_forecast.data(data, model, h=horizon,
                                        fcst_share=fcst_share)
  } else {
    cur_forecasts <- tsCV_forecast.var(data, p=var_p, type='const',
                                       h=horizon, fcst_share=fcst_share)
  }

  # RMSFE для текущего горизота
  cur_rmse <- data.frame(maturity=maturity_vector, 
                         RMSE=tsCV_rmse(cur_forecasts, data),
                         row.names=NULL)
  
  # lists of forecasts and RMSE for horizons
  h_name = paste0('h', horizon) 
  # depending on wanted order
  if (by_horizon == TRUE){
    results[[h_name]][['forecasts']] = cur_forecasts
    results[[h_name]][['RMSFE']] = cur_rmse
    #results[[h_name]][['MASFE']] = cur_mase
  } else if (by_stat == TRUE){
    results[['forecasts']][[h_name]] = cur_forecasts
    results[['RMSFE']][[h_name]] = cur_rmse
    #results[['MASFE']][[h_name]] = cur_mase
  }
  }

  return (results)
}

##############################################################
## NO EXOGEN FOR VAR and BVAR!!!
## DM.pvalues =c(soft, tight)
## if bvar_lambda_vector is a list of numeric, each set is used for each horizon
get_model_report.DNS <- function(yields, NSParams, model_type, h_vector, fcst_share,
                                 maturity_vector, lambda=0.0609, no_curvature=FALSE, 
                                 include_init_data=FALSE, window=NULL, xreg=NULL,
                                 contemp_variables=NULL,
                                 bvar_lambda_vector=c(0.2, 1, 1, 100, 100, 100),
                                 bvar_delta='AR1',
                                 benchmark_forecasts_list=NULL, dm_power=2,
                                 small.sample.correction=TRUE, alt_hypothesis='less',
                                 bvar.lambdas_validation=TRUE,
                                 bvar.validation_split='same size',
                                 l_tight.grid=c(0.01, 0.2, 1, 10, 20, 50, 500, 10000),
                                 l_io.grid=c(0.01, 0.2, 1, 10, 20, 50, 500, 10000),
                                 by_horizon=TRUE, by_stats=FALSE){
  
  test_size <- ceiling(fcst_share*nrow(yields))
  
  if (no_curvature == FALSE){
    n_factors <- 3
  } else {
    n_factors <- 2
  }
  
  results <- list()
  
  if (include_init_data == TRUE){
    results[['data']][['yields']] <- yields
    results[['data']][['NSParameters']] <- NSParams
  }
  
  for (horizon in h_vector){
    
    # прогнозы для текущего горизонта
    if (model_type == 'AR1'){
      factors_forecasts <- tsCV_forecast.data(NSParams[,1:n_factors], far1, h=horizon,
                                            fcst_share=fcst_share, xreg=xreg)
    } else if (model_type == 'AR2'){
      factors_forecasts <- tsCV_forecast.data(NSParams[,1:n_factors], far2, h=horizon,
                                              fcst_share=fcst_share, xreg=xreg)
    } else if (substring(model_type, 1, 3) == 'VAR'){
      lag <- as.numeric(substring(model_type, 4, nchar(model_type)))
      
      if (is.null(contemp_variables) == TRUE){
        factors_forecasts <- tsCV_forecast.var(NSParams[,1:n_factors], p=lag,
                                           type='const', h=horizon, 
                                           fcst_share=fcst_share)
      } else {
        train_data <- cbind(NSParams[,1:n_factors], contemp_variables)
        factors_forecasts <- tsCV_forecast.var(train_data, p=lag,
                                              type='const', h=horizon, 
                                              fcst_share=fcst_share)[,1:n_factors]
      }
    
    # BVAR in get_model_report.DNS ####
    } else if (substring(model_type, 1, 4) == 'BVAR'){
      lag <- as.numeric(substring(model_type, 5, nchar(model_type)))
      
      # если для каждого горизонта могут быть разные лямбды
      if (class(bvar_lambda_vector) == "list"){
        current_bvar_lambda_vector <- bvar_lambda_vector[[paste0("h", horizon)]]
      } else{
        current_bvar_lambda_vector <- bvar_lambda_vector
      }
      
      if (is.null(contemp_variables) == TRUE){
        
        if (bvar.lambdas_validation == TRUE){
          print('BVAR lambda_tight and lambda_io validation is used')
          ##############
          # Validation
          
          # define initial best TRMSEs
          best_trmse <- 10^6
          trmse.robustness <- data.frame(matrix(nrow=0, ncol=3)) 
          
          # define train set used as the whole data
          train_set.yields <- head(yields, -test_size)
          train_set.NSParams <- head(NSParams, -test_size)
          
          # define validation share
          if (bvar.validation_split == 'same size'){
            validation_share <- fcst_share/(1-fcst_share)
          } else if (bvar.validation_split == 'same proportion'){
            validation_share <- fcst_share
          }
          
          # Iterate through bvar lambdas
          for (l_tight in l_tight.grid){
            for (l_io in l_io.grid){
              #print(paste0('l_tight: ', l_tight))
              #print(paste0('l_io: ', l_io))
              
              # define current bvar lambdas
              current_validation_lambdas <- current_bvar_lambda_vector
              current_validation_lambdas[1] <- l_tight
              current_validation_lambdas[4] <- l_io
              
              # estimate model an test TRMSE on validation set
              validation_results <- get_model_report.DNS(yields=train_set.yields,
                                           NSParams=train_set.NSParams[,1:n_factors],
                                                         no_curvature=no_curvature,  
                                                         model_type=model_type, 
                                                         h_vector=c(horizon),
                                                         fcst_share=validation_share,
                                                    maturity_vector=maturity_vector,
                                                      bvar.lambdas_validation=FALSE,
                                      bvar_lambda_vector=current_validation_lambdas,
                                                         bvar_delta=bvar_delta)
              
              current_val_trmse <- validation_results[[paste0('h', horizon)]]$yields$TRMSE$absolute
              # add to save distribution of TRMSEs
              trmse.robustness <- rbind(trmse.robustness, 
                                        c(l_tight, l_io, current_val_trmse))
              
              # Check if current bvar lambdas are better
              if (current_val_trmse < best_trmse){
                best_trmse <- current_val_trmse
                best_validation_bvar_lambdas <- current_validation_lambdas
              }
            }
          } 
          colnames(trmse.robustness) <- c('l_tight', 'l_io', 'TRMSE_validation')
          current_bvar_lambda_vector <- best_validation_bvar_lambdas
          # END OF VALIDATION
          ############
        }

        factors_forecasts <- tsCV_forecast.bvar(NSParams[,1:n_factors], p=lag, 
                                              horizon=horizon, fcst_share=fcst_share,
                                                fast=TRUE, 
                                                lambda=current_bvar_lambda_vector,
                                                delta=bvar_delta)
        
      } else {
        
        if (bvar.lambdas_validation == TRUE){
          print('BVAR lambda_tight and lambda_io validation is used')
          ##############
          # Validation
          
          # define initial best TRMSEs
          best_trmse <- 10^6
          trmse.robustness <- data.frame(matrix(nrow=0, ncol=3))
          
          # define train set used as the whole data
          train_set.yields <- head(yields, -test_size)
          train_set.NSParams <- head(NSParams, -test_size)
          train_set.contemp_variables <- head(contemp_variables, -test_size)
          
          # define validation share
          if (bvar.validation_split == 'same size'){
            validation_share <- fcst_share/(1-fcst_share)
          } else if (bvar.validation_split == 'same proportion'){
            validation_share <- fcst_share
          }
          
          # Iterate through bvar lambdas
          for (l_tight in l_tight.grid){
            for (l_io in l_io.grid){
              #print(paste0('l_tight: ', l_tight))
              #print(paste0('l_io: ', l_io))
              
              # define current bvar lambdas
              current_validation_lambdas <- current_bvar_lambda_vector
              current_validation_lambdas[1] <- l_tight
              current_validation_lambdas[4] <- l_io
              
              # estimate model an test TRMSE on validation set
              validation_results <- get_model_report.DNS(yields=train_set.yields,
                                          NSParams=train_set.NSParams[,1:n_factors],
                                                         no_curvature=no_curvature,
                                                         model_type=model_type, 
                                                         h_vector=c(horizon),
                                                         fcst_share=validation_share,
                                                     maturity_vector=maturity_vector,
                                        contemp_variables=train_set.contemp_variables, 
                                                        bvar.lambdas_validation=FALSE,
                                        bvar_lambda_vector=current_validation_lambdas,
                                                         bvar_delta=bvar_delta)
              
              current_val_trmse <- validation_results[[paste0('h', horizon)]]$yields$TRMSE$absolute
              # add to save distribution of TRMSEs
              trmse.robustness <- rbind(trmse.robustness, 
                                        c(l_tight, l_io, current_val_trmse)) 
              
              # Check if current bvar lambdas are better
              if (current_val_trmse < best_trmse){
                best_trmse <- current_val_trmse
                best_validation_bvar_lambdas <- current_validation_lambdas
              }
            }
          }
          colnames(trmse.robustness) <- c('l_tight', 'l_io', 'TRMSE_validation')
          current_bvar_lambda_vector <- best_validation_bvar_lambdas
          # END OF VALIDATION
          ############
        }
        
        train_data <- cbind(NSParams[,1:n_factors], contemp_variables)
        factors_forecasts <- tsCV_forecast.bvar(train_data, p=lag, 
                                      horizon=horizon, fcst_share=fcst_share,
                                      fast=TRUE, 
                                      lambda=current_bvar_lambda_vector,
                                      delta=bvar_delta)[,1:n_factors]
      }
      

    } else if (class(model_type) == 'list'){
      # если список, то так и передавай
      # РАБОТАЕТ ТОЛЬКО ДЛЯ ОДНОМЕРНЫХ МОДЕЛЕЙ!!! Для VAR, BVAR нет смысла
      factors_forecasts <- tsCV_forecast.data(NSParams[,1:n_factors],
                                              forecast_func=model_type,
                                              h=horizon,
                                              fcst_share=fcst_share, xreg=xreg)
    }
    
    if (no_curvature == FALSE){
      colnames(factors_forecasts) <- c('level', 'slope', 'curvature')
    } else {
      colnames(factors_forecasts) <- c('level', 'slope')
    }
    
    
    # RMSFE для текущего горизонта
    factors_rmse <- data.frame(factor=colnames(factors_forecasts),
                              RMSE=tsCV_rmse(factors_forecasts, NSParams[,1:n_factors]),
                              row.names=NULL)
    
    # Все то же самое, но теперь для ставок
    if (no_curvature == FALSE){
      curv <- factors_forecasts[,'curvature']
    } else {
      curv <- NULL
    }
    
    yields_forecasts <- NS_curve(dates=as.Date(tail(index(yields), test_size)),
                               level=factors_forecasts[,'level'],
                               slope=factors_forecasts[,'slope'],
                               curvature=curv, 
                               lambda=lambda, maturities=maturity_vector)
    yields_rmse <- data.frame(maturity=maturity_vector,
                        RMSE=tsCV_rmse(cv_forecasts=yields_forecasts,
                                       initial_data=yields),
                                        row.names=NULL)
    yields_trmse <- sqrt(mean(yields_rmse$RMSE^2))
    
    # lists of forecasts and RMSE for horizons
    h_name = paste0('h', horizon) 
    
    # depending on wanted order
    if (by_horizon == TRUE){
      results[[h_name]][['NSParameters']][['forecasts']] = factors_forecasts
      results[[h_name]][['NSParameters']][['RMSFE']] = factors_rmse
      results[[h_name]][['yields']][['forecasts']] = yields_forecasts
      results[[h_name]][['yields']][['RMSFE']]$absolute = yields_rmse
      results[[h_name]][['yields']]$TRMSE$absolute <- yields_trmse
      
      if (substring(model_type, 1, 4) == 'BVAR'){
        # save BVAR lambdas
        results[[h_name]][['bvar_lambdas']][['values']] <- current_bvar_lambda_vector
        if (bvar.lambdas_validation == TRUE){
          results[[h_name]][['bvar_lambdas']][['source']] <- 'validation'
          results[[h_name]][['bvar_lambdas']][['robustness_check']] <- trmse.robustness
        } else {
          results[[h_name]][['bvar_lambdas']][['source']] <- 'user'
        }
      }
      
      # Diebold-Mariano test if benchmark is provided
      if (is.null(benchmark_forecasts_list) == FALSE){
        
        current_benchmark_forecasts <- benchmark_forecasts_list[[h_name]]
        
        dm.test.pvalues <- tsCV_dm_test(cv_forecasts=yields_forecasts,
                                        benchmark_forecasts=current_benchmark_forecasts,
                                        initial_data=yields,
                                        horizon=horizon)
        
        # TRMSE
        vectorized_fcst <- as.vector(yields_forecasts)
        vectorized_benchmark_fcst <- as.vector(current_benchmark_forecasts)
        vectorized_data <- as.vector(tail(yields, nrow(yields_forecasts)))
        
        dm_trmse_result <- dm.test(e1=vectorized_fcst-vectorized_data,
                              e2=vectorized_benchmark_fcst-vectorized_data,
                              alternative=alt_hypothesis,
                              h=horizon, power=dm_power)

        # dm_trmse_result <- multDM::DM.test(f1=vectorized_fcst,
        #                                    f2=vectorized_benchmark_fcst,
        #                                    y=vectorized_data, loss.type='SE', 
        #                                    c=nrow(yields_forecasts)<100, H1='more')
        
        dm_trmse_pvalue <- dm_trmse_result$p.value
        
        benchmark_rmse <- data.frame(maturity=maturity_vector,
                                     RMSE=tsCV_rmse(cv_forecasts=current_benchmark_forecasts,
                                                    initial_data=yields), row.names=NULL)
        benchmark_trmse <- sqrt(mean(benchmark_rmse$RMSE^2))
        
        results[[h_name]][['yields']][['DMTest']][['RMSE_p.values']] <- dm.test.pvalues
        results[[h_name]][['yields']][['DMTest']][['TRMSE_p.value']] <- dm_trmse_pvalue
        results[[h_name]][['yields']][['RMSFE']]$relative = yields_rmse$RMSE/benchmark_rmse$RMSE
        results[[h_name]][['yields']]$TRMSE$relative <- yields_trmse/benchmark_trmse
        
      }
      
      # TODO: сделать все те же расчеты, что и в by_horizon=TRUE!
    } else if (by_stats == TRUE){
      results[['forecasts']][['NSParameters']][[h_name]] = factors_forecasts
      results[['RMSFE']][['NSParameters']][[h_name]] = factors_rmse
      results[['forecasts']][['yields']][[h_name]] = yields_forecasts
      results[['RMSFE']][['yields']][[h_name]] = yields_rmse
      results[['TRMSE']][['yields']][[h_name]] = sqrt(mean(yields_rmse$RMSE^2))
      
      # Diebold-Mariano test if benchmark is provided
      if (is.null(benchmark_forecasts_list) == FALSE){
        
        current_benchmark_forecasts <- benchmark_forecasts_list[[h_name]]
        
        dm.test.pvalues <- tsCV_dm_test(cv_forecasts=yields_forecasts,
                                        benchmark_forecasts=current_benchmark_forecasts,
                                        initial_data=yields,
                                        horizon=horizon)
        
        # DM test
        vectorized_fcst <- as.vector(yields_forecasts)
        vectorized_benchmark_fcst <- as.vector(current_benchmark_forecasts)
        vectorized_data <- as.vector(tail(yields, nrow(yields_forecasts)))
        
        dm_trmse_result <- multDM::DM.test(f1=vectorized_fcst, f2=vectorized_benchmark_fcst,
                                           y=vectorized_data, loss.type='SE', c=nrow(yields_forecasts)<100, H1='more')
        dm_trmse_pvalue <- dm_trmse_result$p.value
        
        #outperforming.frequency.soft <- mean(dm.test.pvalues < DM.pvalues[1])
        #outperforming.frequency.tight <- mean(dm.test.pvalues < DM.pvalues[2])
        
        benchmark_rmse <- data.frame(maturity=maturity_vector,
                                     RMSE=tsCV_rmse(cv_forecasts=current_benchmark_forecasts,
                                                    initial_data=yields),
                                     row.names=NULL)
        benchmark_trmse <- sqrt(mean(benchmark_rmse$RMSE^2))
        
        results[[h_name]][['yields']][['DMTest']][['p.values']] = dm.test.pvalues
        #results[[h_name]][['yields']][['DMTest']][['outperforming.frequency.soft']] = outperforming.frequency.soft
        #results[[h_name]][['yields']][['DMTest']][['outperforming.frequency.tight']] = outperforming.frequency.tight
        results[[h_name]][['yields']]$TRMSE$absolute = yields_trmse
        results[[h_name]][['yields']]$TRMSE$relative = yields_trmse/benchmark_trmse
      }
      
    }
  }
  
  return (results)
}


##################################################################
tsCV_forecasts <- function(y, forecast_func, h_vector=c(1,6,12), fcst_share=0.2,
                          window=NULL){
  
  # DOES NOT WORK FOR NOW!
  
  # y - Univariate time series
  # forecastfunction - unction to return an object of class forecast.
  # Its first argument must be a univariate time series,
  # and it must have an argument h for the forecast horizon.
  # h - Forecast horizon
  # fcst_share - share of sample to be forecasted (always last observations),
  # ceiling is used if the first number of obs is not int with this share
  # window - Length of the rolling window, if NULL, a rolling window will not be used.
  # na.rm - whether to remove NA forecasts (depreciated)
  
  # число наблюдений (из конца), используемых для кросс-валид. тестирования
  test_size <- ceiling(fcst_share*length(y))
  y <- tail(y, test_size)
    
  # прогнозы на несколько горизонтов
  h_max <- max(h_vector)
  
  # ошибки на кросс-валидации для всех горизонтов прогнозирования
  tsCV_errors <- tsCV(y, forecastfunction=forecast_func, h=h_max)[,h_vector]

  for (col in colnames(tsCV_errors)){
    
    # убираем NA и берем последние test_size прогнозов
    cur_errors <- tail(na.omit(tsCV_errors[,col]), test_size)
    # сохраняем в матрицу прогнозы на все периоды вперед
    cur_fcsts <- y - cur_errors
    cur_rmse <- data.frame(maturity=maturity_vector, 
                           RMSE=tsCV_rmse(cur_forecasts, data), row.names=NULL)# RMSFE
    # преобразуем в xts и обозначаем горизонты прогнозов как названия столбцов
    tsCV_fcsts <- as.xts(tsCV_fcsts, order.by=index(y))
    colnames(tsCV_fcsts) <- paste0('h', as.character(h_vector))
    
  } 
  
  # slice data to consider only forecasting period
  tsCV_errors <- tail(tsCV_errors, test_size)
  
  # сохраняем в матрицу прогнозы на все периоды вперед
  tsCV_fcsts <- matrix(NA_real_, nrow=test_size, ncol = ncol(tsCV_errors))
  for (col in 1:ncol(tsCV_errors)){
    tsCV_fcsts[,col] <- y - c(tsCV_errors[,col])
  }
  # преобразуем в xts и обозначаем горизонты прогнозов как названия столбцов
  tsCV_fcsts <- as.xts(tsCV_fcsts, order.by=index(y))
  colnames(tsCV_fcsts) <- paste0('h', as.character(h_vector))
  
  return (tsCV_fcsts)
  
  # deprecitated (no NaNs expected now)
  #if (na.rm == T) {
  #  tsCV_forecast <- tsCV_forecast[!is.na(tsCV_forecast)]}
  
}


## tsCV_rmse ####
tsCV_rmse <- function(cv_forecasts, initial_data){
  ## slice all data to n obs of forecast
  ## calculate RMSE for series or dataframes!
  test_size <- nrow(cv_forecasts)
  data <- tail(initial_data, test_size)
  rmse <- hydroGOF::rmse(cv_forecasts, as.xts(data))
  
  return(rmse)
}

# tsCV_DM_test ####
tsCV_dm_test <- function(cv_forecasts, benchmark_forecasts, initial_data, horizon,
                         alt_hypothesis='less', power=2){
  
  ## slice all data to n obs of forecast
  test_size <- nrow(cv_forecasts)
  data <- tail(initial_data, test_size)
  
  dm.test.pvalues <- matrix(NA_real_, nrow=ncol(data), ncol=1)
  
  # Итерируемся по всем рядам данных
  for (col in 1:ncol(data)){
    
    # small.sample.correction = nrow(data[,col]) < small.sample.threshold
    # dm_results <- multDM::DM.test(f1=as.vector(cv_forecasts[,col]),
    #                               f2=as.vector(benchmark_forecasts[,col]),
    #                               y=as.vector(data[,col]),
    #                               loss.type=loss.type,
    #                               c=small.sample.correction,
    #                               H1=alt_hypothesis, h=horizon)
    
    # dm.test автоматически корректирует на малую выборку!
    dm_results <- dm.test(e1=as.vector(cv_forecasts[,col]-data[,col]),
                          e2=as.vector(benchmark_forecasts[,col]-data[,col]),
                          alternative=alt_hypothesis,
                          h=horizon, power=power)
    
    dm.test.pvalues[col,1] <- dm_results$p.value
  }
  
  dm.test.pvalues <- as.data.frame(dm.test.pvalues)
  row.names(dm.test.pvalues) <- colnames(data)
  colnames(dm.test.pvalues) <- c('p.value')

  return(dm.test.pvalues)
}

# ## tsCV_mase ####
# tsCV_mase <- function(cv_forecasts, initial_data, step_size=1, sum=TRUE){
#   
#   ## slice all data to n obs of forecast
#   ## calculate RMSE for series or dataframes!
#   test_size <- nrow(cv_forecasts)
#   data <- tail(initial_data, test_size)
#   
#   if (ncol(data) != 1){
#     mase <- matrix(NA_real_, nrow = 1, ncol = ncol(data))
#     for (col in 1:ncol(mase)){
#       mase[1, col] <- Metrics::mase(as.numeric(data[, col]),
#                                     as.numeric(cv_forecasts[, col]),
#                                     step_size=step_size)
#     }
#     mase <- as.data.frame(mase)
#     if (sum == TRUE){
#       mase <- rowSums(mase)
#     } else{
#       colnames(mase) <- colnames(data)
#     }
#     
#   } else {
#     mase <- Metrics::mase(as.numeric(data), as.numeric(cv_forecasts),
#                           step_size=step_size)
#   }
#   
#   return(mase)
# }

## NS_curve ####
NS_curve <- function(dates, level, slope, curvature, lambda, maturities,
                     prefix='M'){
  
  level_init <- as.matrix(level)
  level <- level_init
  for (i in 1:(length(maturities)-1)){
    level <- cbind(level, level_init)
  }
  
  slope <- as.matrix(slope)
  slope_loading <- (1-exp(-lambda*maturities))/(lambda*maturities)
  
  if (is.null(curvature) == FALSE){
    curvature <- as.matrix(curvature)
    curvature_loading <- (1-exp(-lambda*maturities))/(lambda*maturities) -
      exp(-lambda*maturities)
  }
  
  if (is.null(curvature) == FALSE){
    yc <- level + slope%*%t(slope_loading) + curvature%*%t(curvature_loading)
  } else {
    yc <- level + slope%*%t(slope_loading)
  }
  
  yc_df <- data.frame(yc)
  rownames(yc_df) <- as.Date(dates)
  colnames(yc_df) <- paste0(prefix, as.character(maturities))
  return(as.xts(yc_df))
}


# Adjusted .NS.estimator #### 
# 1) you can choose wherther to include curvature
# 2) betas are renamed to level, slope and curvature
NS.estimate <- function( rate, maturity, lambda, include_curvature=TRUE)
{
  if (include_curvature == TRUE){
    beta <- lm( rate ~ 1 + .factorBeta1(lambda,maturity) + 
                  .factorBeta2(lambda,maturity) )
    betaPar <- coef(beta)
    NaValues <- na.omit(betaPar)
    if( length(NaValues)<3 ) betaPar <- c(0,0,0)
    names(betaPar) <- c("level", "slope", "curvature")
    
  } else {
    beta <- lm( rate ~ 1 + .factorBeta1(lambda,maturity))
    betaPar <- coef(beta)
    NaValues <- na.omit(betaPar)
    if( length(NaValues)<2 ) betaPar <- c(0,0)
    names(betaPar) <- c("level", "slope")
  }
  
  EstResults <- list(Par=betaPar, Res=resid(beta))
  return(EstResults)
}

# Compute MASE (mean absolute scaled error) for yield forecasts####
compute_mase <- function(actual_values, forecasts, sum=TRUE){
  
  if (ncol(actual_values) != 1){
    mase <- matrix(NA_real_, nrow = 1, ncol = ncol(actual_values))
    for (col in 1:ncol(mase)){
      mase[1, col] <- Metrics::mase(as.numeric(actual_values[, col]),
                                    as.numeric(forecasts[, col]))
    }
    mase <- as.data.frame(mase)
    if (sum == TRUE){
      mase <- rowSums(mase)
    } else{
      colnames(mase) <- colnames(actual_values)
    }
    
  } else {
    mase <- Metrics::mase(as.numeric(actual_values), as.numeric(forecasts))
  }
  
  return(mase)
}


###################################################################
NS.fixed_lambda <- function(rate, maturity, lambda, include_curvature=TRUE){
  
  if (include_curvature == TRUE){
    FinalResults <- matrix(0, nrow(rate), 4)
    colnames( FinalResults ) <- c("level","slope","curvature","lambda")
  } else {
    FinalResults <- matrix(0, nrow(rate), 3)
    colnames( FinalResults ) <- c("level","slope","lambda")
  }

  for (i in 1:nrow(rate)){
    NS_estim <- NS.estimate(as.numeric(rate[i,]), maturity, lambda, 
                            include_curvature=include_curvature)
    BetaCoef <- NS_estim$Par # beta_coefs
    FinalResults[i,] <- c(BetaCoef, lambda)
  } 

  FinalResults <- xts(FinalResults, order.by=index(rate))
  return(FinalResults)
}


################################
## create our 3d surface yield curve ####
plot_3D_YC <- function(yields.xts, maturities,
                       xlabel='Date', ylabel='Maturity', zlabel='Yield, %'){
  yields.xts %>%
    # convert to numeric matrix
    data.matrix() %>% 
    # transpose
    t() %>%
    # draw our Plotly 3d surface
    plot_ly(
      x=as.Date(index(yields.xts)),
      y=maturities,
      z=.,
      type="surface"
    ) %>%
    plotly::layout(
      scene=list(
        xaxis=list(title=xlabel),
        yaxis=list(title=ylabel),
        zaxis=list(title=zlabel)
      )
    )
}

## Plot mean, median and percentiles of the yield curve ####
plot_stat_yc <- function(yields, maturities, country_name){
  mean_yc <- apply(yields, 2, FUN = mean)
  median_yc <- apply(yields, 2, FUN = median) # 2 for columns
  q25_yc <- colQuantiles(yields)[,2]
  q75_yc <- colQuantiles(yields)[,4]
  stat_df <- data.frame("mean"=mean_yc, "median"=median_yc, 'Q25'=q25_yc, 'Q75'=q75_yc,
                        'maturity'=maturities)
  stat_yc <- ggplot(data=stat_df) +
    geom_line(aes(x=maturity, y=mean, linetype='mean'), size=1.5) +
    geom_line(aes(x=maturity, y=median, linetype='median'), size=1.5) + 
    geom_point(aes(x=maturity, y=Q25, color='25th percentile'), size=2) +
    geom_point(aes(x=maturity, y=Q75, color='75th percentile'), size=2) + 
    labs(title=country_name, linetype='', color='',
         x='Maturity, months', y='Yield, %',
         subtitle = paste('Sample:', 
                          paste(as.yearmon(index(yields)[1]),
                                as.yearmon(index(yields[nrow(yields)])), sep=" - "),
                          sep=" ")) + 
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
          legend.position = 'bottom') +
    scale_color_brewer(palette = "Dark2")
  
  return(stat_yc)
}

# create beautiful model name
beautiful_model_name <- function(model_name, add_to_end=NULL){
  
  # Many univariate models
  if (str_detect(model_name, "-") == TRUE){
    
    models <- strsplit(model_name, '-')[[1]]
    new_names <- c()
    
    for (n in 1:length(models)){
      new_names[n] <- beautiful_model_name(models[n])
    }
    
    output <- paste0(new_names, collapse='-')
    
    if (is.null(add_to_end) == FALSE){
      output <- paste0(output, '-', add_to_end)
    }
    
  # ARMA 
  } else if( substring(model_name, 1, 4) == 'ARMA'){
    output <- paste0('ARMA(', substring(model_name, 5, 5), ',',
                   substring(model_name, 6, 6), ')')
    if (is.null(add_to_end) == FALSE){
      output <- paste0(output, '-', add_to_end)
    }
  # ARIMA
  } else if( substring(model_name, 1, 5) == 'ARIMA'){
    if (model_name != 'ARIMA010'){
      output <- paste0('ARIMA(', substring(model_name, 6, 6), ',',
                       substring(model_name, 7, 7), ',',
                       substring(model_name, 8, 8), ')') 
    } else{
      output <- 'RWD'
    }

    if (is.null(add_to_end) == FALSE){
      output <- paste0(output, '-', add_to_end)
    }
  # AR
  } else if (substring(model_name, 1, 2) == 'AR'){
    output <- paste0('AR(', substring(model_name, 3, nchar(model_name)), ')')
    if (is.null(add_to_end) == FALSE){
      output <- paste0(output, '-', add_to_end)
    }
  # MA
  } else if( substring(model_name, 1, 2) == 'MA'){ 
    output <- paste0('MA(', substring(model_name, 3, nchar(model_name)), ')')
    if (is.null(add_to_end) == FALSE){
      output <- paste0(output, '-', add_to_end)
    }
  # VAR
  } else if (substring(model_name, 1, 3) == 'VAR'){
    output <- paste0('VAR(', substring(model_name, 4, nchar(model_name)), ')')
    if (is.null(add_to_end) == FALSE){
      output <- paste0(output, '-', add_to_end)
    }
  # BVAR
  } else if (substring(model_name, 1, 4) == 'BVAR'){
    output <- paste0('BVAR(', substring(model_name, 5, nchar(model_name)), ')')
    if (is.null(add_to_end) == FALSE){
      output <- paste0(output, '-', add_to_end)
    }
  } else {
    output <- model_name
    if (is.null(add_to_end) == FALSE){
      output <- paste0(output, '-', add_to_end)
    }
  }
  
  return(output)
}

# get RMSE to plot data ####
get.RMSE_to_plot <- function(forecasts, countries, horizon_names, alpha=0.05,
                             rmse_type='relative', add_to_end=NULL){
  rmsfe_to_plot <- list()
  
  for (country in countries){
    for (horizon in horizon_names){
      models_rmsfe_list <- list()
      
      for (model_name in names(forecasts[[country]])){
        current_model_rmsfe <- forecasts[[country]][[model_name]][[horizon]]$RMSFE
        
        if (is.null(current_model_rmsfe)){ # если предыдущий путь не подошел
          current_model_rmsfe <- forecasts[[country]][[model_name]][[horizon]]$yields$RMSFE$absolute
          if (rmse_type == 'relative'){
            current_model_rmsfe$RMSE <- forecasts[[country]][[model_name]][[horizon]]$yields$RMSFE$relative
          }
          
        } else{
          if (rmse_type == 'relative'){
            current_model_rmsfe$RMSE <- 1
          }
        }
        
        if (model_name != 'RW'){
          current_model_name <- beautiful_model_name(model_name,
                                                     add_to_end=add_to_end)
        } else{
          current_model_name <- model_name
        }

        current_model_rmsfe$model <- current_model_name
        
        # if (model_name == 'RW'){
        #   current_model_rmsfe['better_than_benchmark'] = FALSE
        # } else {
        #   current_model_rmsfe['better_than_benchmark'] <- as.factor(forecasts[[country]][[model_name]][[horizon]]$yields$DMTest$RMSE_p.values < alpha)
        # }

        models_rmsfe_list[[current_model_name]] <- current_model_rmsfe
      }
      models_rmsfe <- do.call("rbind", models_rmsfe_list)
      rmsfe_to_plot[[country]][[horizon]] <- models_rmsfe
    }
  }
    
  return(rmsfe_to_plot)
}


# get TRMSE to plot data ####
get.TRMSE_to_plot <- function(forecasts, countries, horizon_names, alpha=0.05,
                             add_to_end=NULL){
  trmsfe_to_plot <- list()
  
  for (country in countries){
    for (horizon in horizon_names){
      models_trmsfe_list <- list()
      
      for (model_name in names(forecasts[[country]])){
        current_model_trmsfe <- forecasts[[country]][[model_name]][[horizon]]$TRMSE
        if (is.null(current_model_rmsfe)){ # если предыдущий путь не подошел
          current_model_trmsfe <- forecasts[[country]][[model_name]][[horizon]]$yields$TRMSE$absolute
        }
        
        if (model_name != 'RW'){
          current_model_name <- beautiful_model_name(model_name,
                                                     add_to_end=add_to_end)
        } else{
          current_model_name <- model_name
        }
        
        current_model_trmsfe$model <- current_model_name
        
        if (model_name == 'RW'){
          current_model_trmsfe['better_than_benchmark'] = FALSE
          current_model_trmsfe['p.value'] = 1
        } else {
          current_model_trmsfe['better_than_benchmark'] <- forecasts[[country]][[model_name]][[horizon]]$yields$DMTest$TRMSE_p.value < alpha
          current_model_trmsfe['p.value'] <- forecasts[[country]][[model_name]][[horizon]]$yields$DMTest$TRMSE_p.value
        }
        models_trmsfe_list[[current_model_name]] <- current_model_trmsfe
      }
      models_trmsfe <- do.call("rbind", models_rmsfe_list)
      models_trmsfe$horizon <- horizon
      current_trmsfe_to_plot[[country]][[horizon]] <- models_trmsfe
    }
    trmsfe_to_plot[[country]] <- do.call("rbind",
                                         current_trmsfe_to_plot[[country]])
  }
  
  return(trmsfe_to_plot)
}


# get MCS model selection ####
library(MCS) # процедура отбора моделей MCS
library(parallel) # параллельные вычисления

# TODO: где-то ошибка при подаче матрицы
get_MCS_selection <- function(actual_data, forecasts_data, alpha=0.1,
                              horizon_names=c('h1','h6','h12')){
  
  MCS_selection <- list()
  n_jobs <- detectCores()-1
  
  for (country in names(forecasts_data)){
    test_size <- nrow(forecasts_data[[country]]$h1$RW$forecasts)
    for (horizon_name in horizon_names){
      
      model_names <- names(forecasts_data[[country]][[horizon_name]])
      
      for (yield in colnames(forecasts_data[[country]][[horizon_name]]$RW$forecasts)){
        models_losses_list <- list()
        actual <- as.vector(tail(actual_data[[country]]$yields[,yield], test_size))
        for (model_name in model_names){
          pred <- as.vector(forecasts_data[[country]][[horizon_name]][[model_name]]$forecasts[,yield])
          squared_loss <- (pred-actual)^2
          models_losses_list[[model_name]] <- squared_loss
        }
        models_losses_matrix <- do.call("cbind", models_losses_list)
        
        cluster <- makeCluster(n_jobs)
        best_models <- MCSprocedure(models_losses_matrix, verbose = FALSE,
                                    alpha=alpha, cl=cluster)
        stopCluster(cluster)
        
        MCS_selection[[country]][[horizon_name]][[yield]] <- best_models
      }
    }
  }
  return(MCS_selection)
}

# Get legend that you want for plot grid!
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}