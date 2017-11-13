require(xts)
require(lubridate)
require(dplyr)
require(prophet)


#Prophet based Anomaly Detection Model #6

model_prophet_train <- function(data, yearly="auto", weekly="auto", daily="auto", interval_width=0.95){
    # Function prophet model for time-series data
    #
    # Input:
    # data - xts object with time-series data
    # yearly - yearly seasonality ('auto', TRUE, FALSE), similarly
    # weekly - weekly seasonality
    # daily - daily seasonality
    # interval_width - width of the uncertainty intervals provided for the forecast 
    #
    # Output:
    # model - prophet model
    # data - forecast data was predicted by model for historical data
    
    #prepare data for prophet model
    if (dim(data)[2] == 1){
        data_prophet <- data.frame(ds=index(data), y=coredata(data))
    } else{
        return("Please select data with one column for values!")
    }
    
    # Build prophet model and predict results
    m <- prophet(data_prophet,  yearly.seasonality = yearly, weekly.seasonality = weekly, 
                 daily.seasonality = daily, interval.width = interval_width)
    #future <- make_future_dataframe(m, periods = 1)
    future <- data_prophet %>% select(ds)
    fcst <- predict(m, future)
    
    # prepare outputs for detecting anomalies
    result <- xts(fcst %>% select("yhat", "yhat_lower", "yhat_upper"), order.by = index(data))
    result$values <- coredata(data)
    
    return(list(model=m,
                data=result))
}


model_prophet_new_interval <- function(fit_data, percent_up=0, percent_low=0, method='both'){
    # Function for change treshholds interval and detect anomalies
    #
    # Input:
    # fit_data - result data from model_prophet function
    # percent_up - percent  for define up treshholds  
    # percent_low - percent  for define low treshholds     
    # method - method for defile anomalies (both, high or low)
    #
    # Output:
    # model - prophet model
    # xts object with historical data, treshholds and anomalies for visualization
    
    result <- fit_data
    
    # detect anomalies and change treshholds interval
    switch(method,
           'both' = {
               result$thresolds.h <- result$yhat + (1 + percent_up / 100) * (result$yhat_upper - result$yhat)
               result$thresolds.l <- result$yhat - (1 + percent_low / 100) * (result$yhat - result$yhat_lower)
               result$anomalies <- ifelse((result$values > result$thresolds.h) | (result$values < result$thresolds.l), result$values, NA)
           },
           'low' = {
               result$thresolds <- result$yhat - (1 + percent_low / 100) * (result$yhat - result$yhat_lower)
               result$anomalies <- ifelse(result$values < result$thresolds, result$values, NA)
           },
           'high' = {
               result$thresolds <- result$yhat + (1 + percent_up / 100) * (result$yhat_upper - result$yhat)
               result$anomalies <- ifelse(result$values > result$thresolds, result$values, NA)
           })
    
    return(result[, -(1:3)])
}


model_prophet_test <- function(fit, test){
    # Function found anomalies on test data
    #
    # Input:
    # fit - result of model_prophet function
    # test - data for testing (xts object)
    #
    # Output:
    # model - prophet model
    # xts object with testing data for predict treshholds and anomalies
    
    if (is.xts(test)){
        future <- index(test)
    } else{
        future <- test[[1]]
    }
    
    # predict test data
    all_index <- data.frame(ds=c(index(fit$data), future))
    fcst <- predict(fit$model, all_index)
    

    # prepare outputs for detecting anomalies
    result <- xts(fcst[(dim(fit$data)[1] + 1):dim(fcst)[1], ] %>% select("yhat", "yhat_lower", "yhat_upper"), order.by = index(test))
    result$values <- coredata(test)
    
    return(list(model=fit$model,
                data=result))
}


model_prophet_train_test <- function(train, test, yearly="auto", weekly="auto", daily="auto", interval_width=0.95, method='train'){
    # Function prophet model for time-series data
    #
    # Input:
    # train - xts object with train time-series data
    # test - xts object with test time-series data
    # yearly - yearly seasonality ('auto', TRUE, FALSE), similarly
    # weekly - weekly seasonality
    # daily - daily seasonality
    # interval_width - width of the uncertainty intervals provided for the forecast 
    # method - select 'train' to train model only on train data, predict for train and test, 
    # and 'all' - to train model on train and test, predict to both
    #
    # Output:
    # model - prophet model
    # data_train - forecast data was predicted by model on train
    # data_test - forecast data was predicted by model on test
    
    #prepare data for prophet model
    data <- rbind(train, test)
    
    if ((dim(data)[2] == 1) & (method == 'train')){
        data_prophet <- data.frame(ds=index(train), y=coredata(train))
    } else if (method == 'all'){
        data_prophet <- data.frame(ds=index(data), y=coredata(data))
    } else {
        return("Please select data with one column for values!")
    }
    
    # Build prophet model and predict results
    m <- prophet(data_prophet,  yearly.seasonality = yearly, weekly.seasonality = weekly, 
                 daily.seasonality = daily, interval.width = interval_width)
    #future <- make_future_dataframe(m, periods = 1)
    future <- data.frame(ds=index(data))
    fcst <- predict(m, future)
    
    # prepare outputs for detecting anomalies
    result <- xts(fcst %>% select("yhat", "yhat_lower", "yhat_upper"), order.by = index(data))
    result$values <- coredata(data)
    
    return(list(model=m,
                data_train=result[1:dim(train)[1]],
                data_test=result[(dim(train)[1] + 1):(dim(train)[1] + dim(test)[1])]))
}

