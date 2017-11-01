require(xts)
require(lubridate)
require(dplyr)
require(prophet)


#Prophet based Anomaly Detection Model #6

model_prophet <- function(data, yearly="auto", weekly="auto", daily="auto", interval_width=0.95){
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
    # forecast - forecast data was predicted by model 
    # historical_data - input data
    
    if (dim(data)[2] == 1){
        data_prophet <- data.frame(ds=index(data), y=coredata(data))
    } else{
        return("Please select data with one column for values!")
    }
    
    
    m <- prophet(data_prophet,  yearly.seasonality = yearly, weekly.seasonality = weekly, 
                 daily.seasonality = daily, interval.width = interval_width)
    future <- make_future_dataframe(m, periods = 1)
    future <- data_prophet %>% select(ds)
    fcst <- predict(m, future)

    result <- xts(fcst %>% select("yhat", "yhat_lower", "yhat_upper"), order.by = index(data))
    result$values <- coredata(data)
    
    return(list(model=m,
                data=result))
}

#testing

# harMet_15Min <- read.csv(
#     file="data/HARV/FisherTower-Met/hf001-10-15min-m.csv",
#     stringsAsFactors = FALSE) %>% mutate(datetime =as.POSIXct(datetime,format="%Y-%m-%dT%H:%M") ) %>%
#     na.omit(datetime) %>% select(datetime, airt)
# 
# head(harMet_15Min)
# 
# data <- xts(x = harMet_15Min$airt, order.by = harMet_15Min$datetime) %>%
#     aggregation_data(type = '2 days', func_aggregate = 'mean')
# 
# fit <- model_prophet(data)
# fit$data %>% tail()
# data %>% tail()

model_prophet_new_interval <- function(fit, percent_up=0, percent_low=0, method='both'){
    # Function prophet model for time-series data
    #
    # Input:
    # fit - result of model_prophet function
    # percent_up - percent  for define up treshholds  
    # percent_low - percent  for define low treshholds     
    # method - method for defile anomalies (both, high or low)
    #
    # Output:
    # model - prophet model
    # xts object with historical data treshholds and anomalies for visualization
    
    result <- fit$data
    
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

# testing
# fit <- model_prophet(data, interval_width = 0.9)
# fit$data %>% tail()
# 
# res <- model_prophet_new_interval(fit, percent_up=0, percent_low=50, method='both')
# plot_time_series(res, treshhold_type = 'both')
