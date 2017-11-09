require(xts)
require(lubridate)
require(dplyr)


#Data Preprocessing Functionality: Data Imputation and Denoising #1

remove_outliers_from_data <- function(data, type='none', number=5){
    # Function replace outliers values on quantile
    #
    # Input:
    # data - dataframe with time-series data
    # type - type of method ('none' - without changes, 
    #       'normal' - replace outliers of data to quantile, 
    #       'diff' - replace outliers of differences to quantile)
    # number - count of sigma for non outliers interval
    #
    # Output:
    # xts object with time-series data without outliers
    
    # Change type of input data to xts
    
    if (!is.xts(data)){
        data <- as.xts(data)
    }
    
    #save index
    data_index <- index(data)
    
    #define mean and sd for data and differenced data
    data_sd <- sd(data[, 1])
    data_mean <- mean(data, 1)
    
    data_diff <- diff(data, lag=1, differences=1)[-1]
    data_diff_sd <- sd(data_diff[, 1])
    data_diff_mean <- mean(data_diff[, 1])
    
    # replace outliers to quantile
    switch(type,
           'normal' = {
               data[data[, 1] > data_mean + number * data_sd] <- data_mean + number * data_sd
               data[data[, 1] < data_mean - number * data_sd] <- data_mean - number * data_sd
           },
           'diff' = {
               data_diff[data_diff[, 1] > data_diff_mean + number * data_diff_sd] <- data_diff_mean + number * data_diff_sd
               data_diff[data_diff[, 1] < data_diff_mean - number * data_diff_sd] <- data_diff_mean - number * data_diff_sd
               data[-length(data)] <- diffinv(data_diff, lag=1, differences = 1, xi = data[1])[-length(data)]
               data <- xts(data, order.by = data_index)
           }
    )
    
    return(data)
}
