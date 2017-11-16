require(xts)
require(lubridate)
require(dplyr)
require(smoother)

#Data Preprocessing Functionality: Data Imputation and Denoising #1

denoise_data <- function(data, type='none', window_noise='auto'){
    # Function remove noise from data 
    #
    # Input:
    # data - xts object with time-series data
    # type - type of method ('none' - without changes, 
    #       'gaussian', 'sma', 'ema', 'dema' or 'wma')
    # SMA calculates the arithmetic mean of the series over the past n observations.
    # EMA calculates an exponentially-weighted mean, giving more weight to recent observations. 
    # WMA is similar to an EMA, but with linear weighting if the length of wts is 
    # equal to n. If the length of wts is equal to the length of x, the WMA will 
    # use the values of wts as weights.
    # DEMA is calculated as: DEMA = (1 + v) * EMA(x,n) - EMA(EMA(x,n),n) * v 
    # (with the corresponding wilder and ratio arguments).
    #
    # window_noise - 'auto' or the length of the smoothing window, if an integer, represents 
    # number of items, else, if a value between 0 and 1, represents the proportion of the input vector
    # Output:
    # xts object without noise
    
    if (type == 'none'){
        return(data)    
    }
    
    # Change type of input data to xts
    if (!is.xts(data)){
        data <- as.xts(data)
    }
    
    
    #prepare data to denoising
    index_data <- index(data)
    core_data <- coredata(data)
    
    # select window to denoising
    if (window_noise == 'auto'){
        window_noise <- (max(core_data) - min(core_data)) / length(core_data)
    }
    
    # denoise data
    core_data_denoise <-  smth(core_data, window = window_noise, method = type) 
    result <- xts(core_data_denoise, order.by = index_data)
    result[is.na(result)] <- data[is.na(result)]
    
    
    return(result)
}
