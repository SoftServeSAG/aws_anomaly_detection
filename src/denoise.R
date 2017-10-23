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
        window_data <- (max(core_data) - min(core_data)) / length(core_data)
    }
    
    # denoise data
    core_data_denoise <-  smth(core_data, window = window_data, method = type) 
    
    return(xts(core_data_denoise, order.by = index_data))
}

# testing


# HH_data <- readRDS("data/household_power_consumption_datatime.rds")
# data <- xts(x = HH_data$Voltage, order.by = HH_data$datetime)
# data <- data %>% remove_na_from_data(type = "mean") %>%
#                 aggregation_data(type = "6 hours", func_aggregate = 'median')
# ttest <- denoise_data(data, type = 'wma')
# plot(data)
# plot(ttest)
# 
# n  = 50
# x  = seq(-pi,pi,length.out=n)
# y  = sin(x) + (runif(length(x))*0.1) #NOISY DATA
# ys = smth(y,window = .1,method = "gaussian") #SMOOTHING
# plot(x,y,type="l",col="red")
# lines(x,ys,col="black",lwd=3)
# title("Example Smoothing of Noisy Data")

