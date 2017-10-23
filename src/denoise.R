require(xts)
require(lubridate)
require(dplyr)
require(smoother)

#Data Preprocessing Functionality: Data Imputation and Denoising #1

denoise_data <- function(data, type='none'){
    # Function remove noise from data 
    #
    # Input:
    # data - xts object with time-series data
    # type - type of method ('none' - without changes, 
    #       'gaussian', 'sma', 'ema', 'dema' or 'wma')
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
    window_data <- (max(core_data) - min(core_data)) / length(core_data)
    
    # denoise data
    core_data_denoise <-  smth(core_data, window = window_data, method = type) 
    
    return(xts(core_data_denoise, order.by = index_data))
}

# testing


# HH_data <- readRDS("data/household_power_consumption_datatime.rds")
# data <- xts(x = HH_data$Voltage, order.by = HH_data$datetime)
# data <- data %>% remove_na_from_data(type = "mean") %>% 
#                 aggregation_data(type = "6 hours", func_aggregate = 'median')
# ttest <- denoise_data(data, type = 'gaussian')
# plot(data)
# plot(ttest)
