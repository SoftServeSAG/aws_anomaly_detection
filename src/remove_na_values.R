require(xts)
require(lubridate)
require(dplyr)


#Data Preprocessing Functionality: Data Imputation and Denoising #1

remove_na_from_data <- function(data, type='zero'){
    # Function replace NA values on 0, mean,... in time-seris data
    #
    # Input:
    # data - dataframe with time-series data
    # type - type of method ('none' - without changes, 
    #       'zero' - replace NA to 0, 
    #       'linear' - using linear interpolation,
    #       'mean' - replase NA to mean of all time series data)
    #
    # Output:
    # xts object with time-series data without NA values
    
    # Change type of input data to xts
    
    if (!is.xts(data)){
        data <- as.xts(data)
    }
    
    # remove NAs values from either sides
    data <- zoo::na.trim(object = data, sides = "both")
    
    # replace NA to 0
    switch(type,
           'zero' = {
                data[is.na(data)] <- 0
            },
           'mean' = {
                mean_data <- mean(data, na.rm=TRUE)
                data[is.na(data)] <- mean_data
           },
           'linear' = {
               data <- na.approx(data)
           }
    )
    
    return(data)
}

# testing

# n = 15
# full.dates = seq(Sys.Date(), by = 'day', length = n)
# y = rnorm(n)
# serie = zoo(y, full.dates)
# data <- as.xts(serie)
# data[c(2, 6:7, 10)] <- NA
# 
# remove_na_from_data(data, type = 'linear')

