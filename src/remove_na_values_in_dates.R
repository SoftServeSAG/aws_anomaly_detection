require(xts)
require(lubridate)
require(dplyr)
require(zoo)

#Data Preprocessing Functionality: Remove NA's in dates after aggregation

remove_na_from_data_in_dates <- function(data, type='value', fill_value=0){
    # Function replace NA values on 0, mean,... in time-seris data
    #
    # Input:
    # data - dataframe with time-series data
    # type - type of method ('none' - without changes, 
    #       'value' - replace NA with fill_value,
    #       'linear' - using linear interpolation,
    #       'mean' - replase NA to mean of all time series data)
    # fill_value - value for filling for type 'value'
    # Output:
    # xts object with time-series data without NA values
    
    # Change type of input data to xts
    
    if (!is.xts(data)){
        data <- as.xts(data)
    }
    
    
    
    # remove NAs values from either sides
    data <- zoo::na.trim(object = data, sides = "both")
    
    # create full index
    data_index <- time(data)
    deltas <- data_index[2:length(data_index)] - data_index[1:(length(data_index) - 1)]
    count_na_in_index <- sum(deltas - 1)
    delta <-  min(data_index[2:length(data_index)] - data_index[1:(length(data_index) - 1)])
    new_index <- min(data_index) + (0:(length(data_index) - 1 + count_na_in_index)) * delta
    print(data_index)
    print(new_index)
    
    # create data with NA's in missing dates
    data_new <- zoo(x = NA, order.by = new_index) %>% as.xts()
    data_merge <- merge(data_new,data,join = 'left')$data
    print(head(data_merge, n=20))
    
    # replace NA to 0
    switch(type,
           'value' = {
               data_merge[is.na(data_merge)] <- fill_value
            },
           'mean' = {
                mean_data <- mean(data_merge, na.rm=TRUE)
                data_merge[is.na(data_merge)] <- mean_data
           },
           'linear' = {
               data_merge <- na.approx(data_merge)
           }
    )
    
    return(data_merge)
}
