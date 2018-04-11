require(xts)
require(lubridate)
require(dplyr)
require(zoo)
require(hydroTSM)

#Data Preprocessing Functionality: Remove NA's in dates after aggregation

remove_na_from_data_in_dates <- function(data, type_ag = "1 hours", date_format="%Y-%m-%d %H:%M:%S", type='value', fill_value=-1){
    # Function replace NA values on 0, mean,... in time-seris data
    #
    # Input:
    # data - dataframe with time-series data
    # type - type of method ('none' - without changes, 
    # type_ag - type of aggregation
    # date_format - Posic format of date column
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
    
    # round dates after aggregation
    number_periods <- strsplit(type_ag, " ")[[1]][1] %>% as.integer()
    type_periods <- strsplit(type_ag, " ")[[1]][2]
    count_sec <- switch(type_periods,
           "seconds" = number_periods,
           "minutes" = number_periods * 60, 
           "hours" = number_periods * 3600, 
           "days" =  number_periods * 3600 * 24, 
           "weeks" = number_periods * 3600 * 24 * 7, 
           "months" = number_periods * 3600 * 24 * 7 * 30, 
           "quarters" = number_periods * 3600 * 24 * 7 * 90, 
           "years" = number_periods * 3600 * 24 * 7 * 365
           )

    data <- align.time(data, n=count_sec)
    
    # define NA's in dates
    data <- izoo2rzoo(data, 
                      from=start(data), 
                      to=end(data), 
                      date.fmt = date_format, 
                      tstep= type_ag)
    data <- as.xts(data)
    
    # fill NA's values in time-series data
    switch(type,
           'value' = {
               data[is.na(data)] <- fill_value
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
