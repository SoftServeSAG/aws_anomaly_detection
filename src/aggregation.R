require(xts)
require(lubridate)
require(dplyr)

#Data Preprocessing Functionality: Data Transformation #2

count_non_zero <- function(vector){
    # Function count non zero values in vector 
    
    sum(vector != 0)
}




aggregation_data <- function(data, type='none', func_aggregate="mean", quantile_percent=.9){
    # Function aggregate data of time-series
    #
    # Input:
    # data - xts object with time-series data
    # type - type of method ('none' - without changes, 
    #       "seconds", for example "5 seconds", or "minutes", "hours", "days", "weeks", 
    #       "months", "quarters", and "years")
    # func_aggregate - function for aggregating data, for example "sum", "mean", "median", 
    #       "max", "min", "count_non_zero" or "n_quantile"
    # quantile_percent - for "n% -quantile" aggregation function
    #
    # Output:
    # xts object with aggregated data
    
    if (type == 'none'){
        return(data)    
    }
    
    # Change type of input data to xts
    if (!is.xts(data)){
        data <- as.xts(data)
    }
    
    # parsing type of aggregation
    number_periods <- strsplit(type, " ")[[1]][1] %>% as.integer()
    type_periods <- strsplit(type, " ")[[1]][2]
    
    #Aggregate data
    if (func_aggregate == 'n_quantile'){
        aggregated_data <- period.apply(data, INDEX=endpoints(data, on=type_periods, k=number_periods), FUN=function(x){
            quantile(x, probs = quantile_percent)
        })
    } else{
        aggregated_data <- period.apply(data, INDEX=endpoints(data, on=type_periods, k=number_periods), FUN=func_aggregate)
    }
    
    return(aggregated_data)
}
