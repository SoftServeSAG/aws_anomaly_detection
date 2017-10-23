require(xts)
require(lubridate)
require(dplyr)


#Data Preprocessing Functionality: Data Transformation #2

aggregation_data <- function(data, type='none', func_aggregate="sum", quantile_percent=.9){
    # Function aggregate data of time-series
    #
    # Input:
    # data - dataframe with time-series data
    # type - type of method ('none' - without changes, 
    #       "seconds", for example "5 seconds", or "minutes", "hours", "days", "weeks", 
    #       "months", "quarters", and "years")
    # func_aggregate - function for aggregating data, for example "sum", "mean", "median", 
    #       "max", "min" or "n_quantile"
    # quantile_percent - for "n% -quantile" aggregation function
    #
    # Output:
    # aggregated data
    
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
    if (func_aggregate %in% c("sum", "mean", "median", "max", "min")){
        aggregated_data <- period.apply(data, INDEX=endpoints(data, on=type_periods, k=number_periods), FUN=func_aggregate)
    }
    
    if (func_aggregate == 'n_quantile'){
        aggregated_data <- period.apply(data, INDEX=endpoints(data, on=type_periods, k=number_periods), FUN=function(x){
            quantile(x, probs = quantile_percent)
        })
    }
    
    
    return(aggregated_data)
}

# testing

# harMet_15Min <- read.csv(
#     file="data/HARV/FisherTower-Met/hf001-10-15min-m.csv",
#     stringsAsFactors = FALSE) %>% mutate(datetime =as.POSIXct(datetime,format="%Y-%m-%dT%H:%M") ) %>% 
#     na.omit(datetime) %>% select(datetime, airt)
# 
# head(harMet_15Min)
# 
# data <- xts(x = harMet_15Min$airt, order.by = harMet_15Min$datetime)
# first(data, 10)
# 
# aggregation_data(data, type = '3 weeks', func_aggregate = 'n_quantile', quantile_percent = .5) %>% head()
# aggregation_data(data, type = '3 weeks', func_aggregate = 'median', quantile_percent = .5) %>% head()
