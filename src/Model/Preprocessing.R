require(xts)
require(lubridate)
require(dplyr)


#Data Preprocessing Functionality: Data Imputation and Denoising #1

remove_na_from_data <- function(data, type='zero'){
  # Function replace NA values on 0, mean,...
  #
  # Input:
  # data - dataframe with time-series data
  # type - type of method ('none' - without changes, 
  #       'zero' - replace NA to 0, 
  #       'linear' - using linear interpolation,
  #       'mean' - replase NA to mean of all time series data)
  #
  # Output:
  # data without NA values
  
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
  
  return((data))
}


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
  
  
  return((aggregated_data))
}
# testing
