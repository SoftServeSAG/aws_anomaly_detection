require(xts)
require(lubridate)
require(dplyr)
require(dygraphs)


#Data Preprocessing Functionality: Data Visualization

plot_time_series <- function(data, columns='one'){
    # Function plot time-series data
    #
    # Input:
    # data - xts object with time-series data
    # columns - columns to visualize
    #
    # Output:
    # dygraphs plot of time series
    
    
    # Change type of input data to xts
    if (!is.xts(data)){
        data <- as.xts(data)
    }
    
    switch(columns,
        'one' = {
            #define window for plot
            window_data <- index(data)[c(.75, 1) * length(data) %>% as.integer()]
            names(data) <- "Historical Data"
            
            #plot time series data
            plot_data <- dygraph(data, main = "Time series", 
                                 ylab = "Values",
                                 xlab = "Time") %>% 
                dyRangeSelector(height = 40, 
                                dateWindow = window_data)
        }
    )

    
    return(plot_data)
}

# testing

# harMet_15Min <- read.csv(
#     file="data/HARV/FisherTower-Met/hf001-10-15min-m.csv",
#     stringsAsFactors = FALSE) %>% mutate(datetime =as.POSIXct(datetime,format="%Y-%m-%dT%H:%M") ) %>%
#     na.omit(datetime) %>% select(datetime, airt)
# 
# head(harMet_15Min)
# 
# data <- xts(x = harMet_15Min$airt, order.by = harMet_15Min$datetime) %>% 
#     aggregation_data(type = '1 days', func_aggregate = 'median')
# 
# plot_time_series(data, columns = 'one')
