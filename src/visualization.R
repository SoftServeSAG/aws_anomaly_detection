require(xts)
require(lubridate)
require(dplyr)
require(dygraphs)


#Data Preprocessing Functionality: Data Visualization

plot_time_series <- function(data, treshhold_type='all', window_size=0.75, train_test_split='none'){
    # Function plot time-series data
    #
    # Input:
    # data - xts object with time-series data
    # treshhold_type - type of treshholds (all, none, low or high)
    # window_size - size of plot window on bottom or vector of index date c(start_date, end_date)
    # train_test_split - number from 0 to 1, which split all data on train and test
    #
    # Output:
    # dygraphs plot of time series
    
    
    # Change type of input data to xts
    if (!is.xts(data)){
        data <- as.xts(data)
    }
    
    #define window for plot and prepare xts
    if (is.numeric(window_size)){
        window_data <- index(data)[c(max(window_size * length(index(data)) %>% as.integer(), 1), length(index(data)))]
    } else{
        window_data <- window_size
    }
    
    names(data)[1] <- "values"
    
    # define dygraph plot
    plot_data <- dygraph(data)
    
    if (dim(data)[2] == 1){
            plot_data <- plot_data %>% dySeries("values", label = "Historical Data",
                                  color='blue')
    } else if (dim(data)[2] == 4){
        
        plot_data <- plot_data %>% 
            dySeries("thresolds.l", label = "Low Thresolds", color='blue') %>% 
            dySeries("thresolds.h", label = "High Thresolds", color='blue') %>% 
            dySeries(c("thresolds.l","values","thresolds.h"), label = "Historical Data",
                     color='blue') %>% 
            dySeries("anomalies", label = "Anomalies", color = 'red', drawPoints = TRUE, pointSize = 3,
                     strokeBorderColor='blue', strokeWidth=0)
        
        
    }else if ((dim(data)[2] == 3) & (treshhold_type == 'high')){

        if (min(data$values) < 0){
            data$min_values <- min(data$values) * 1.01
        } else{
            data$min_values <- min(data$values) * 0.99
        }
        plot_data <- dygraph(data) %>% 
            dySeries("thresolds", label = "High Thresolds", color='blue') %>% 
            dySeries(c("min_values","values","thresolds"), label = "Historical Data",
                     color='blue') %>%   
            dySeries("anomalies", label = "Anomalies", color = 'red', drawPoints = TRUE, pointSize = 3,
                     strokeBorderColor='blue', strokeWidth=0)
        
        
    }else if ((dim(data)[2] == 3) & (treshhold_type == 'low')){
        
        data$max_values <- max(data$values) * 1.01
        
        plot_data <- dygraph(data) %>% 
            dySeries("thresolds", label = "Low Thresolds", color='blue') %>% 
            dySeries(c("thresolds","values","max_values"), label = "Historical Data",
                     color='blue') %>% 
            dySeries("anomalies", label = "Anomalies", color = 'red', drawPoints = TRUE, pointSize = 3,
                     strokeBorderColor='blue', strokeWidth=0)
        
    }
    
    if (train_test_split !='none'){
        index_split <- index(data)[round(length(index(data)) * train_test_split, 0)]
        plot_data <- plot_data %>% 
            dyEvent(index_split, "Train-test split", labelLoc = "bottom", strokePattern='solid', color = 'red')
    }
    
    return(plot_data %>% 
               dyHighlight(highlightSeriesOpts = list(strokeWidth = 2),
                           highlightCircleSize = 4,
                           highlightSeriesBackgroundAlpha = 0.2,
                           hideOnMouseOut = FALSE) %>%
               dyLegend(show = "always", hideOnMouseOut = FALSE) %>% 
               dyOptions(colors = RColorBrewer::brewer.pal(4, "Accent")) %>% 
               dyRangeSelector(height = 40, 
                               dateWindow = window_data))
}
