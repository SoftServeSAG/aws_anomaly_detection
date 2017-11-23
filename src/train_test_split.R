require(xts)
require(lubridate)
require(dplyr)
require(dygraphs)


#Data Preprocessing Functionality: Train-test split

train_test_split_time_series <- function(data, split_data=0.75){
    # Function splitting time-series data in train and test, plot time-series data
    #
    # Input:
    # data - xts object with time-series data
    # split_data - number from 0 to 1, split coefficient for train and test
    #
    # Output:
    # train - train data
    # test - test data
    # plot - dygraphs plot of all time series data
    
    
    # Change type of input data to xts
    if (!is.xts(data)){
        data <- as.xts(data)
    }
    
    #define split's index
    split_index <- max(as.integer(split_data * length(index(data))), 1)
    
    # define train and test data
    train <- head(data, split_index)
    test <- tail(data, dim(data)[1] - split_index)
                                 
    # define dygraph plot
    if (length(test) > 0)
    {
        plot_data <- dygraph(merge(train, test, fill = NA)) %>% 
            dySeries("train", label = "Train Data", color='steelblue') %>% 
            dySeries("test", label = "Test Data", color=rgb(0.2,0.7,0.5)) %>% 
            dyHighlight(highlightSeriesOpts = list(strokeWidth = 2),
                        highlightCircleSize = 4,
                        highlightSeriesBackgroundAlpha = 0.2,
                        hideOnMouseOut = FALSE) %>%
            dyLegend(show = "always", hideOnMouseOut = FALSE) %>% 
            dyOptions(colors = RColorBrewer::brewer.pal(4, "Accent")) 
    }
    else{
        plot_data <- dygraph(train) %>% 
            dySeries(label = "Train Data", color='steelblue') %>% 
            dyHighlight(highlightSeriesOpts = list(strokeWidth = 2),
                        highlightCircleSize = 4,
                        highlightSeriesBackgroundAlpha = 0.2,
                        hideOnMouseOut = FALSE) %>%
            dyLegend(show = "always", hideOnMouseOut = FALSE) %>% 
            dyOptions(colors = RColorBrewer::brewer.pal(4, "Accent")) 
    }
        
        
    
    
    
    return(list(train=train,
                test=test,
                plot=plot_data))
}
