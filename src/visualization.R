require(xts)
require(lubridate)
require(dplyr)
require(dygraphs)


#Data Preprocessing Functionality: Data Visualization

plot_time_series <- function(data, treshhold_type='all', window_size=0.75){
    # Function plot time-series data
    #
    # Input:
    # data - xts object with time-series data
    # treshhold_type - type of treshholds (all, none, low or high)
    # window_size - size of plot window on bottom 
    #
    # Output:
    # dygraphs plot of time series
    
    
    # Change type of input data to xts
    if (!is.xts(data)){
        data <- as.xts(data)
    }
    
    #define window for plot and prepare xts
    window_data <- index(data)[c(window_size, 1) * length(index(data)) %>% as.integer()]
    names(data)[1] <- "values"
    
    # define dygraph plot
    plot_data <- dygraph(data, main = "Time series", 
                         ylab = "Values",
                         xlab = "Time")
    
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

        plot_data <- plot_data %>%  
            dySeries("thresolds", label = "High Thresolds", color='blue', fillGraph = T) %>% 
            dySeries("values", label = "Historical Data",
                     color='blue') %>% 
            dySeries("anomalies", label = "Anomalies", color = 'red', drawPoints = TRUE, pointSize = 3,
                     strokeBorderColor='blue', strokeWidth=0)
        
        
    }else if ((dim(data)[2] == 3) & (treshhold_type == 'low')){
        
        data$max_values <- max(data$values) * 1.01
        
        plot_data <- dygraph(data, main = "Time series", 
                             ylab = "Values",
                             xlab = "Time") %>% 
            dySeries("thresolds", label = "Low Thresolds", color='blue') %>% 
            dySeries(c("thresolds","values","max_values"), label = "Historical Data",
                     color='blue') %>% 
            dySeries("anomalies", label = "Anomalies", color = 'red', drawPoints = TRUE, pointSize = 3,
                     strokeBorderColor='blue', strokeWidth=0)
        
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

# # testing 1 column
# 
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
# plot_time_series(data)
# 
# # testing 4 column
# 
# HH_data <- readRDS("data/household_power_consumption_datatime.rds")
# 
# #create time series
# data <- xts(x = HH_data$Voltage, order.by = HH_data$datetime)
# 
# #remove NA
# data_na.rm=remove_na_from_data(data, type = "mean")
# 
# #aggregation
# ts_type="days"
# ts_val=1
# data.agg=aggregation_data(data_na.rm, type = paste(ts_val, ts_type), func_aggregate = 'median', quantile_percent = .5)
# 
# 
# 
# data.agg.train = data.agg[1:1200]
# data.agg.test = data.agg[1201:length(data.agg)]
# 
# 
# # Time Series for model Training and Applying 
# 
# ts.agg.train=list(data.agg=data.agg.train, ts_type=ts_type, ts_val=ts_val)
# ts.agg.test=list(data.agg=data.agg.test, ts_type=ts_type, ts_val=ts_val)
# ts.agg=list(data.agg=data.agg, ts_type=ts_type, ts_val=ts_val)
# 
# 
# #==== Train ====================================================================================================
# 
# #++++ Simple + Medium ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 
# my.model.h=dynamicThreshold.smart_train(ts.agg  = ts.agg.train,
#                                         type_th = "high",
#                                         train.params = list(mode = 'simple', sensitivity = 'Medium'))
# 
# my.model.l=dynamicThreshold.smart_train(ts.agg  = ts.agg.train,
#                                         type_th = "low",
#                                         train.params = list(mode = 'simple', sensitivity = 'Low'))
# 
# #==== Apply ====================================================================================================
# 
# 
# RES.h=find.anomalies(ts.agg  = ts.agg.test, 
#                      ad.model = my.model.h$model, 
#                      coef=0.0, scale=1)
# 
# RES.l=find.anomalies(ts.agg  = ts.agg.test, 
#                      ad.model = my.model.l$model, 
#                      coef=0.4, scale=1.2)
# 
# xts.vis = TSThresholdsAnomaliesVis_both(ts.agg = ts.agg.test, 
#                                         thresholds = list(RES.l$th_plot, RES.h$th_plot),
#                                         anomalies = c(RES.l$anomalies,RES.h$anomalies))
# plot_time_series(xts.vis)
# 
# 
# #testing 3 columns with High or Low treshholds
# 
# my.model.h=dynamicThreshold.smart_train(ts.agg  = ts.agg.train, 
#                                         type_th = "high", 
#                                         train.params = list(mode = 'simple', sensitivity = 'Medium'))
# 
# my.model.l=dynamicThreshold.smart_train(ts.agg  = ts.agg.train, 
#                                         type_th = "low", 
#                                         train.params = list(mode = 'simple', sensitivity = 'Low'))
# 
# 
# xts.vis = TSThresholdsAnomaliesVis(ts.agg=my.model.h$raw_timeseries, 
#                                    thresholds = my.model.h$ad_results$th_plot, 
#                                    anomalies = my.model.h$ad_results$anomalies)
# 
# plot_time_series(xts.vis, treshhold_type = 'high')
# 
# xts.vis = TSThresholdsAnomaliesVis(ts.agg=my.model.l$raw_timeseries, 
#                                    thresholds = my.model.l$ad_results$th_plot, 
#                                    anomalies = my.model.l$ad_results$anomalies)
# 
# plot_time_series(xts.vis, treshhold_type = 'low')
