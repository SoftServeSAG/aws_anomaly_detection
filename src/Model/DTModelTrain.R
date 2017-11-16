dynamicThreshold.train <- function (ts.agg, 
                          periods=NULL,
                          train.params = list(
                              mode = c("expert", "simple")[1],
                              sensitivity = c('High', 'Medium','Low')[2],
                              params = list
                              (
                                  agg_th = 0.75,
                                  local_trend = 0.5,
                                  similar = 0.1
                              )
                          ), type_th = c("Low", "High", "Both")[3])

# Dynamic Threshold model training wrapper - allows to traing model both for "simple" and "expert" modes 
# for all anomalies types
# 
# inputs:
# ts.agg - aggregated data list:
#         (data.agg - aggregated time series - xts format
#         ts_type, ts_val - aggregation parameters) 
# type_th - type of thresholds
# periods - set of analyzed periods, if NULL - periods will be detected automatically
# train_params -  traioning parameters:
#                 mode - simple or expert
#                 sensitivity - High, Medium, Low - for simple mode only
#                 params - for expert mode
#
# return : 
#         model = DT Model:
#                 list( thresholds - thresolds for shortest period, 
#                       levels - local trends based on all periods,  
#                       period = set of periods, 
#                       max = maximal observed value in train set , 
#                       initial_time -  from train set, 
#                       ts_type - aggregation units,
#                       ts_val - aggregation step,
#                       ts_corr - correction for low-thresholds only,
#                       type_th - thersholds type)  
#         timeseries = time series in appropriate format for firther analysis, 
#         raw_timeseries  - time series in initial (xts) format, 
#         ad_results  - anomaly detection results
#
{
    if (length(ts.agg$data.agg)<3)
    {
        stop("Too small timeseries")
    }
    ts.agg$data.agg[,1][is.na(ts.agg$data.agg[,1])]=median(ts.agg$data.agg[,1], na.rm = T)
    model=NA
    if (type_th %in% c("Low", "High"))
    {
        model = dynamicThreshold.smart_train(ts.agg  = ts.agg, 
                                             periods = periods,
                                             type_th = tolower(type_th), 
                                             train.params = train.params)
    }
    if (type_th =="Both"){
        model = dynamicThreshold.smart_train.two_sided(ts.agg  = ts.agg, 
                                                       periods = periods,
                                                       train.params = train.params)
    }
    
    return(model)
    
}