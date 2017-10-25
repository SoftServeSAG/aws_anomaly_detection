dynamicThreshold.smart_train <- function (ts.agg, 
                                          type_th = c("high","low")[1],
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
                                          ))

# Dynamic Threshold model training wrapper - allows to traing model both for "simple" and "expert" modes
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
#         model = DT Model, 
#         timeseries = time series in appropriate format for firther analysis, 
#         raw_timeseries  - time series in initial (xts) format, 
#         ad_results  - anomaly detection results
#
{
  
  if (type_th=="high")
  {
    ts_corr=NULL
    ts_train = timeseries_train(data.agg = ts.agg$data.agg, 
                                ts_type = ts.agg$ts_type, 
                                ts_val = ts.agg$ts_val)
  }else
  {
    
    ts_corr = mean(range(as.numeric(ts.agg$data.agg)))
    data.agg_new =  2*ts_corr -ts.agg$data.agg
    ts_train = timeseries_train(data.agg = data.agg_new, 
                                ts_type = ts.agg$ts_type, 
                                ts_val = ts.agg$ts_val)
  }
  
  if (is.null(periods))
    periods=sort(ts_train$periods)
  else
  {
    periods = sort(periods)   
    if (length(periods)==1)
    {
      periods=c(periods, nrow(ts_train$time_series$ts))
    } 
  }
  
  
  ts_test = timeseries_test(data.agg = ts.agg$data.agg, 
                            ts_type = ts.agg$ts_type, 
                            ts_val = ts.agg$ts_val)
  
  
  if (train.params$mode == 'simple')
  {
    params = 
      switch(train.params$sensitivity,
             'High' = list
             (
               agg_th = 0.5, 
               local_trend = 0.1, 
               similar = 0.01
             ),
             'Medium' = list
             (
               agg_th = 0.6, 
               local_trend = 0.3, 
               similar = 0.1
             ),
             'Low' = list
             (
               agg_th = 0.8, 
               local_trend = 0.5, 
               similar = 0.3
             )
           )
  }else{
    params = train.params$params
  }
  
    
  model.DT<-dynamicThreshold.model(ts_train$time_series,
                                   period = periods,
                                   agg_th = params$agg_th, # the higher the more tolerant
                                   local_trend = params$local_trend, # local trend is determined based on quantile of corresponded preriods
                                   k = 0.1,
                                   similar = params$similar, #significance in the divergence between thresholds of the near diapasons
                                   identical_thresholds = rep(FALSE, length(ts_train$periods)))
  model.DT$ts_corr = ts_corr
  model.DT$type_th = type_th
    
  results=find.anomalies(ts.agg, ad.model = model.DT)
  
  #, anomalies=results$anomalies, th_plot = results$th_plot
  
  return(list(model = model.DT, timeseries = ts_test, raw_timeseries = ts.agg, ad_results = results))
  
}