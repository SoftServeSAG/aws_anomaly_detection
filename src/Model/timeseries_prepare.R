timeseries_train <- function (data.agg, ts_type, ts_val)
  # Prepare data for model Training
  #
  # data.agg - time series in xts format
  # ts_type, ts_val - aggregation parameters
  #
  # return:
  #
  # time_series = list( ts - timeseries as dataframe, 
  #                     ts_rec = reconstracted time series , 
  #                     ssa - SSA decomposition of time series, 
  #                     ts_type, ts_val - aggregation parameters)
  # periods - extracted periods
{
  #prepare dataframe
  names(data.agg)=c("values")
  ts.datetime = index(data.agg)
  data.agg=as.data.frame(data.agg)
  data.agg$time=ts.datetime
  
  #extract periods form timeseries
  periods_ssa=findPeriod_ssa(data.agg$values)
  periods=periods_ssa$periods
  if (length(periods)==0)
      periods = max(floor(length(data.agg$values)/2),2)
  SSA=periods_ssa$ssa
  
  #time series reconstruction
  data.reconstructed=app_timeseries(SSA, dsigma = 0.1)
  
  # DT model training
  if (length(periods)==1)
  {
    periods=c(periods, nrow(data.agg))
  }
  
  time_series = list(ts = data.agg, ts_rec = data.reconstructed, ssa=SSA, ts_type=ts_type, ts_val=ts_val)
  
  return(list(time_series=time_series, periods = periods))
}


timeseries_test <- function (data.agg, ts_type, ts_val)
  #
  # Prepare data for model Annpying
  #
  # data.agg - time series in xts format
  # ts_type, ts_val - aggregation parameters
  #
  # return:
  #
  # time_series = list( ts - timeseries as dataframe, 
  #                     ts_par - aggregation parameters)

{
  #prepare dataframe
  names(data.agg)=c("values")
  ts.datetime = index(data.agg)
  data.agg=as.data.frame(data.agg)
  data.agg$time=ts.datetime
  
  
  time_series = list(ts = data.agg, ts_par=list(ts_type=ts_type, ts_val=ts_val))
  
  return(time_series)
}