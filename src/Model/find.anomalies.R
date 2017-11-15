find.anomalies <- function (ts.agg,
                            ad.model, coef = 0, scale=1)
  #Detect anomalies based on developed AD model
  #
  # ts.agg - timeseries data in xts fornat with aggregation parameters 
  # data.agg, ts_type, ts_val
  # ad.model - AD model :
  # list( thresholds - thresolds for shortest period, 
  #       levels - local trends based on all periods,  
  #       period = set of periods, 
  #       max = maximal observed value in train set , 
  #       initial_time -  from train set, 
  #       ts_type - aggregation units,
  #       ts_val - aggregation step,
  #       type_th - thresholds type - Low or High)
  #
  # coef - (default = 0) additive threshold correction coefficient 
  # scale - (default 1) threshold scale coefficient 
  # 
  # return:
  # anomalies - indexes of anomalies observations
  # expected - expected metric values for anomaly cases
  # observed - observed  values for anomaly cases 
  # significance - significance of anomalies
  # type - type of anomalies
  # thresholds - thresholds representation for visualization
  
{
 
  options(warn=-1)
  
  ts_data = timeseries_test(data.agg = ts.agg$data.agg, ts_type = ts.agg$ts_type, ts_val = ts.agg$ts_val)
  data=ts_data$ts
  ts_par=list(ts_type = ts.agg$ts_type, ts_val = ts.agg$ts_val)
  
  if (!is.null(ad.model$ts_corr))
  {
    data$values = 2*ad.model$ts_corr - data$values
    coef = -coef
  }
  
  if (!is.null(ts_par))
  {
    if ((ad.model$ts_type != ts_par$ts_type) |  (ad.model$ts_val != ts_par$ts_val))
      stop("Time series parameters don't match Model ones")
  }
  metric=data$values
  
  time=difftime(ad.model$initial_time,data$time[1], units = ad.model$ts_type)
  time=round(abs(as.numeric(time)/ad.model$ts_val)+1)
  
  
  Levels=ad.model$levels
  one_period <- list(thresholds = ad.model$thresholds, levels = ad.model$levels[[1]])
  period = ad.model$period
  
  
  #Determining anomalies in train data
  dts <- one_period$thresholds[rep(1:nrow(one_period$thresholds), (floor((length(metric)+time) / period[1])+1)),]
  dts$Day <- rep(1:(floor((length(metric)+time) / period[1])+1), each = nrow(one_period$thresholds))
  
  V=rep(rep(Levels[[1]], times = length(Levels[[2]])), each = nrow(one_period$thresholds))
  V=V[1:min(length(V), length(dts$Threshold))]
  dts$Threshold <- dts$Threshold * V
  
  for (lev2 in 2:length(period)) {
    V=rep(rep(Levels[[lev2]], each = period[lev2] / period[1]), each = nrow(one_period$thresholds))
    V=V[1:min(length(V), length(dts$Threshold))]
    dts$Threshold <- dts$Threshold * V
    
  }
  
  dts$Minute_Low <- dts$Minute_Low + (dts$Day - 1) * period[1]
  dts$Minute_High <- dts$Minute_High + (dts$Day - 1) * period[1]
  
  anomaly <- NULL
  
  dts$Len <- dts$Minute_High - dts$Minute_Low + 1
  dff=(ad.model$max-dts$Threshold)
  dff1=(dff-(dff*coef))
  dff2=dff1-mean(dff1)
  dff2=dff2*scale+mean(dff1)
  

  
  dts$Threshold = ad.model$max - dff2
  
  dts$Threshold[dts$Threshold>max(metric)]=max(metric)
  dts$Threshold[dts$Threshold<0]=0
  
  i1=max(which(dts$Minute_Low < time))
  if ((i1>0) & (dts$Minute_High[i1]-time)<0)
  {
      i1=min(which(dts$Minute_High > time)) 
  }
  if (i1>0)
  {
    v=dts$Minute_High[i1]-time
    dts$Minute_Low[i1]=time
    dts$Len[i1]=dts$Minute_High[i1]-dts$Minute_Low[i1]
    dts=dts[i1:nrow(dts),]
    dts$Minute_Low=dts$Minute_Low-time+1
    dts$Minute_High=dts$Minute_High-time+1
  }
  
  
  a<-rep(dts$Threshold, dts$Len)
  a=a[1:min(length(a), length(metric))]
  
  anomaly <- metric > a
  
  options(warn=0)
  anomalies = which(anomaly == TRUE)
  
  expected_val = a[anomalies]
  observed_val = metric[anomalies]
  vvv=NULL
  
  if (length(anomalies)>0){
      metr_diff = (metric - a)[anomalies]
      #signif = cut(metr_diff, breaks=quantile(metr_diff, seq(0,1,1/3)), labels = c("Low", "Medium", "High"), include.lowest = T)
      vvv = cut(metr_diff, breaks=3, labels = c("Low", "Medium", "High"), include.lowest = T)
      
  }
  
  if (!is.null(ad.model$ts_corr))
  {
    dts$Threshold = 2*ad.model$ts_corr - dts$Threshold
    expected_val = 2*ad.model$ts_corr - expected_val
    observed_val = 2*ad.model$ts_corr - observed_val
  }
  
  return(list(anomalies=anomalies, expected=expected_val, observed=observed_val, 
              significance = vvv, type = rep(ad.model$type_th, length(anomalies)), thresholds = dts))
}