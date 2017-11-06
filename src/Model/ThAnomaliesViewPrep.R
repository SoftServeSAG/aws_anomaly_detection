TSThresholdsAnomaliesVis <- function (ts.agg, thresholds=NULL, anomalies=NULL)
{
  # Prepare xts object for visualization
  #
  # ts.agg - initial time series xts + agg parameters
  # thresholds - detected dynamic thresholds
  # anomalies - detected anomalies
  #
  # return:
  # xts: values, thresholds, anomalies
  
  
  ts.data = timeseries_test(data.agg = ts.agg$data.agg, ts_type = ts.agg$ts_type, ts_val = ts.agg$ts_val)
  data=ts.data$ts
  
  shapes=NULL
  x=NULL
  y=NULL
  
  if (!is.null(thresholds)) {
    x=data$time[c(thresholds$Minute_Low, max(thresholds$Minute_High))]
    x[is.na(x)]=max(data$time)
    y=c(thresholds$Threshold, thresholds$Threshold[nrow(thresholds)])
    
    xx=seq(thresholds$Minute_Low[1], thresholds$Minute_High[length(thresholds$Minute_High)])
    xx=data$time[xx]
    xx[is.na(xx)]=max(data$time)
    xx=xx[1: which(xx==max(xx))[1]]
    th_df = data.frame(x=xx, y=NA)
    th_df$y[th_df$x %in% x]=y
    th_df$y=na.locf(th_df$y)
    
    colnames(ts.agg$data.agg)='values'
    ts.agg$data.agg$thresolds = th_df$y
  }
  if (!is.null(anomalies)) {
    
    ts.agg$data.agg$anomalies = NA
    ts.agg$data.agg$anomalies[anomalies] = ts.agg$data.agg$values[anomalies]

  }  
  
  return(ts.agg$data.agg)
  
}

TSThresholdsAnomaliesVis_both <- function (ts.agg, thresholds=NULL, anomalies=NULL)
{
  # Prepare xts object for visualization
  #
  # data - initial time series (vector)
  # thresholds - list of High and Low thresholds
  # anomalies - High and Low Anomalies
  #
  # return:
  # xts: values, thresholds.l, thrasholds.h, anomalies
  
  
  shapes=NULL
  x=NULL
  y=NULL
  
  ts.data = timeseries_test(data.agg = ts.agg$data.agg, ts_type = ts.agg$ts_type, ts_val = ts.agg$ts_val)
  data=ts.data$ts
  
  if (!is.null(thresholds)) {
    x.L=data$time[c(thresholds[[1]]$Minute_Low, max(thresholds[[1]]$Minute_High))]
    x.L[is.na(x.L)]=max(data$time)
    y.L=c(thresholds[[1]]$Threshold, thresholds[[1]]$Threshold[nrow(thresholds[[1]])])
    
    x.H=data$time[c(thresholds[[2]]$Minute_Low, max(thresholds[[2]]$Minute_High))]
    x.H[is.na(x.H)]=max(data$time)
    y.H=c(thresholds[[2]]$Threshold, thresholds[[2]]$Threshold[nrow(thresholds[[2]])])
    
    TR=merge(thresholds[[1]],thresholds[[2]], by=c("Minute_Low"), all = T)
    names(TR)=c("Minute_Low","Minute_High.L","Threshold.L","Day.L","Len.L",
                "Minute_High.H","Threshold.H","Day.H","Len.H")
    
    TR$Minute_High.L=na.locf(TR$Minute_High.L)
    TR$Minute_High.H=na.locf(TR$Minute_High.H)
    TR$Threshold.H=na.locf(TR$Threshold.H)
    TR$Threshold.L=na.locf(TR$Threshold.L)
    
    TR$Minute_High = apply(TR, 1, function(x){min(x[2], x[6])})
    
    xx=seq(TR$Minute_Low[1], TR$Minute_High[length(TR$Minute_High)])
    xx=data$time[xx]
    xx[is.na(xx)]=max(data$time)
    xx=xx[1: which(xx==max(xx))[1]]
    th_df = data.frame(x=xx, l=NA, h=NA)
    th_df$l[th_df$x %in% x.L]=y.L
    th_df$l=na.locf(th_df$l)
    
    th_df$h[th_df$x %in% x.H]=y.H
    th_df$h=na.locf(th_df$h)
    
    colnames(ts.agg$data.agg)='values'
    ts.agg$data.agg$thresolds.l = th_df$l
    ts.agg$data.agg$thresolds.h = th_df$h
  }
  
  if (!is.null(anomalies)) 
  {
    ts.agg$data.agg$anomalies = NA
    ts.agg$data.agg$anomalies[anomalies] = ts.agg$data.agg$values[anomalies]
  }
  
  
  return(ts.agg$data.agg)
  
}