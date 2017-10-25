plotTSThresholdsAnomalies <- function (ts.agg, type_th = c("high","low")[1],
                                       thresholds=NULL, anomalies=NULL)
{
  # Plot timeserias with thresholds ans anomalies
  #
  # ts.agg - initial time series xts + agg parameters
  # type_th - type of anomalies (high or low)
  # thresholds - detected dynamic thresholds
  # anomalies - detected anomalies
  #
  
  
  ts.data = timeseries_test(data.agg = ts.agg$data.agg, ts_type = ts.agg$ts_type, ts_val = ts.agg$ts_val)
  data=ts.data$ts
  
  shapes=NULL
  x=NULL
  y=NULL
  
  if (!is.null(thresholds)) {
    x=data$time[c(thresholds$Minute_Low, max(thresholds$Minute_High))]
    x[is.na(x)]=max(data$time)
    y=c(thresholds$Threshold, thresholds$Threshold[nrow(thresholds)])
    shapes=list()
    for (i in (1:nrow(thresholds)))
    {
      shapes[[i]]=list(type = "rect",
                       fillcolor = "blue", line = list(color = "blue"), opacity = 0.1,
                       x0 = data$time[min(thresholds$Minute_Low[i], nrow(data))], 
                       x1 = data$time[min(thresholds$Minute_High[i], nrow(data))], 
                       xref="x",
                       y0 = ifelse(type_th=="high",min(data$values), max(data$values)),
                       y1 = thresholds$Threshold[i], yref = "y")
    }
  }
  
  if (!is.null(anomalies)) 
  {
    an_time=data$time[anomalies]
    an_values = data$values[anomalies]
  }
  
  
  
  
  p <- plot_ly(data, x=~time, y=~values)
  p <- add_lines(p, name="data") 
  if (!is.null(thresholds)) 
    p <- add_lines(p, x=x,y=y, name="threshold", line = list(shape = "hv"))
  
  if (!is.null(anomalies)) 
    p <-  add_markers(p, x=an_time,y=an_values, name="anomalies", 
                      marker = list(size = 5,color = 'red'))
  
  p <- layout(p,title = 'Time Series Analysis',shapes = shapes)
  
  print(p)
  
}

plotTSThresholdsAnomalies_both <- function (ts.agg, thresholds=NULL, anomalies=NULL)
{
  # Plot timeserias with thresholds ans anomalies
  #
  # data - initial time series (vector)
  # thresholds - list of High and Low thresholds
  # anomalies - High and Low Anomalies
  #

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
    
    shapes=list()
    for (i in (1:nrow(TR)))
    {
      shapes[[i]]=list(type = "rect",
                       fillcolor = "blue", line = list(color = "blue"), opacity = 0.1,
                       x0 = data$time[min(TR$Minute_Low[i], nrow(data))], 
                       x1 = data$time[min(TR$Minute_High[i], nrow(data))], 
                       xref="x",
                       y0 = TR$Threshold.L[i],
                       y1 = TR$Threshold.H[i], yref = "y")
    }
  }
  
  if (!is.null(anomalies)) 
  {
    an_time=data$time[anomalies]
    an_values = data$values[anomalies]
  }
  
  p <- plot_ly(data, x=~time, y=~values)
  p <- add_lines(p, name="data") 
  if (!is.null(thresholds)) 
  {
        p <- add_lines(p, x=x.L,y=y.L, name="threshold_low", line = list(shape = "hv"))
        p <- add_lines(p, x=x.H,y=y.H, name="threshold_high", line = list(shape = "hv"))
  }
  
  if (!is.null(anomalies)) 
    p <-  add_markers(p, x=an_time,y=an_values, name="anomalies", 
                      marker = list(size = 5,color = 'red'))
 
  p <- layout(p,title = 'Time Series Analysis',shapes = shapes)
  
  print(p)
  
}