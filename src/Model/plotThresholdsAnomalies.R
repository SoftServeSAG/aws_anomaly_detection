plotTSThresholdsAnomalies <- function (data, thresholds=NULL, anomalies=NULL)
{
  # Plot timeserias with thresholds ans anomalies
  #
  # data - initial time series (vector)
  # thresholds - detected dynamic thresholds
  # anomalies - detected anomalies
  #

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
                       y0 = min(data.agg$values), y1 = thresholds$Threshold[i], yref = "y")
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