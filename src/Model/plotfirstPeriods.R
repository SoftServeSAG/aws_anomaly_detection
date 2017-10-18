plotfirstPeriods <- function (data, periods, N=40, anomalies=NULL)
{
  # Plot first N periods
  #
  # data - initial time series (vector)
  # periods - detected periods
  # N - number of the first plottrd periods
  # anomalies - detected anomalies
  #
  for (i in (1:length(periods))) {
    dst=min(N*periods[i], (nrow(data)))
    plot_data=data[1:dst,]
    sel_anomalies = anomalies[anomalies <= dst]
    x_abline = plot_data$time[diff(floor((1:dst)/periods[i]))==1]
    y_abline_max = rep(max(plot_data$values),length(x_abline))
    y_abline_min = rep(min(plot_data$values),length(x_abline))
    x_abline_NA = plot_data$time[which(diff(floor((1:dst)/periods[i]))==1)+1]
    y_abline_NA = rep(NA,length(x_abline))
    ord=order(c(x_abline,x_abline, x_abline_NA))
    x=c(x_abline,x_abline, x_abline_NA)[ord]
    y=c(y_abline_min, y_abline_max, y_abline_NA)[ord]
    an_time=data$time[sel_anomalies]
    an_values = data$values[sel_anomalies]
    if (!is.null(anomalies))
    {
      print(
        plot_data %>% 
          plot_ly(x=~time, y=~values) %>% add_lines(name="data") %>%
          add_lines(x=x,y=y, name="periods") %>%
          add_markers(x=an_time,y=an_values, name="anomalies", 
                      marker = list(size = 5,color = 'red')) %>% 
          layout(title=paste("Period =", periods[i], ts_type))  
      )
    }else{
      print(
        plot_data %>% 
          plot_ly(x=~time, y=~values) %>% add_lines(name="data") %>%
          add_lines(x=x,y=y, name="periods") %>%
          layout(title=paste("Period =", periods[i], ts_type)))  
    }
   
  }
}

#test
#
# data.agg=readRDS("data/data_agg.rds")
# periods=readRDS("data/periods.rds")
# anomalies=readRDS("data/anomalies.rds")
#
# plotfirstPeriods(data = data.agg, periods = periods, anomalies = model.DT$anomalies)
