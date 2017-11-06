anomalies.analysis <- function(ts.agg, ad_results, k=1)
  
  # Analysis of Anomalies Cases
  # input:
  # ts.agg = list(
  #                data.agg - aggregated time series in xts format,
  #                ts_type, ts_val, ts_func - aggregation parameters
  #             )
  # ad_results - results of anomaly detection:
  #      anomalies - indexes of anomalies observations
  #      expected - expected metric values for anomaly cases
  #      observed - observed  values for anomaly cases 
  #      significance - significance of anomalies
  # k - index of anomaly
{
  ad_case =  ts.agg$data.agg[ad_results$anomalies[k]]
  expectedV = ad_results$expected[k]
  observedV = ad_results$observed[k]
  
  ad_case_high=index(ad_case)
  ad_case_low = ad_case_high - as.difftime(ts.agg$ts_val, units=ts.agg$ts_type)
  
  data_anomaly = data[paste(ad_case_low,ad_case_high,sep="/")]
  names(data_anomaly)="values"
  data_anomaly$statistics = observedV
  data_anomaly$expected = expectedV
  
  dt_anomaly = as.data.frame(data_anomaly)
  dt_anomaly$datatime = index(data_anomaly)
  
  if (!(ts.agg$ts_func %in% c("sum")))
    plt <- dt_anomaly %>% plot_ly(x=~datatime, y=~values) %>% 
          add_lines(name="values", line = list(color = 'steelblue', width = 1), opacity = 0.3) %>%
          add_lines(x=~datatime, y=~expectedV, name ="expected metric" ) %>%
          add_lines(x=~datatime, y=~observedV, name ="observed metric" ) %>%
          layout(title = paste(paste('Type of Anomalies:',ad_results$type[k]), 
                         paste('Significance:',ad_results$significance[k]), sep="\n"))
  else
    plt <- dt_anomaly %>% plot_ly(x=~datatime, y=~values) %>% 
    add_lines(name="values", line = list(color = 'steelblue', width = 1), opacity = 0.3) %>%
    layout(title = paste(paste('Type of Anomalies:',ad_results$type[k]), 
                         paste('Significance:',ad_results$significance[k]), sep="\n"))

  return(list(row_data.anomaly = dt_anomaly, row_data.anomaly.plot = plt))

  
}
