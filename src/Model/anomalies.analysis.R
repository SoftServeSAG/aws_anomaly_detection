anomalies.stat <- function(ad_results, data, ts_type, ts_val)
{
    # Anomalies Summary
    #
    # inputs ad_results - anomalies detection results in xts format, 
    #        data - raw time-series, 
    #        ts_type, ts_val - aggragation parameters like ts_type="days" ts_val=1
    #
    # output - list(ad_res = ad_res - dataframe with format:
    #                                   ( end = end of anomaly case, 
    #                                     start = start of anomaly case,
    #                                     observed = statistic that was observed,
    #                                     expected=statistic trat was expected,
    #                                     significance = anomaly significance - low, medium, high)
    #               sign_stat - frequency histogram of anomalies significance as dataframe, 
    #               sign_stat_plt - frequency histogram of anomalies significance as plot))
    #
    
    indx = which(!is.na(ad_results$anomalies))
    if (length(indx)==0){
      return(list(ad_res = NULL, sign_stat=NULL, sign_stat_plt = NULL))  
    }
    observedV = as.numeric(ad_results$values)
    if (length(colnames(ad_results))>3){
        expectedV = apply(coredata(ad_results), 1, 
                          function(x)
                              {
                              if (x[2]>x[1])
                                  return(x[2])
                              else
                                  return(x[3])
                          })
        
    }else{
        expectedV = as.numeric(ad_results$thresolds) 
    }
    observedV=observedV[indx]
    expectedV = expectedV[indx]
    obs_dif = abs(expectedV - observedV)
    significance = cut(obs_dif, breaks=3, labels = c("Low", "Medium", "High"), include.lowest = T)
    
    ad_res = data.frame(end = index(ad_results)[indx], 
                        start = index(ad_results)[indx] - as.difftime(ts_val, units=ts_type),
                        observed = observedV,
                        expected=expectedV,
                        significance = significance)
    sign_stat= as.data.frame(round(prop.table(table(ad_res$significance))*100,2))
    names(sign_stat)=c("significance", "count")
    
    plt <- sign_stat %>% plot_ly(x=~significance, y=~count) %>% add_bars()
    
    return(list(ad_res = ad_res, sign_stat=sign_stat, sign_stat_plt = plt))
    
}


anomalies.detail <- function(anomaly, data, ts_func)
    
    # Analysis of Anomalies Cases
    # input:
    # anomaly = anomaly case as dataframe:
    #                                   ( end = end of anomaly case, 
    #                                     start = start of anomaly case,
    #                                     observed = statistic that was observed,
    #                                     expected=statistic trat was expected,
    #                                     significance = anomaly significance - low, medium, high)
    # data - raw time-series, 
    # ts_func - aggragation function
    #
{
    
    data_anomaly = data[paste(anomaly$start,anomaly$end,sep="/"),]
    names(data_anomaly)="values"
    data_anomaly$observedV = as.numeric(anomaly$observed)
    data_anomaly$expectedV = as.numeric(anomaly$expected)
    
    dt_anomaly = as.data.frame(data_anomaly)
    dt_anomaly$datatime = index(data_anomaly)
    
    type_anomaly = ifelse(as.numeric(anomaly$observed)>as.numeric(anomaly$expected), "High", "Low")
    
    if (!(ts_func %in% c("sum")))
        plt <- dt_anomaly %>% plot_ly(x=~datatime, y=~values) %>% 
        add_lines(name="values", line = list(color = 'steelblue', width = 1), opacity = 0.3) %>%
        add_lines(x=~datatime, y=~expectedV, name ="expected metric" ) %>%
        add_lines(x=~datatime, y=~observedV, name ="observed metric" ) %>%
        layout(title = paste(paste('Type of Anomalies:',type_anomaly), 
                             paste('Significance:',anomaly$significance), sep="\n"))
    else
        plt <- dt_anomaly %>% plot_ly(x=~datatime, y=~values) %>% 
        add_lines(name="values", line = list(color = 'steelblue', width = 1), opacity = 0.3) %>%
        layout(title = paste(paste('Type of Anomalies:',type_anomaly), 
                             paste('Significance:',anomaly$significance), sep="\n"))
    
    return(list(row_data.anomaly = dt_anomaly, row_data.anomaly.plot = plt))
    
    
}