anomalies.stat <- function(AD_res, ts.agg)
{
  # Anomalies Summary
  #
  # inputs AD_result, initial time-series
  #
  # output - dataframe with anomalies
  # anomalies list
  # significance statistic
  # signigficance statistic barplot
  
  ad_res = data.frame(start = index(ts.agg$data.agg)[AD_res$anomalies], 
                      end = index(ts.agg$data.agg)[AD_res$anomalies] - as.difftime(ts.agg$ts_val, units=ts.agg$ts_type),
                      observed = AD_res$observed,
                      expected=AD_res$expected,significance = AD_res$significance)
  sign_stat= as.data.frame(round(prop.table(table(ad_res$significance))*100,2))
  names(sign_stat)=c("significance", "count")
  
  plt <- sign_stat %>% plot_ly(x=~significance, y=~count) %>% add_bars()
  
  return(list(ad_res = ad_res, sign_stat=sign_stat, sign_stat_plt = plt))
  
}