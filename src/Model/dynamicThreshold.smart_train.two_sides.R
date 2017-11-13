dynamicThreshold.smart_train.two_sided <- function (ts.agg, 
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

{
  model.h=dynamicThreshold.smart_train(ts.agg  = ts.agg, 
                                       type_th = "high", 
                                       periods = periods,
                                       train.params=train.params)
  
  model.l=dynamicThreshold.smart_train(ts.agg  = ts.agg, 
                                          type_th = "low", 
                                       periods = periods,
                                       train.params=train.params)
  
  return(list(low =model.l$low, high = model.h$high))
  
}