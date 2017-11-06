find.anomalies.two_sides <- function (ts.agg,
                                      low = list(model = NULL, coef = 0, scale=1), 
                                      high = list(model = NULL, coef = 0, scale=1))
  # two-sides anomalies analysis
  # input:
  #       ts.agg - timeseries data in xts fornat with aggregation parameters 
  #                data.agg, ts_type, ts_val
  # low - model: AD model for low thresholds:
  #                   list( thresholds - thresolds for shortest period, 
  #                         levels - local trends based on all periods,  
  #                         period = set of periods, 
  #                         max = maximal observed value in train set , 
  #                         initial_time -  from train set, 
  #                         ts_type - aggregation units,
  #                         ts_val - aggregation step,
  #                        type_th - thresholds type - Low or High)
  #
  #         coef - (default = 0) additive threshold correction coefficient 
  #         scale - (default 1) threshold scale coefficient 
  # high - model and coefficients for high-thresholds
  # 
  # return:
  # anomalies - indexes of anomalies observations
  # expected - expected metric values for anomaly cases
  # observed - observed  values for anomaly cases 
  # significance - significance of anomalies
  # type - type of anomalies
  # thresholds - thresholds representation for visualization
{
  RES.h=find.anomalies(ts.agg = ts.agg,
                       ad.model = high$model, 
                       coef=high$coef, scale=high$scale)
  
  RES.l=find.anomalies(ts.agg = ts.agg,
                       ad.model = low$model, 
                       coef=low$coef, scale=low$scale)
  
  return(list(thresholds = list(RES.l$thresholds, RES.h$thresholds),
              anomalies = c(RES.l$anomalies, RES.h$anomalies),
              observed = c(RES.l$observed,RES.h$observed),
              expected = c(RES.l$expected,RES.h$expected),
              significance = as.factor(c(as.character(RES.l$significance),as.character(RES.h$significance))),
              type = c(rep("low", length(RES.l$anomalies)),rep("high", length(RES.h$anomalies)))
              ))
}






