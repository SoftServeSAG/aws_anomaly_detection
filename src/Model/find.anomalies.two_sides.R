find.anomalies.two_sides <- function (ts.agg,
                                      low = list(model = NULL, coef = 0, scale=1), 
                                      high = list(model = NULL, coef = 0, scale=1))
{
  RES.h=find.anomalies(ts.agg = ts.agg,
                       ad.model = high$model, 
                       coef=high$coef, scale=high$scale)
  
  RES.l=find.anomalies(ts.agg = ts.agg,
                       ad.model = low$model, 
                       coef=low$coef, scale=low$scale)
  
  return(list(thresholds = list(RES.l$th_plot, RES.h$th_plot),
              anomalies = c(RES.l$anomalies, RES.h$anomalies)))
}