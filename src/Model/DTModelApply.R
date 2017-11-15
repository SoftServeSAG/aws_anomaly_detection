dynamicThreshold.apply <- function (ts.agg,
                                    model,
                                    type_th = c("Low", "High", "Both")[3],
                                    correction = list(Low=c(coef = 0, scale=1), High=c(coef = 0, scale=1)))
    # Anomalies detection (Dynamic Thershold model) wrapper  - allows to detect low, high and both types of anomalies 
    # 
    # 
    # inputs:
    # ts.agg - aggregated data list:
    #         (data.agg - aggregated time series - xts format
    #         ts_type, ts_val - aggregation parameters) ]
    # model - anomaly detection model
    # type_th - type of thresholds
    # correction - list of correction coefficients: 
    #           coef - (default = 0) additive threshold correction coefficient 
    #           scale - (default 1) threshold scale coefficient 
    #
    # return : 
    #         result of anomaly detection - xts object:
    #                                       (values, thresolds/(thresolds.l,thresolds.h), anomalies)
    #
{
    ts.agg$data.agg[,1][is.na(ts.agg$data.agg[,1])]=median(ts.agg$data.agg[,1], na.rm = T)
    res=NA
    if ((type_th %in% c("Low", "High")) & tolower(type_th) %in% names(model))
    {
        res=find.anomalies(ts.agg = ts.agg,
                           model[[tolower(type_th)]]$model, 
                           coef = correction[[type_th]][1], scale = correction[[type_th]][2])
        res=TSThresholdsAnomaliesVis(ts.agg=ts.agg, 
                                     thresholds = res$thresholds,
                                     anomalies = res$anomalies)
        #colnames(res)[2]=paste(colnames(res)[2],tolower(substr(type_th,1,1)), sep='.')
    }
    if ((type_th == "Both") & (length(model)==2))
    {
        res=find.anomalies.two_sides(ts.agg = ts.agg,
                                 low = list(model = model$low$model, coef = correction[["Low"]][1], scale = correction[["Low"]][2]),
                                 high = list(model = model$high$model, coef = correction[["High"]][1], scale = correction[["High"]][2]))
        res=TSThresholdsAnomaliesVis_both(ts.agg=ts.agg, 
                                 thresholds = res$thresholds,
                                 anomalies = res$anomalies)
    }
    return(res)
}