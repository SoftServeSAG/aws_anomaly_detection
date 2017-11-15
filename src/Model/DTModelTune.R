dynamicThreshold.autoturn <- function ( ts.agg,
                                        model,
                                        type_th = c("Low", "High", "Both")[3],
                                        p_anomalies = 0.01)
    # Anomalies detection (Dynamic Thershold model) tuning  - allows to detect correction coefficient based on 
    # High thresolds of anomalies percent 
    # 
    # 
    # inputs:
    # ts.agg - aggregated data list:
    #         (data.agg - aggregated time series - xts format
    #         ts_type, ts_val - aggregation parameters) ]
    # model - anomaly detection model
    # type_th - type of thresholds
    # p_anomalies = High thresolds of anomalies percent (0,1)  0.01 - default
    #
    # return : 
    #         result of correction coefficients for both types of anomalies:
    #           list("Low" = c(coef, scale), "High"=c(coef, scale)):
    #
{
    ts.agg$data.agg[,1][is.na(ts.agg$data.agg[,1])]=median(ts.agg$data.agg[,1], na.rm = T)
    coeff1 = -c(-10:10)/10
    coeff2 = c(-10:10)/10
    sc1=(c(10:20)/10)^2
    sc2=(c(10:20)/10)^2
    
    r1=c(0,1) 
    r2=c(0,1)
    
    if (type_th %in% c("Low", "Both"))
    {
        grids1 = list()
        k=1
        for (c1 in coeff1)
            for (s1 in sc1)
            {
                RES = dynamicThreshold.apply(ts.agg = ts.agg,
                                             model = model,
                                             type_th = "Low", 
                                             correction =list("Low" = c(coef = c1, scale = s1)))
                
                grids1[[k]]=c(sum(!is.na(RES$anomalies))/length(RES$anomalies), c1,s1)
                k=k+1
                print (k)
            }  
        dd=unlist(lapply(grids1, function(x){x[1]}))
        V=sort(dd[which(dd>=p_anomalies)])[1]
        if (is.na(V))
            V=max(dd)
        r1 = grids1[which(dd==V)[1]][[1]][2:3]
    }
   
    
    if (type_th %in% c("High", "Both"))
    {
        grids2 = list()
        k=1
        for (c1 in coeff2)
            for (s1 in sc2)
            {
                
                RES = dynamicThreshold.apply(ts.agg = ts.agg,
                                             model = model,
                                             type_th = "High", 
                                             correction =list("High" = c(coef = c1, scale = s1)))
                
                grids2[[k]]=c(sum(!is.na(RES$anomalies))/length(RES$anomalies), c1,s1)
                k=k+1
                print (k)
            }
        
        
        
        dd=unlist(lapply(grids2, function(x){x[1]}))
        V=sort(dd[which(dd>=p_anomalies)])[1]
        if (is.na(V))
            V=max(dd)
        r2 = grids2[which(dd==V)[1]][[1]][2:3]
    }
    
    
    return(list("Low" = r1, "High"=r2))
}