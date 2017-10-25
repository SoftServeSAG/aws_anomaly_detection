dynamicThreshold.model <- function(time_series,
                          period = c(1440, 7*1440),
                          agg_th = 0.9,
                          local_trend = 0.9,
                          k = 0.1,
                          similar = 0.1,
                          plt = TRUE,
                          plt_period = NULL,
                          identical_thresholds = c(FALSE, FALSE)) 
  # Thresholds Determination
  #
  # data - timeseries data in dataframe format
  # metric_reconstructed - reconstructed timeseries with less noise. Could be computed with \code{\link{app_timeseries}}
  # period - (default c(1440, 7*1440)) vector of periods of time series with minimum two elements.
  # agg_th - (default 0.9) parameter that determines the aggregated value of the thresholds for the same diapasons of the 
  # different periods. N% quantile of the threshold values
  # local_trend - (default 0.9) local trend id determined as N% quantile of time series values for the corresponded period
  # k - (default 0.1) vector of indicates how short time-slots could be. If time-slot is less than k*period, it will be concatenated with time-slot with nearest threshold
  # similar - (default 0.1) coefficient in range [0, 1] which indicates how similar neighboring time-slots' thresholds have to be to concatenate them. So, if fraction of two neighboring time-slots is < similar -> they will be concatenated in one group
  # corrected_by - (default c(1, 1)) vector of coefficients of the same length as period vector which is used to increase or decrease thresholds of different levels. Could be used to tune model
  # identical_thresholds - (default c(FALSE, FALSE)) indication if thresholds should be the same for periods of different levels
  #
  # return:
  # AD model: 
  # list( thresholds - thresolds for shortest period, 
  #       levels - local trends based on all periods,  
  #       period = set of periods, 
  #       max = maximal observed value in train set , 
  #       initial_time -  from train set, 
  #       ts_type - aggregation units,
  #       ts_val - aggregation step)  
  #
  # anomalies - indexes of anomalies observations
  # th_plot - thresholds representation for visualization

{
  
  
  
        #Calculating sets of thresholds
        options(warn=-1)
  print ("Find Thresholds")
  data = time_series$ts
  metric_reconstructed = time_series$ts_rec
  metric=data$values
        thresholds <- find_Thresh_set(metric = metric,
                                      metric_reconstructed = metric_reconstructed,
                                      period = period[1],
                                      period_set = period[2],
                                      simil = 1,
                                      alpha = 0.05,
                                      k = 0,
                                      divide_min = FALSE,
                                      simil_measure = max)

        #Cutting out
       # metric <- metric[1:(floor(length(metric) / period[length(period)]) *  period[length(period)])]

        #Concatenate all sets
        all_thresholds <- data.frame(do.call(rbind, thresholds))

        #Picking together all possible time slots
        agg_thresholds <- data.frame(Minute_Low = sort(unique(all_thresholds$Minute_Low)),
                                     Minute_High = sort(unique(all_thresholds$Minute_High)))

        #Finding thresholds for all timeslots
        for (i in 1:nrow(agg_thresholds)) {
                a <- NULL
                a<- mclapply(thresholds,
                         FUN = function(x) {x$Threshold[(x$Minute_Low <= agg_thresholds$Minute_Low[i]) &
                                                                        (x$Minute_High >= agg_thresholds$Minute_High[i])]}, mc.cores = 8)
                a=unlist(a)
                agg_thresholds$Threshold[i] <- quantile(a, probs = agg_th, na.rm = T)
        }
        
        print ("Find Local Trend")
        
        #Finding Local Trends
        Levels <- list()
        for (lev2 in 1:length(period)){
                levels_data <- NULL
                levels_data_row <- NULL
                if (!is.na(period[(lev2+1)])) {
                        levels_part <- NULL
                        for (i in 1:(period[lev2+1]/period[lev2])) {
                                for (ii in 1:(floor(length(metric)/period[lev2+1])+1)) {
                                        mt <- metric[(1 + period[lev2]*(i-1) + period[lev2+1]*(ii-1)):(period[lev2]*i + period[lev2+1]*(ii-1))]
                                        levels_part <- c(levels_part, mt)
                                        levels_data_row <- c(levels_data_row, mt)
                                }
                                levels_data <- c(levels_data, quantile(levels_part, probs = local_trend, na.rm = T))
                        }
                }

                if (is.na(period[(lev2+1)])) {
                        for (i in 1:(floor(length(metric)/period[lev2])+1)) {
                                mt <- metric[((i-1)*period[lev2] + 1):((i-1)*period[lev2] + period[lev2])]
                                levels_data <- c(levels_data, quantile(mt, probs = local_trend, na.rm = T))
                                levels_data_row <- c(levels_data_row, mt)
                        }
                }

                Levels[[lev2]] <- levels_data / quantile(levels_data_row, probs = local_trend, na.rm = T)

                if (identical_thresholds[[lev2]] == TRUE) {
                        Levels[[lev2]] <- rep(1, length(Levels[[lev2]]))
                }

                Levels[[lev2]] <- as.vector(Levels[[lev2]] )
        }
        
        Levels=lapply(Levels, function(x){x[is.na(x)]=1; return(x)})
        print ("Threshold Adjusting")
        
        one_period <- list()
        params <- list()
        
        for (set in 1:length(thresholds)) {
          params[[set]] <- list(metric = metric[(1 + period[2] * (set - 1)):(set * period[2])],
                                thresholds = agg_thresholds,
                                period = period[1],
                                agg_th = agg_th,
                                k = k,
                                similar = similar)}
        
      #Aggregating Thresholds   
        one_period<-mclapply(params,
                            FUN = function(x) {aggregate.thresholds(metric = x$metric,
                                                         thresholds = x$thresholds,
                                                         period = x$period,
                                                         agg_th = x$agg_th,
                                                         k = x$k,
                                                         similar = x$similar)}, mc.cores = 8)

        a=1

        one_period <- list(thresholds = one_period[[1]]$thresholds, levels = Levels[[1]])
        
        ad.model=list(thresholds = one_period$thresholds, levels = Levels,  
                   period = period, max = max(metric), initial_time = data$time[1], 
                   ts_type=time_series$ts_type,
                   ts_val=time_series$ts_val)
        
      # Detect Anomalies
        

        options(warn=0)
 
        return(ad.model)
}
