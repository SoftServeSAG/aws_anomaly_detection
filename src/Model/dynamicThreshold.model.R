dynamicThreshold.model <- function(metric,
                          metric_reconstructed,
                          period = c(1440, 7*1440),
                          prob_th = 0.9,
                          prob_agg = 0.9,
                          k = 0.1,
                          similar = 0.1,
                          corrected_by = rep(1, 1),
                          identical_thresholds = c(FALSE, FALSE)) {
  #Load Thresholds Determination
  #
  # metric - timeseries data in vector format
  # metric_reconstructed - reconstructed timeseries with less noise. Could be computed with \code{\link{app_timeseries}}
  # period - (default c(1440, 7*1440)) vector of periods of time series with minimum two elements.
  # prob_th - (default 0.9) probability from range [0, 1] which will be used to compute aggregated value for thresholds (as probability for quantile)
  # prob_agg - (default 0.9) probability from range [0, 1] which will be used to compute aggregation for periods (as probability for quantile)
  # k - (default 0.1) vector of indicates how short time-slots could be. If time-slot is less than k*period, it will be concatenated with time-slot with nearest threshold
  # similar - (default 0.1) coefficient in range [0, 1] which indicates how similar neighboring time-slots' thresholds have to be to concatenate them. So, if fraction of two neighboring time-slots is < similar -> they will be concatenated in one group
  # corrected_by - (default c(1, 1)) vector of coefficients of the same length as period vector which is used to increase or decrease thresholds of different levels. Could be used to tune model
  # identical_thresholds - (default c(FALSE, FALSE)) indication if thresholds should be the same for periods of different levels

        #Calculating sets of thresholds
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
        metric <- metric[1:(floor(length(metric) / period[length(period)]) *  period[length(period)])]

        #Concatenate all sets
        all_thresholds <- data.frame(do.call(rbind, thresholds))

        #Picking together all possible time slots
        agg_thresholds <- data.frame(Minute_Low = sort(unique(all_thresholds$Minute_Low)),
                                     Minute_High = sort(unique(all_thresholds$Minute_High)))

        #Finding thresholds for all timeslots
        for (i in 1:nrow(agg_thresholds)) {
                a <- NULL
                for (j in 1:length(thresholds)){
                        a <- c(a, thresholds[[j]]$Threshold[(thresholds[[j]]$Minute_Low <= agg_thresholds$Minute_Low[i]) &
                                                                    (thresholds[[j]]$Minute_High >= agg_thresholds$Minute_High[i])])
                }
                agg_thresholds$Threshold[i] <- quantile(a, probs = prob_th, na.rm = T)
        }

        #Finding levels
        Levels <- list()
        for (lev2 in 1:length(period)){
                levels_data <- NULL
                levels_data_row <- NULL
                if (!is.na(period[(lev2+1)])) {
                        levels_part <- NULL
                        for (i in 1:(period[lev2+1]/period[lev2])) {
                                for (ii in 1:floor(length(metric)/period[lev2+1])) {
                                        mt <- metric[(1 + period[lev2]*(i-1) + period[lev2+1]*(ii-1)):(period[lev2]*i + period[lev2+1]*(ii-1))]
                                        levels_part <- c(levels_part, mt)
                                        levels_data_row <- c(levels_data_row, mt)
                                }
                                levels_data <- c(levels_data, quantile(levels_part, probs = prob_agg, na.rm = T))
                        }
                }

                if (is.na(period[(lev2+1)])) {
                        for (i in 1:floor(length(metric)/period[lev2])) {
                                mt <- metric[((i-1)*period[lev2] + 1):((i-1)*period[lev2] + period[lev2])]
                                levels_data <- c(levels_data, quantile(mt, probs = prob_agg))
                                levels_data_row <- c(levels_data_row, mt)
                        }
                }

                Levels[[lev2]] <- levels_data / quantile(levels_data_row, probs = prob_agg, na.rm = T)

                if (identical_thresholds[[lev2]] == TRUE) {
                        Levels[[lev2]] <- rep(1, length(Levels[[lev2]]))
                }

                Levels[[lev2]] <- as.vector(Levels[[lev2]] * corrected_by[lev2])
        }

        one_period <- list()

        for (set in 1:length(thresholds)) {
          #print (set)
                one_period[[set]] <- aggregate_thresholds(metric = metric[(1 + period[2] * (set - 1)):(set * period[2])],
                                                          thresholds = agg_thresholds,
                                                          period = period[1],
                                                          prob_th = prob_th,
                                                          prob_agg = prob_agg,
                                                          k = k,
                                                          corrected_by = corrected_by[1],
                                                          similar = similar,
                                                          plt = FALSE,
                                                          identical_thresholds = identical_thresholds[1])
        }

        one_period <- list(thresholds = one_period[[1]]$thresholds, levels = Levels[[1]])

        #Determining anomalies in train data
        dts <- one_period$thresholds[rep(1:nrow(one_period$thresholds), floor(length(metric) / period[1])),]
        dts$Day <- rep(1:floor(length(metric) / period[1]), each = nrow(one_period$thresholds))

        dts$Threshold <- dts$Threshold * rep(rep(Levels[[1]], times = length(Levels[[2]])), each = nrow(one_period$thresholds))

        for (lev2 in 2:length(period)) {
                dts$Threshold <- dts$Threshold * rep(rep(Levels[[lev2]], each = period[lev2] / period[1]), each = nrow(one_period$thresholds))

        }

        dts$Minute_Low <- dts$Minute_Low + (dts$Day - 1) * period[1]
        dts$Minute_High <- dts$Minute_High + (dts$Day - 1) * period[1]

        anomaly <- NULL

        dts$Len <- dts$Minute_High - dts$Minute_Low + 1

        a<-rep(dts$Threshold, dts$Len)

        anomaly <- metric > a
 
        return(list(thresholds = one_period$thresholds, levels = Levels,  period = period, anomalies = which(anomaly == TRUE)))
}

#test

# data.agg=readRDS("data/data_agg.rds")
# periods=readRDS("data/periods.rds")
# data.reconstructed=app_timeseries(data.agg$values, dsigma = 0.1)
# periods=findPeriod_ssa(data.agg$values)
# 
# model.DT<-dynamicThreshold.model(metric = data.agg$values,
#                 metric_reconstructed = data.reconstructed,
#                 period = sort(periods),
#                 prob_th = 0.9,
#                 prob_agg = 0.5,
#                 k = 0.1,
#                 similar = 0.25,
#                 corrected_by = c(1, 1, 1,1,1,1),
#                 identical_thresholds = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE))
# 
# 
# plotfirstPeriods(data = data.agg, periods = periods, anomalies = model.DT$anomalies)
# 
