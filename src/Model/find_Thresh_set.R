find_Thresh_set <- function (metric,
                             metric_reconstructed,
                             period = 1440,
                             period_set = 7*1440,
                             simil = 0.6,
                             alpha = 0.01,
                             k = 0.1,
                             divide_min = FALSE,
                             simil_measure = function (data) {return (mean(data))})
  
  #' Find sets of thresholds
  #' This function works in the same way as \code{\link{find_Tresh}} function. But it firstly divides timeseries in sets and then executes \code{\link{find_Tresh}} on all sets separately. This function is more intermediate like. 
  #' It will be used in aggragation step. For example, by \code{\link{build_model}} fucntion
  
  # metric -timeseries data in vector format
  # metric_reconstructed - reconstructed timeseries with less noise. Could be computed with \code{\link{app_timeseries}}
  # period (default 1440)  - fluctuation period of time series. Could be determined using \code{\link{findPeriod_chisq}}, \code{\link{findPeriod_fft}} functions or set manually. 1440 is used as default value (number of minutes in day and night)
  # period_set  - (default 7*1440) bigger fluctuation period of time series. 1440 x 7 is used as default value (number of minutes in week)
  #  simil  - (default 0.6) coefficient in range [0, 1] which indicates how similar neighboring time-slots have to be to concatenate them. So, if fraction of two neighboring time-slots is > simil -> they will be concatenated in one group
  #  alpha  - (default 0.01) significance level for confidence interval
  #  k ( - default 0.1) indicates how short time-slots could be. If time-slot is less than k*period, it will be concatenated with most similar neighboring time-slot
  #  divide_min  - (default FALSE) if FALSE than boundaries between time-slots will be set on the mean between two local extremes. If TRUE - boundaries will be set on the local minimums
  #  simil_measure  - (default function (data) {return (mean(data))}) function which indicates metric for similarity test. By default - average values of time-slots
  # return:
  # list of data.frames which contain thresholds for periods and their time-slots in scope of different bigger periods (sets)

{
  
        n_sets <- floor(length(metric) / period_set)

        params <- list()
        
        for (set in 1:n_sets) {

          params[[set]] <- list(metric = metric[(1 + (set - 1) * period_set):(set * period_set)],
                                metric_reconstructed = metric_reconstructed[(1 + (set - 1) * period_set):(set * period_set)],
                                period = period,
                                simil = simil,
                                alpha = alpha,
                                k = k,
                                divide_min = divide_min,
                                simil_measure = simil_measure,
                                one_period = FALSE)
        }
        
        
        counter = 0
        
        thresholds <- mclapply(params,
                              FUN = function(x) {
                                find_Thresh(metric = x$metric,
                                                            metric_reconstructed = x$metric_reconstructed,
                                                            period = x$period,
                                                            simil = x$simil,
                                                            alpha = x$alpha,
                                                            k = x$k,
                                                            divide_min = x$divide_min,
                                                            simil_measure = x$simil_measure,
                                                            one_period = x$one_period)
                                }, mc.cores = 8)


        return(thresholds)
}
