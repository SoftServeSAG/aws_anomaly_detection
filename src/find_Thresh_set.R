#' Find sets of thresholds
#'
#' @param metric timeseries data in vector format
#' @param metric_reconstructed reconstructed timeseries with less noise. Could be computed with \code{\link{app_timeseries}}
#' @param period (default 1440) fluctuation period of time series. Could be determined using \code{\link{findPeriod_chisq}}, \code{\link{findPeriod_fft}} functions or set manually. 1440 is used as default value (number of minutes in day and night)
#' @param period_set (default 7*1440) bigger fluctuation period of time series. 1440 x 7 is used as default value (number of minutes in week)
#' @param simil (default 0.6) coefficient in range [0, 1] which indicates how similar neighboring time-slots have to be to concatenate them. So, if fraction of two neighboring time-slots is > simil -> they will be concatenated in one group
#' @param alpha (default 0.01) significance level for confidence interval
#' @param k (default 0.1) indicates how short time-slots could be. If time-slot is less than k*period, it will be concatenated with most similar neighboring time-slot
#' @param divide_min (default FALSE) if FALSE than boundaries between time-slots will be set on the mean between two local extremes. If TRUE - boundaries will be set on the local minimums
#' @param simil_measure (default function (data) {return (mean(data))}) function which indicates metric for similarity test. By default - average values of time-slots
#' @return list of data.frames which contain thresholds for periods and their time-slots in scope of different bigger periods (sets)
#' @details
#' This function works in the same way as \code{\link{find_Tresh}} function. But it firstly divides timeseries in sets and then executes \code{\link{find_Tresh}} on all sets separately. This function is more intermediate like. It will be used in aggragation step. For example, by \code{\link{build_model}} fucntion
#'
#' @examples
#'
#' reconstructed_timeseries <- app_timeseries(week_data[[1]], trend = TRUE)
#' find_Thresh_set(metric = week_data[[1]],
#'                 metric_reconstructed = reconstructed_timeseries,
#'                 period = 1440,
#'                 period_set = 7*1440,
#'                 simil = 0.6,
#'                 alpha = 0.01,
#'                 k = 0.1,
#'                 divide_min = FALSE,
#'                 simil_measure = max)
#'
#' @export

find_Thresh_set <- function (metric,
                             metric_reconstructed,
                             period = 1440,
                             period_set = 7*1440,
                             simil = 0.6,
                             alpha = 0.01,
                             k = 0.1,
                             divide_min = FALSE,
                             simil_measure = function (data) {return (mean(data))}){

        n_sets <- floor(length(metric) / period_set)

        thresholds <- list()

        for (set in 1:n_sets) {
                thresholds[[set]] <- find_Thresh(metric = metric[(1 + (set - 1) * period_set):(set * period_set)],
                                                 metric_reconstructed = metric_reconstructed[(1 + (set - 1) * period_set):(set * period_set)],
                                                 period = period,
                                                 simil = simil,
                                                 alpha = alpha,
                                                 k = k,
                                                 divide_min = divide_min,
                                                 simil_measure = simil_measure,
                                                 one_period = FALSE,
                                                 plt = FALSE)
        }

        return(thresholds)
}

#' 1 timeseries for 3 weeks as an example
#'
#' @format A vector with 10080 x 3 elements
"week_data"
