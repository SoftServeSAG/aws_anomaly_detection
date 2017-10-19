#' First level aggregating of thresholds
#'
#' @param metric timeseries data in vector format
#' @param thresholds data.frame of found thresholds for metric. Could be computed with \code{\link{find_Thresh}}
#' @param period (default 1440) fluctuation period of time series. Should be used the same as while computing not aggregated thresholds (thresholds parameter)
#' @param prob_th (default 0.95) probability from range [0, 1] which will be used to compute aggregated value for thresholds (as probability for quantile)
#' @param prob_agg (default 0.9) probability from range [0, 1] which will be used to compute aggregation for periods (as probability for quantile)
#' @param k (default 0.1) indicates how short time-slots could be. If time-slot is less than k*period, it will be concatenated with time-slot with nearest threshold
#' @param similar (default 0.1) coefficient in range [0, 1] which indicates how similar neighboring time-slots' thresholds have to be to concatenate them. So, if fraction of two neighboring time-slots is < similar -> they will be concatenated in one group
#' @param corrected_by (default 1) coefficient which is used to increase or decrease all thresholds. Could be used to tune model
#' @param plt (default TRUE) if TRUE than thresholds will be plotted
#' @param identical_thresholds (default FALSE) indication if thresholds should be the same for all the periods. In case TRUE all period levels will be equal "1"
#'
#' @return list of two elements. One contains data.frame with thresholds for one period. Another - vector of period levels
#'
#' @details
#' Function is used to aggregate thresholds and make them the same for all periods (with respect to period level). So, thresholds for some period could be computed as thresholds multipliyed on this period level
#'
#' @examples
#'
#' reconstructed_timeseries <- app_timeseries(timeseries_examples[[1]], trend = TRUE)
#'
#' thresholds <- find_Thresh(metric = timeseries_examples[[1]],
#'                           metric_reconstructed = reconstructed_timeseries,
#'                           period = 1440,
#'                           simil = 1,
#'                           alpha = 0.05,
#'                           k = 0,
#'                           divide_min = FALSE,
#'                           simil_measure = max,
#'                           one_period = FALSE)
#'
#' aggregate_thresholds (metric = timeseries_examples[[1]],
#'                       thresholds = thresholds,
#'                       period = 1440,
#'                       prob_th = 0.7,
#'                       k = 0.1,
#'                       similar = 0.2,
#'                       corrected_by = 1,
#'                       plt = TRUE,
#'                       identical_thresholds = FALSE)
#'
#' aggregate_thresholds (metric = timeseries_examples[[1]],
#'                       thresholds = thresholds,
#'                       period = 1440,
#'                       prob_th = 0.8,
#'                       k = 0.1,
#'                       similar = 0.2,
#'                       corrected_by = 1,
#'                       plt = TRUE,
#'                       identical_thresholds = TRUE)
#'
#' @export

aggregate_thresholds <- function(metric,
                                 thresholds,
                                 period = 1440,
                                 prob_th = 0.95,
                                 prob_agg = 0.9,
                                 k = 0.1,
                                 similar = 0.1,
                                 corrected_by = 1,
                                 plt = TRUE,
                                 identical_thresholds = FALSE) {

        #Add indicator of beginning or end of the period
        thresholds$Period_Start <- c(thresholds$Minute_Low %% period == 1)
        thresholds$Period_End <- c(c(thresholds$Minute_High %% period == 0))

        #Deleting rows for possible incomplete period
        while (thresholds$Period_End[nrow(thresholds)] == FALSE) {
                thresholds <- thresholds[-nrow(thresholds),]
        }

        #Calculating time of the beginning and end of the periods' timeslots in scope of the periods
        for (i in 1:dim(thresholds)[1]) {
                thresholds$Minute_Low_Reset[i] <- thresholds$Minute_Low[i] - period * (sum(thresholds$Period_Start[1:i])-1)
        }

        for (i in 1:dim(thresholds)[1]) {
                thresholds$Minute_High_Reset[i] <- thresholds$Minute_High[i] - period * sum(thresholds$Period_End[1:i])
        }

        thresholds$Minute_High_Reset[thresholds$Minute_High_Reset == 0] <- period

        #Dividing data.frame into few data.frames. Each will contain only one period
        period_divided <- list()

        n1 <- which(thresholds$Minute_Low_Reset == 1)
        n2 <- which(thresholds$Minute_High_Reset == period)

        for (i in 1:length(n1)) {
                period_divided[[i]] <- thresholds[n1[i]:n2[i],]
        }

        #Picking together all possible time slots
        agg_thresholds <- data.frame(Minute_Low = sort(unique(thresholds$Minute_Low_Reset)),
                                     Minute_High = sort(unique(thresholds$Minute_High_Reset)))

        #Finding thresholds for all timeslots
        for (i in 1:dim(agg_thresholds)[1]) {
                a <- NULL
                for (j in 1:length(period_divided)){
                        a <- c(a, period_divided[[j]]$Threshold[(period_divided[[j]]$Minute_Low_Reset <= agg_thresholds$Minute_Low[i]) &
                                                                        (period_divided[[j]]$Minute_High_Reset >= agg_thresholds$Minute_High[i])])
                }
                agg_thresholds$Threshold[i] <- quantile(a, probs = prob_th, na.rm = T)
        }

        agg_thresholds$Simil <- c(NA, abs(1 - (agg_thresholds$Threshold[-length(agg_thresholds$Threshold)] / agg_thresholds$Threshold[-1])))

        #Aggregating similar timeslots
        while (sum(agg_thresholds$Simil < similar, na.rm = TRUE) > 0) {

                sim <- which.min(agg_thresholds$Simil)

                agg_thresholds[(sim - 1), c("Minute_High")] <- agg_thresholds[(sim), c("Minute_High")]

                agg_thresholds[(sim - 1), c("Threshold")] <- max(agg_thresholds[(sim - 1), c("Threshold")], agg_thresholds[(sim), c("Threshold")])

                agg_thresholds <- agg_thresholds[-sim, ]

                if (sim >= 3) {
                        agg_thresholds[(sim-1), c("Simil")] <- abs(1 - (agg_thresholds$Threshold[(sim-2)] / agg_thresholds$Threshold[(sim-1)]))
                }

                if (sim <= nrow(agg_thresholds)) {
                        agg_thresholds[(sim), c("Simil")] <- abs(1 - agg_thresholds$Threshold[(sim-1)] / agg_thresholds$Threshold[(sim)])
                }
        }

        #Aggregating short timeslots
        agg_thresholds$Length <- agg_thresholds$Minute_High - agg_thresholds$Minute_Low + 1

        while (sum(agg_thresholds$Length < k * period) > 0) {
                short <- which.min(agg_thresholds$Length)
                if (short == 1) {
                        short_conc <- 2
                } else if (short == dim(agg_thresholds)[1]) {
                        short_conc <- dim(agg_thresholds)[1] - 1
                } else {
                        if (abs(agg_thresholds[(short), c("Threshold")] - agg_thresholds[(short+1), c("Threshold")]) >= abs(agg_thresholds[(short), c("Threshold")] - agg_thresholds[(short-1), c("Threshold")])) {
                                short_conc <- short - 1
                        } else {
                                short_conc <- short + 1
                        }
                }

                short <- max(short, short_conc)

                agg_thresholds[(short - 1), c("Minute_High")] <- agg_thresholds[(short), c("Minute_High")]

                agg_thresholds[(short - 1), c("Threshold")] <- (agg_thresholds[(short - 1), c("Threshold")] * agg_thresholds[(short - 1), c("Length")] + agg_thresholds[(short), c("Threshold")] * agg_thresholds[(short), c("Length")]) / (sum(agg_thresholds[((short-1):short), c("Length")]))

                agg_thresholds[(short - 1), c("Length")] <- agg_thresholds[(short - 1), ("Minute_High")] - agg_thresholds[(short - 1), ("Minute_Low")] + 1

                agg_thresholds <- agg_thresholds[-short, ]
        }

        #Calculating levels of each period
        lev <- NULL
        for (i in 1:floor(length(metric) / period)) {
                lev <- c(lev, quantile(metric[((i-1)*period + 1):((i-1)*period + period)], probs = prob_agg, na.rm = T))
        }
        lev <- lev / quantile(metric, probs = prob_agg, na.rm = T)

        if (identical_thresholds == TRUE) {
                lev <- rep(1, length(lev))
        }

        lev <- lev * corrected_by

        #Plotting thresholds
        if (plt == TRUE) {
                #x11(width = 14, height = 10)
                par(oma = c(4, 1, 1, 1), bg = "gray95")

                plot(metric[1:thresholds$Minute_High[nrow(thresholds)]], type = "l", ylim = c(1, 1.5 * max(metric)), xlim = c(1, length(metric)), xlab = "Time", ylab = "Metric", xaxt='n', yaxt = "n", col = "gray", cex.lab = 1.3)

                for (i in 1:floor(length(metric) / period)) {
                        abline(v = period * i, col = "tan1", lwd = 2)
                }

                for (per in 1:floor(length(metric) / period)) {
                        for (i in 1:nrow(agg_thresholds)) {
                                rect(period * (per - 1) + agg_thresholds$Minute_Low[i],
                                     0,
                                     period * (per - 1) + agg_thresholds$Minute_High[i],
                                     agg_thresholds$Threshold[i] * lev[per],
                                     lty = 2,
                                     density = 20,
                                     col = 139)

                                points(c(agg_thresholds[i, 1]:agg_thresholds[i, 2])[metric[(1+(period * (per-1))):(period*per)][agg_thresholds[i, 1]:agg_thresholds[i, 2]] > agg_thresholds$Threshold[i] * lev[per]] + period * (per - 1),
                                       metric[(1+(period * (per-1))):(period*per)][agg_thresholds[i, 1]:agg_thresholds[i, 2]][metric[(1+(period * (per-1))):(period*per)][agg_thresholds[i, 1]:agg_thresholds[i, 2]] > agg_thresholds$Threshold[i] * lev[per]],
                                       col = "red",
                                       pch = 16)
                        }
                }

                axis(1, at=c(1, period * (1:floor(length(metric) / period))), labels = TRUE, tick = TRUE)
                axis(2, at=c(0, round(agg_thresholds$Threshold)), labels = TRUE, tick = TRUE)

                legend("bottom",
                       legend = c("TimeSeries", "Anomaly", "Normal zone"),
                       lwd = 1, cex = 1,
                       xpd = TRUE,
                       horiz = TRUE,
                       inset = c(0,1),
                       bty = "n",
                       col = c("gray", "red", 139),
                       lty = c(1, NA, 2),
                       pch = c(NA, 16, NA))

        }
        return(list(thresholds = agg_thresholds[,1:3], levels = as.vector(lev)))
}

