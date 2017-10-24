find_Thresh <- function (metric,
                         metric_reconstructed,
                         period = 1440,
                         simil = 0.6,
                         alpha = 0.01,
                         k = 0.1,
                         divide_min = FALSE,
                         simil_measure = function (data) {return (mean(data))},
                         one_period = FALSE){
  
  # Find thresholds for time series
  # Function is used to divide timeseries into periods and time-slots and find thresholds in them. 
  # It also produces plot which visualize timeseries and found thresholds
  # metric - timeseries data in vector format
  # metric_reconstructed - reconstructed timeseries with less noise. Could be computed with \code{\link{app_timeseries}}
  # period - (default 1440) fluctuation period of time series. Could be determined using \code{\link{findPeriod_chisq}}, \code{\link{findPeriod_fft}} functions or set manually. 1440 is used as default value (number of minutes in day and night)
  # simil - (default 0.6) coefficient in range [0, 1] which indicates how similar neighboring time-slots have to be to concatenate them. So, if fraction of two neighboring time-slots is > simil -> they will be concatenated in one group
  # alpha - (default 0.01) significance level for confidence interval
  # k - (default 0.1) indicates how short time-slots could be. If time-slot is less than k*period, it will be concatenated with most similar neighboring time-slot
  # divide_min  - (default FALSE) if FALSE than boundaries between time-slots will be set on the mean between two local extremes. If TRUE - boundaries will be set on the local minimums
  # simil_measure - (default function (data) {return (mean(data))}) function which indicates metric for similarity test. By default - average values of time-slots
  # one_period - indicates if only one period should be analysed. If FALSE - than all timeseries will be processed. If 1..number of periods - than single period will be processed
  
  # return data.frame which contains thresholds for all periods and their time-slots

        if ((length(metric) - (floor(length(metric) / period) * period)) == 1) {
                metric <- metric[1:(length(metric)-1)]
                metric_reconstructed <- metric_reconstructed[1:(length(metric_reconstructed)-1)]
        }

        dynamic_thresholds <- data.frame(Minute_Low = integer(),
                                         Minute_High = integer(),
                                         Threshold = numeric())

        if (one_period > 0) {
                metric <- metric[(1 + (one_period - 1) * period):(period + (one_period - 1) * period)]
                metric_reconstructed <- metric_reconstructed[(1 + (one_period - 1) * period):(period + (one_period - 1) * period)]
        }

        

        for (per in 1:ceiling(length(metric) / period)) {

                x1 <- period * (per - 1) + 1
                x2 <- min(length(metric), period * (per - 1) + period)

                metr <- metric[x1:x2]
                a <- metric_reconstructed[x1:x2]

                xz <- as.zoo(a)

                n1 <- which(rollapply(xz, 3, function(x) which.max(x)==2) == TRUE)
                n2 <- which(rollapply(xz, 3, function(x) which.min(x)==2) == TRUE)

                if ((1 %in% n1 == FALSE) & (1 %in% n2 == FALSE)){
                        if (xz[[1]] > xz[[2]]) {
                                n1 <- c(1, n1)
                        } else {n2 <- c(1, n2)}
                }

                if ((length(xz) %in% n1 == FALSE) & (length(xz) %in% n2 == FALSE)){
                        if (xz[[length(xz)]] > xz[[length(xz) - 1]]) {
                                n1 <- c(n1, length(xz))
                        } else {n2 <- c(n2, length(xz))}
                }

                if (divide_min == FALSE) {

                        n <- sort(c(n1, n2))

                        avepart <- NULL

                        for(i in 1:(length(n) - 1)) {
                                avepart <- c(avepart,
                                             n[i] - 1 + which(mean(c(a[n[i]], a[n[i+1]])) == sort(c(a[n[i]:n[i+1]], mean(c(a[n[i]], a[n[i+1]]))))))
                        }

                }

                if (divide_min == TRUE) {

                        avepart <- n2

                }

                if ((1 %in% avepart) == FALSE){
                        avepart <- c(1, avepart)
                }

                if ((length(xz) %in% avepart) == FALSE){
                        avepart <- c(avepart, length(xz))
                }

                if (length(avepart) > 2) {
                        new_per <- NULL
                        for (i in 2:(length(avepart)-1)){
                                new_per <- c(new_per, (avepart[i] + 1))
                        }
                        avepart <- sort(c(avepart, new_per))
                } else {
                        avepart <- sort(avepart)
                }

                periods <- data.frame(matrix(avepart,nrow = length(avepart) / 2,ncol = 2, byrow = TRUE))
                names(periods) <- c("start", "end")

                periods["value"] <- NA
                for (i in 1:dim(periods)[1]) {
                        periods[i, c("value")] <- simil_measure(a[periods[i,1]:periods[i,2]])
                }

                periods["similarity"] <- NA

                if (dim(periods)[1] > 1) {
                        for (i in 2:dim(periods)[1]) {
                                periods[i, c("similarity")] <- min(periods[i-1,c("value")],
                                                                           periods[i,c("value")]) / max(periods[i-1,c("value")],
                                                                                                                periods[i,c("value")])
                        }
                }

                while (sum(periods[, c("similarity")] > simil, na.rm = TRUE) > 0) {
                        conc <- which.max(periods[, c("similarity")])
                        periods[(conc - 1), c("end")] <- periods[conc, c("end")]
                        periods[(conc - 1), c("value")] <- simil_measure(a[periods[(conc - 1),1]:periods[(conc - 1),2]])
                        if ((conc - 1) > 1) {
                                periods[(conc - 1), c("similarity")] <- min(periods[(conc - 2),c("value")],
                                                                           periods[(conc - 1),c("value")]) / max(periods[(conc - 2),c("value")],
                                                                                                                periods[(conc - 1),c("value")])
                        } else periods[(conc - 1), c("similarity")] <- NA

                        periods <- periods[-conc, ]

                        if (dim(periods)[1] >= conc) {
                                periods[conc, c("similarity")] <- min(periods[(conc - 1),c("value")],
                                                                              periods[(conc),c("value")]) / max(periods[(conc - 1),c("value")],
                                                                                                                        periods[(conc),c("value")])
                        }
                }

                periods[, c("leng")] <- periods[, ("end")] - periods[, ("start")]

                while ((sum(periods[, c("leng")] < k * period) > 0) & (nrow(periods) > 1)) {
                        short <- which.min(periods[, c("leng")])
                        if (short == 1) {
                                short_conc <- 2
                        } else if (short == dim(periods)[1]) {
                                short_conc <- dim(periods)[1] - 1
                        } else {
                                if (periods[(short), c("similarity")] < periods[(short+1), c("similarity")]) {
                                        short_conc <- short - 1
                                } else {
                                        short_conc <- short + 1
                                }
                        }

                        short <- max(short, short_conc)

                        periods[(short - 1), c("end")] <- periods[short, c("end")]
                        periods[(short - 1), c("value")] <- simil_measure(a[periods[(short - 1),1]:periods[(short - 1),2]])
                        periods[(short - 1), c("leng")] <- periods[(short - 1), ("end")] - periods[(short - 1), ("start")]

                        if ((short - 1) > 1) {
                                periods[(short - 1), c("similarity")] <- min(periods[(short - 2),c("value")],
                                                                                    periods[(short - 1),c("value")]) / max(periods[(short - 2),c("value")],
                                                                                                                                  periods[(short - 1),c("value")])
                        } else periods[(short - 1), c("similarity")] <- NA

                        periods <- periods[-short, ]

                        if (dim(periods)[1] >= short) {
                                periods[short, c("similarity")] <- min(periods[(short - 1),c("value")],
                                                                              periods[(short),c("value")]) / max(periods[(short - 1),c("value")],
                                                                                                                        periods[(short),c("value")])
                        }
                }

                thresh <- NULL

                for (i in 1:nrow(periods)) {
                        rmsd <- qnorm(1 - (alpha / 2)) * sqrt(sum(((a[periods[i, 1]:periods[i, 2]] - metr[periods[i, 1]:periods[i, 2]]) ^ 2)) / length(a[periods[i, 1]:periods[i, 2]]))
                        thresh <- c(thresh, max(a[periods[i, 1]:periods[i, 2]] + rmsd))
                        dynamic_thresholds <- rbind(dynamic_thresholds, data.frame(Minute_Low = period * (per - 1) + periods[i, 1],
                                                                                   Minute_High = period * (per - 1) + periods[i, 2],
                                                                                   Threshold = max(a[periods[i, 1]:periods[i, 2]] + rmsd)))
                }

        }


        for (i in 2:nrow(dynamic_thresholds)) {
                if (dynamic_thresholds[i, 1] == dynamic_thresholds[i-1, 2]) dynamic_thresholds[i, 1] <- dynamic_thresholds[i, 1] + 1
        }

        
        return (dynamic_thresholds)
}
