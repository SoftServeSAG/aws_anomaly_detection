app_timeseries <- function(
        data,
        trend = TRUE,
        dsigma = 0.1) {
  
  # Find timeseries approximation
  #
  # Input:
  # data - vector with time-series data
  # trend - flag that represents if trend will be included to the time series
  # dsigma - minimal significance of eigenvectors 
  #
  # Output:
  # reconstracted time series

        SSA= ssa(data, L = round(length(data) / 2))

        sigma=SSA$sigma[2:nsigma(SSA)]
        n = sum(sigma/max(sigma)>=dsigma)

        if (trend == TRUE) {
                R=reconstruct(SSA, groups=1:n)
        } else {
                R=reconstruct(SSA, groups=2:n)
        }

        timeSeries_reconstructed <- rep(0, length(R[[1]]))

        for (i in 1:length(R)) {
                timeSeries_reconstructed <- timeSeries_reconstructed + R[[i]]
        }

        return(timeSeries_reconstructed)
}

# test

# HH_data <- readRDS("data/household_power_consumption_datatime.rds")
# 
# #create time series
# data <- xts(x = HH_data$Voltage, order.by = HH_data$datetime)
# 
# #remove NA
# data_na.rm=remove_na_from_data(data, type = "mean")
# ts_type="hour"
# ts_val=1
# #remove NA
# data.agg=aggregation_data(data_na.rm, type = paste(ts_val, ts_type), func_aggregate = 'median', quantile_percent = .5)
# 
# #prepare dataframe
# names(data.agg)=c("values")
# data.agg=as.data.frame(data.agg)
# data.agg$time=as.POSIXct(rownames(data.agg), "GMT",format = "%Y-%m-%d %H:%M:%S")
# 
# #plot timeseries
# data.agg %>% plot_ly(x=~time, y=~values) %>% add_lines()
# 
# #extract periods form timeseries
# data.reconstracted=app_timeseries(data.agg$values, dsigma = 0.1)
# data.reconstracted=data.frame(time=data.agg$time, values=data.reconstracted)
# 
# #plot reconstracted timeseries
# data.agg %>% plot_ly(x=~time, y=~values) %>% add_lines(name="data") %>% 
#   add_lines(data=data.reconstracted, x=~time, y=~values, name="reconstructed")
# 
