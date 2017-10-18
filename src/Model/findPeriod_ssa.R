findPeriod_ssa <- function (data, dsigma=0.4) {
  
  # Function extracts periods form Time Series based on Singular Spectral Analysis
  #
  # Input:
  # data - vector with time-series data
  # dsigma - minimal significance of eigenvectors 
  #
  # Output:
  # set of unique periods

        SSA= ssa(data, L = round(length(data) / 2))
        sigma=SSA$sigma[2:nsigma(SSA)]
        nsignif = sum(sigma/max(sigma)>=dsigma)
        
        per_estimate = parestimate(SSA, groups = list(c(1:(nsignif+1))),method = "esprit-ls")
        periods = round(per_estimate$periods[per_estimate$periods>0 & !is.infinite(per_estimate$periods)])
        periods = unique(periods)
       
        return(periods)
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
# periods=findPeriod_ssa(data.agg$values)
# 
#plot timeseries with extracted periods

#plotfirstPeriods(data = data.agg, periods = periods, N = 100)
