source("Model/aggregate_thresholds.R")
source("Model/app_timeseries.R")
source("Model/dynamicThreshold.model.R")
source("Model/findPeriod_ssa.R")
source("Model/find_Thresh.R")
source("Model/find_Thresh_set.R")
source("Model/plotfirstPeriods.R")

source("Preprocessing.R")
library(plotly)
library(Rssa)


HH_data <- readRDS("data/household_power_consumption_datatime.rds")

#create time series
data <- xts(x = HH_data$Voltage, order.by = HH_data$datetime)

#remove NA
data_na.rm=remove_na_from_data(data, type = "mean")
ts_type="hours"
ts_val=1
#remove NA
data.agg=aggregation_data(data_na.rm, type = paste(ts_val, ts_type), func_aggregate = 'median', quantile_percent = .5)

#prepare dataframe
names(data.agg)=c("values")
data.agg=as.data.frame(data.agg)
data.agg$time=as.POSIXct(rownames(data.agg), "GMT",format = "%Y-%m-%d %H:%M:%S")

#plot timeseries
data.agg %>% plot_ly(x=~time, y=~values) %>% add_lines()

#extract periods form timeseries
periods=findPeriod_ssa(data.agg$values)

#plot timeseries with extracted periods
plotfirstPeriods(data = data.agg, periods = periods, N = 100)

#time series reconstruction
data.reconstructed=app_timeseries(data.agg$values, dsigma = 0.1)

# DT model training
model.DT<-dynamicThreshold.model(metric = data.agg$values,
                metric_reconstructed = data.reconstructed,
                period = sort(periods),
                prob_th = 0.9,
                prob_agg = 0.5,
                k = 0.1,
                similar = 0.25,
                corrected_by = c(1, 1, 1,1,1,1),
                identical_thresholds = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE))

# plot timeseries with anomalies
plotfirstPeriods(data = data.agg, periods = periods, anomalies = model.DT$anomalies)

