source("src/Model/aggregate.thresholds.R")
source("src/Model/app_timeseries.R")
source("src/Model/dynamicThreshold.model.R")
source("src/Model/findPeriod_ssa.R")
source("src/Model/find_Thresh.R")
source("src/Model/find_Thresh_set.R")
source("src/Model/plotfirstPeriods.R")
source("src/Model/plotThresholdsAnomalies.R")
source("src/Model/timeseries_prepare.R")
source("src/Model/find.anomalies.R")

source("src/aggregation.R")
source("src/denoise.R")
source("src/remove_na_values.R")
source("src/remove_outliers.R")

#source("Preprocessing.R")
library(plotly)
library(Rssa)

library(parallel)



HH_data <- readRDS("data/household_power_consumption_datatime.rds")

#create time series
data <- xts(x = HH_data$Voltage, order.by = HH_data$datetime)

#remove NA
data_na.rm=remove_na_from_data(data, type = "mean")

#aggregation
ts_type="days"
ts_val=1
data.agg=aggregation_data(data_na.rm, type = paste(ts_val, ts_type), func_aggregate = 'median', quantile_percent = .5)


#prepare data for model traioning
ts_train = timeseries_train(data.agg = data.agg, ts_type = ts_type, ts_val = ts_val)

#prepare data for model applying
ts_test = timeseries_test(data.agg = data.agg, ts_type = ts_type, ts_val = ts_val)

#plot timeseries
ts_train$time_series$ts %>% plot_ly(x=~time, y=~values) %>% add_lines()
#plot timeseries with extracted periods
plotfirstPeriods(data = ts_train$time_series$ts, periods = ts_train$periods, N = 100)




model.DT<-dynamicThreshold.model(ts_train$time_series,
                                 period = sort(ts_train$periods),
                                 agg_th = 0.75, # the higher the more tolerant
                                 local_trend = 0.5, # local trend is determined based on quantile of corresponded preriods
                                 k = 0.1,
                                 similar = 0.1, #significance in the divergence between thresholds of the near diapasons
                                 identical_thresholds = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE))
plotTSThresholdsAnomalies(data=ts_train$time_series$ts, thresholds = model.DT$th_plot, anomalies = model.DT$anomalies)

RES=find.anomalies(data = ts_test$ts, ts_par = ts_test$ts_par, 
                   ad.model = model.DT$model, coef=0.1, scale=2)
plotTSThresholdsAnomalies(data=ts_test$ts, thresholds = RES$th_plot, anomalies = RES$anomalies)


RES=find.anomalies(data = ts_test$ts[500:1300,], ts_par = ts_test$ts_par, ad.model = model.DT$model,  coef=0.1, scale=1.2)
plotTSThresholdsAnomalies(data=ts_test$ts[500:1300,], thresholds = RES$th_plot, anomalies = RES$anomalies)



# High sencetivity
model.DT<-dynamicThreshold.model(ts_train$time_series,
                                 period = sort(ts_train$periods),
                                 agg_th = 0.5, # the higher the more tolerant
                                 local_trend = 0.1, # local trend is determined based on quantile of corresponded preriods
                                 k = 0.1,
                                 similar = 0.01, #significance in the divergence between thresholds of the near diapasons
                                 
                                 identical_thresholds = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE))
plotTSThresholdsAnomalies(data=ts_train$time_series$ts, thresholds = model.DT$th_plot, anomalies = model.DT$anomalies)

# Medium sencetivity
model.DT<-dynamicThreshold.model(ts_train$time_series,
                                 period = sort(ts_train$periods),
                                 agg_th = 0.6, # the higher the more tolerant
                                 local_trend = 0.3, # local trend is determined based on quantile of corresponded preriods
                                 k = 0.1,
                                 similar = 0.1, #significance in the divergence between thresholds of the near diapasons
                                 
                                 identical_thresholds = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE))
plotTSThresholdsAnomalies(data=ts_train$time_series$ts, thresholds = model.DT$th_plot, anomalies = model.DT$anomalies)


# Low sencetivity
model.DT<-dynamicThreshold.model(ts_train$time_series,
                                 period = sort(ts_train$periods),
                                 agg_th = 0.8, # the higher the more tolerant
                                 local_trend = 0.5, # local trend is determined based on quantile of corresponded preriods
                                 k = 0.01,
                                 similar = 0.3, #significance in the divergence between thresholds of the near diapasons
                                 identical_thresholds = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE))
plotTSThresholdsAnomalies(data=ts_train$time_series$ts, thresholds = model.DT$th_plot, anomalies = model.DT$anomalies)

RES=find.anomalies(data = ts_test$ts[700:1400,], 
                   ts_par = ts_test$ts_par, 
                   ad.model = model.DT$model, 
                   coef=0, scale=1)

plotTSThresholdsAnomalies(data = ts_test$ts[700:1400,], 
                          thresholds = RES$th_plot, 
                          anomalies = RES$anomalies)

