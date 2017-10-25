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

ts.agg=list(data.agg=data.agg, ts_type=ts_type, ts_val=ts_val)


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
                                 identical_thresholds = rep(FALSE, length(ts_train$periods)))
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
                                 
                                 identical_thresholds = rep(FALSE, length(ts_train$periods)))
plotTSThresholdsAnomalies(data=ts_train$time_series$ts, thresholds = model.DT$th_plot, anomalies = model.DT$anomalies)

# Medium sencetivity
model.DT<-dynamicThreshold.model(ts_train$time_series,
                                 period = sort(ts_train$periods),
                                 agg_th = 0.6, # the higher the more tolerant
                                 local_trend = 0.3, # local trend is determined based on quantile of corresponded preriods
                                 k = 0.1,
                                 similar = 0.1, #significance in the divergence between thresholds of the near diapasons
                                 
                                 identical_thresholds = rep(FALSE, length(ts_train$periods)))
plotTSThresholdsAnomalies(data=ts_train$time_series$ts, thresholds = model.DT$th_plot, anomalies = model.DT$anomalies)


# Low sencetivity
model.DT<-dynamicThreshold.model(ts_train$time_series,
                                 period = sort(ts_train$periods),
                                 agg_th = 0.8, # the higher the more tolerant
                                 local_trend = 0.5, # local trend is determined based on quantile of corresponded preriods
                                 k = 0.01,
                                 similar = 0.3, #significance in the divergence between thresholds of the near diapasons
                                 identical_thresholds = rep(FALSE, length(ts_train$periods)))
plotTSThresholdsAnomalies(data=ts_train$time_series$ts, thresholds = model.DT$th_plot, anomalies = model.DT$anomalies)

RES=find.anomalies(data = ts_test$ts[700:1400,], 
                   ts_par = ts_test$ts_par, 
                   ad.model = model.DT$model, 
                   coef=0, scale=1)

plotTSThresholdsAnomalies(data = ts_test$ts[700:1400,], 
                          thresholds = RES$th_plot, 
                          anomalies = RES$anomalies)



#==== Smart Train ====================================================================================================

#++++ Simple + Medium ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
my.model=dynamicThreshold.smart_train(ts_train = ts_train, 
                                      train.params = list(mode = 'simple', sensitivity = 'Medium'))
plotTSThresholdsAnomalies(data=ts_train$time_series$ts, thresholds = my.model$th_plot, anomalies = my.model$anomalies)


#++++ Simple + Medium + Correction ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

RES=find.anomalies(data = ts_train$time_series$ts, 
                   ts_par = ts_train$time_seriestest$ts_par, 
                   ad.model = my.model$model, 
                   coef=0.01, scale=2)

plotTSThresholdsAnomalies(data=ts_train$time_series$ts, thresholds = RES$th_plot, anomalies = RES$anomalies)


#++++ Expert  +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
my.model=dynamicThreshold.smart_train(ts_train = ts_train, 
                                      train.params = list
                                      (
                                        mode = 'expert', 
                                        params = list
                                        (
                                          agg_th = 0.75,
                                          local_trend = 0.5,
                                          similar = 0.1
                                        )
                                        ))

my.model_h=dynamicThreshold.smart_train(ts.agg  = ts.agg, 
                                      type_th = "high",
                                      train.params = list
                                      (
                                        mode = 'expert', 
                                        params = list
                                        (
                                          agg_th = 0.75,
                                          local_trend = 0.5,
                                          similar = 0.1
                                        )
                                      ))

my.model_l=dynamicThreshold.smart_train(ts.agg  = ts.agg, 
                                        type_th = "low",
                                        train.params = list
                                        (
                                          mode = 'expert', 
                                          params = list
                                          (
                                            agg_th = 0.75,
                                            local_trend = 0.5,
                                            similar = 0.1
                                          )
                                        ))

plotTSThresholdsAnomalies(data=my.model_h$timeseries$ts, 
                          thresholds = my.model_h$ad_results$th_plot, 
                          anomalies = my.model_h$ad_results$anomalies,
                          type_th=my.model_h$model$type_th)

plotTSThresholdsAnomalies(data=my.model_l$timeseries$ts, 
                          thresholds = my.model_l$ad_results$th_plot, 
                          anomalies = my.model_l$ad_results$anomalies,
                          type_th=my.model_l$model$type_th)



RES1=find.anomalies(data = my.model_h$timeseries$ts, 
                   ts_par = my.model_h$timeseries$ts_par, 
                   ad.model = my.model_h$model, 
                   coef=0.01, scale=2)

plotTSThresholdsAnomalies(data=my.model_h$timeseries$ts, 
                          thresholds = RES1$th_plot, 
                          anomalies = RES1$anomalies,
                          type_th=my.model_h$model$type_th)


RES2=find.anomalies(data = my.model_l$timeseries$ts, 
                   ts_par = my.model_l$timeseries$ts_par, 
                   ad.model = my.model_l$model, 
                   coef=-0.4, scale=3)

plotTSThresholdsAnomalies(data=my.model_l$timeseries$ts, 
                          thresholds = RES2$th_plot, 
                          anomalies = RES2$anomalies,
                          type_th=my.model_l$model$type_th)


plotTSThresholdsAnomalies_both(data=my.model_l$timeseries$ts, 
                          thresholds = list(RES2$th_plot, RES1$th_plot), 
                          anomalies = c(RES2$anomalies, RES1$anomalies))



#++++ Expert + Correction ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

RES=find.anomalies(data = ts_train$time_series$ts, 
                   ts_par = ts_train$time_seriestest$ts_par, 
                   ad.model = my.model$model, 
                   coef=0.01, scale=2)

plotTSThresholdsAnomalies(data=ts_train$time_series$ts, thresholds = RES$th_plot, anomalies = RES$anomalies)

#=====================================================================================================================


