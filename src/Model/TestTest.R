source("src/Model/aggregate.thresholds.R")
source("src/Model/app_timeseries.R")
source("src/Model/dynamicThreshold.model.R")
source("src/Model/dynamicThreshold.smart.R")
source("src/Model/dynamicThreshold.smart_train.two_sides.R")
source("src/Model/findPeriod_ssa.R")
source("src/Model/find_Thresh.R")
source("src/Model/find_Thresh_set.R")
source("src/Model/plotfirstPeriods.R")
source("src/Model/plotThresholdsAnomalies.R")
source("src/Model/timeseries_prepare.R")
source("src/Model/find.anomalies.R")
source("src/Model/find.anomalies.two_sides.R")

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



#==== Smart Train ====================================================================================================

#++++ Simple + Medium ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

my.model=dynamicThreshold.smart_train(ts.agg  = ts.agg, 
                                      type_th = "high", 
                                      train.params = list(mode = 'simple', sensitivity = 'Medium'))

plotTSThresholdsAnomalies(ts.agg = ts.agg, 
                          thresholds = my.model$ad_results$th_plot, 
                          anomalies = my.model$ad_results$anomalies,
                          type_th=my.model$model$type_th)

#++++ Simple + Medium ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

my.model=dynamicThreshold.smart_train(ts.agg  = ts.agg, 
                                      periods = c(100, 800),
                                      type_th = "high", 
                                      train.params = list(mode = 'simple', sensitivity = 'Medium'))

plotTSThresholdsAnomalies(ts.agg = ts.agg, 
                          thresholds = my.model$ad_results$th_plot, 
                          anomalies = my.model$ad_results$anomalies,
                          type_th=my.model$model$type_th)


#++++ Simple + Medium + Correction ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

RES=find.anomalies(ts.agg = my.model$raw_timeseries, 
                    ad.model = my.model$model, 
                    coef=0.1, scale=2)

plotTSThresholdsAnomalies(ts.agg = ts.agg, 
                          thresholds = RES$th_plot, 
                          anomalies = RES$anomalies,
                          type_th=my.model$model$type_th)


#++++ Expert  +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#++++ High Thresholds  +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

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

#++++ Low Thresholds  +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
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

  plotTSThresholdsAnomalies(ts.agg=ts.agg, 
                          thresholds = my.model_h$ad_results$th_plot, 
                          anomalies = my.model_h$ad_results$anomalies,
                          type_th=my.model_h$model$type_th)

plotTSThresholdsAnomalies(ts.agg=ts.agg, 
                          thresholds = my.model_l$ad_results$th_plot, 
                          anomalies = my.model_l$ad_results$anomalies,
                          type_th=my.model_l$model$type_th)

#++++ Expert + Correction ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


RES1=find.anomalies(ts.agg = ts.agg,
                   ad.model = my.model_h$model, 
                   coef=0.01, scale=2)

plotTSThresholdsAnomalies(ts.agg = ts.agg,
                          thresholds = RES1$th_plot, 
                          anomalies = RES1$anomalies,
                          type_th=my.model_h$model$type_th)


RES2=find.anomalies(ts.agg = ts.agg,
                   ad.model = my.model_l$model, 
                   coef=-0.4, scale=3)

plotTSThresholdsAnomalies(ts.agg = ts.agg,
                          thresholds = RES2$th_plot, 
                          anomalies = RES2$anomalies,
                          type_th=my.model_l$model$type_th)

#++++ Plot both thresholds ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

plotTSThresholdsAnomalies_both(ts.agg = ts.agg, 
                          thresholds = list(RES2$th_plot, RES1$th_plot), 
                          anomalies = c(RES2$anomalies, RES1$anomalies))

