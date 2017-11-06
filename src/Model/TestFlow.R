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
source("src/Model/anomalies.analysis.R")
source("src/Model/timeSliders.R")
source("src/Model/ThAnomaliesViewPrep.R")
source("src/Model/anomalies.stat.R")


source("src/aggregation.R")
source("src/denoise.R")
source("src/remove_na_values.R")
source("src/remove_outliers.R")

#source("Preprocessing.R")
library(plotly)
library(Rssa)

library(parallel)



HH_data <- readRDS("data/household_power_consumption_datatime.rds")


#detect appropriate time sliders for aggregation
TSliders = timeSliders(HH_data$datetime)

#create time series
data <- xts(x = HH_data$Voltage, order.by = HH_data$datetime)


#remove NA
data_na.rm=remove_na_from_data(data, type = "mean")

#aggregation
ts_type="days"
ts_val=1
func='median'
data.agg=aggregation_data(data_na.rm, type = paste(ts_val, ts_type), func_aggregate = func, quantile_percent = .5)



data.agg.train = data.agg[1:1200]
data.agg.test = data.agg[1201:length(data.agg)]


# Time Series for model Training and Applying 

#time series with aggregation parameters (type, value and function)
ts.agg.train=list(data.agg=data.agg.train, ts_type=ts_type, ts_val=ts_val, ts_func = func)
ts.agg.test=list(data.agg=data.agg.test, ts_type=ts_type, ts_val=ts_val, ts_func = func)
ts.agg=list(data.agg=data.agg, ts_type=ts_type, ts_val=ts_val, ts_func = func)


#==== Train ====================================================================================================

#++++ Simple + Medium ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

my.model.h=dynamicThreshold.smart_train(ts.agg  = ts.agg.train, 
                                      type_th = "high", 
                                      train.params = list(mode = 'simple', sensitivity = 'Medium'))

my.model.l=dynamicThreshold.smart_train(ts.agg  = ts.agg.train, 
                                        type_th = "low", 
                                        train.params = list(mode = 'simple', sensitivity = 'Low'))

plotTSThresholdsAnomalies(ts.agg=my.model.h$raw_timeseries, 
                          thresholds = my.model.h$ad_results$thresholds, 
                          anomalies = my.model.h$ad_results$anomalies,
                          type_th=my.model.h$model$type_th)

plotTSThresholdsAnomalies(ts.agg=my.model.l$raw_timeseries, 
                          thresholds = my.model.l$ad_results$thresholds, 
                          anomalies = my.model.l$ad_results$anomalies,
                          type_th=my.model.l$model$type_th)

xts.vis = TSThresholdsAnomaliesVis(ts.agg=my.model.l$raw_timeseries, 
                          thresholds = my.model.l$ad_results$thresholds, 
                          anomalies = my.model.l$ad_results$anomalies)


#==== Apply ====================================================================================================


RES.h=find.anomalies(ts.agg  = ts.agg.test, 
                     ad.model = my.model.h$model, 
                     coef=0.0, scale=1)

RES.l=find.anomalies(ts.agg  = ts.agg.test, 
                     ad.model = my.model.l$model, 
                     coef=0.4, scale=1.2)

plotTSThresholdsAnomalies_both(ts.agg = ts.agg.test, 
                          thresholds = list(RES.l$thresholds, RES.h$thresholds),
                          anomalies = c(RES.l$anomalies,RES.h$anomalies))

xts.vis = TSThresholdsAnomaliesVis_both(ts.agg = ts.agg.test, 
                                   thresholds = list(RES.l$thresholds, RES.h$thresholds),
                                   anomalies = c(RES.l$anomalies,RES.h$anomalies))

anomalies.stat(AD_res = RES.h, ts.agg = ts.agg)
anomalies.stat(AD_res = RES.l, ts.agg = ts.agg)

ind_sign = which(RES.h$significance=="High")
anomalies.analysis(ts.agg  = ts.agg.test, ad_results = RES.h, k=ind_sign[1])
ind_sign = which(RES.l$significance=="High")
anomalies.analysis(ts.agg  = ts.agg.test, ad_results = RES.l, k=ind_sign[1])



#==== Train and Apply Two Sides  ====================================================================================================


full.model = dynamicThreshold.smart_train.two_sided(ts.agg  = ts.agg.train, 
                                                    train.params = list(mode = 'simple', sensitivity = 'Medium'))


RES = find.anomalies.two_sides(ts.agg = ts.agg,
                               low = list(model = full.model$low$model, coef = -0.4, scale = 1.5),
                               high = list(model = full.model$high$model, coef = 0, scale = 1))

plotTSThresholdsAnomalies_both(ts.agg=ts.agg, 
                               thresholds = RES$thresholds,
                               anomalies = RES$anomalies)

anomalies.stat(AD_res = RES, ts.agg = ts.agg)

ind_sign = which(RES$significance=="High")
anomalies.analysis(ts.agg  = ts.agg, ad_results = RES, k=ind_sign[1])

#========== Save and Load ========================

saveRDS(full.model,file="model.bin")

new.model = readRDS(file="model.bin")

RES = find.anomalies.two_sides(ts.agg = ts.agg,
                               low = list(model = new.model$low$model, coef = -0.4, scale = 1.5),
                               high = list(model = new.model$high$model, coef = 0, scale = 1))

plotTSThresholdsAnomalies_both(ts.agg=ts.agg, 
                               thresholds = RES$thresholds,
                               anomalies = RES$anomalies)


anomalies.stat(AD_res = RES, ts.agg = ts.agg)

ind_sign = which(RES$significance=="High")
anomalies.analysis(ts.agg  = ts.agg, ad_results = RES, k=ind_sign[1])



