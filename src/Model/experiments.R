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
source("src/Model/DTModelTrain.R")
source("src/Model/DTModelApply.R")
source("src/Model/DTModelTune.R")
source("src/Model/anomalies.analysis.R")
source("src/timeSliders.R")
source("src/visualization.R")

source("src/Model/ThAnomaliesViewPrep.R")
source("src/prophet/prophet_model.R")

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



# ======== aggregation ====== 

# ======== aggregation ====== 
ts_type="days"
ts_val=1
func='median'
data.agg=aggregation_data(data_na.rm, type = paste(ts_val, ts_type), func_aggregate = func, quantile_percent = .5)

data.agg.train = data.agg[1:(floor(length(data.agg)*0.8))]
data.agg.test = data.agg[(floor(length(data.agg)*0.8)+1):length(data.agg)]


# Time Series for model Training and Applying 

#time series with aggregation parameters (type, value and function)
ts.agg.train=list(data.agg=data.agg.train, ts_type=ts_type, ts_val=ts_val, ts_func = func)
ts.agg.test=list(data.agg=data.agg.test, ts_type=ts_type, ts_val=ts_val, ts_func = func)
ts.agg=list(data.agg=data.agg, ts_type=ts_type, ts_val=ts_val, ts_func = func)


#------------- prophet --------------------
data_train <- ts.agg.train$data.agg
data_test <- ts.agg.test$data.agg

fit <- model_prophet(data_train)
fit_test <- model_prophet_test(fit, data_test)
res <- model_prophet_new_interval(fit_test, percent_up=-30, percent_low=-30, method='both')
plot_time_series(res, treshhold_type = 'both')

an.analysis.res = anomalies.stat(ad_results = res, data, ts_type = ts_type, ts_val = ts_val)
anomalies.detail(an.analysis.res$ad_res[2,], data, ts_func = func)



#------------- DT  --------------------


anomalies_type = "Both"
full.model = dynamicThreshold.train(ts.agg  = ts.agg.train, 
                                    train.params = list(mode = 'simple',sensitivity = 'Low'),
                                    type_th = anomalies_type)

RES = dynamicThreshold.apply(ts.agg = ts.agg.test,model = full.model,
                             type_th = anomalies_type, 
                             correction =list("Low" = c(coef = 0, scale = 1),
                                              "High" = c(coef = 0, scale = 1)))

plot_time_series(RES, treshhold_type = tolower(anomalies_type))

an.analysis.res = anomalies.stat(ad_results = RES, data, ts_type = ts_type, ts_val = ts_val)
anomalies.detail(an.analysis.res$ad_res[2,], data, ts_func = func)



coeff = dynamicThreshold.autoturn(ts.agg = ts.agg.test,model = full.model,type_th = anomalies_type, p_anomalies = 0.03)
RES = dynamicThreshold.apply(ts.agg = ts.agg.test,model = full.model,
                             type_th = anomalies_type, 
                             correction =list("Low" = c(coef = coeff$Low[1], scale = coeff$Low[2]),
                                              "High" = c(coef = coeff$High[1], scale = coeff$High[2])))

plot_time_series(RES, treshhold_type = tolower(anomalies_type))

an.analysis.res = anomalies.stat(ad_results = RES, data, ts_type = ts.agg.test$ts_type, ts_val = ts.agg.test$ts_val)
indx = which(an.analysis.res$ad_res$significance=="High")
anomalies.detail(an.analysis.res$ad_res[indx[1],], data, ts_func = ts.agg.test$ts_func)



#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# ======== aggregation ====== 
ts_type="hours"
ts_val=1
func='median'
data.agg=aggregation_data(data_na.rm, type = paste(ts_val, ts_type), func_aggregate = func, quantile_percent = .5)

data.agg.train = data.agg[1:(floor(length(data.agg)*0.8))]
data.agg.test = data.agg[(floor(length(data.agg)*0.8)+1):length(data.agg)]


# Time Series for model Training and Applying 

#time series with aggregation parameters (type, value and function)
ts.agg.train=list(data.agg=data.agg.train, ts_type=ts_type, ts_val=ts_val, ts_func = func)
ts.agg.test=list(data.agg=data.agg.test, ts_type=ts_type, ts_val=ts_val, ts_func = func)
ts.agg=list(data.agg=data.agg, ts_type=ts_type, ts_val=ts_val, ts_func = func)


#------------- prophet --------------------
data_train <- ts.agg.train$data.agg
data_test <- ts.agg.test$data.agg

fit <- model_prophet(data_train)
fit_test <- model_prophet_test(fit, data_test)
res <- model_prophet_new_interval(fit_test, percent_up=-30, percent_low=-30, method='both')
plot_time_series(res, treshhold_type = 'both')

an.analysis.res = anomalies.stat(ad_results = res, data, ts_type = ts_type, ts_val = ts_val)
anomalies.detail(an.analysis.res$ad_res[2,], data, ts_func = func)



#------------- DT  --------------------


anomalies_type = "Both"
full.model = dynamicThreshold.train(ts.agg  = ts.agg.train, 
                                    train.params = list(mode = 'simple',sensitivity = 'Low'),
                                    type_th = anomalies_type)

RES = dynamicThreshold.apply(ts.agg = ts.agg.test,model = full.model,
                             type_th = anomalies_type, 
                             correction =list("Low" = c(coef = 0, scale = 1),
                                              "High" = c(coef = 0, scale = 1)))


plot_time_series(RES, treshhold_type = tolower(anomalies_type))

an.analysis.res = anomalies.stat(ad_results = RES, data, ts_type = ts_type, ts_val = ts_val)
anomalies.detail(an.analysis.res$ad_res[2,], data, ts_func = func)



coeff = dynamicThreshold.autoturn(ts.agg = ts.agg.test,model = full.model,type_th = anomalies_type, p_anomalies = 0.004)
RES = dynamicThreshold.apply(ts.agg = ts.agg.test,model = full.model,
                             type_th = anomalies_type, 
                             correction =list("Low" = c(coef = coeff$Low[1], scale = coeff$Low[2]),
                                              "High" = c(coef = coeff$High[1], scale = coeff$High[2])))

plot_time_series(RES, treshhold_type = tolower(anomalies_type))

an.analysis.res = anomalies.stat(ad_results = RES, data, ts_type = ts.agg.test$ts_type, ts_val = ts.agg.test$ts_val)
indx = which(an.analysis.res$ad_res$significance=="High")
anomalies.detail(an.analysis.res$ad_res[indx[1],], data, ts_func = ts.agg.test$ts_func)

