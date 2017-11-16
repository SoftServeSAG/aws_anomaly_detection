source("src/Model/InitDT.R")

source("src/timeSliders.R")
source("src/visualization.R")


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
HH_data <- read.csv("data/HH_data3.csv")
HH_data$datetime=as.POSIXct(as.character(HH_data$datetime), format ="%Y-%m-%d %H:%M:%S")

#detect appropriate time sliders for aggregation
TSliders = timeSliders(HH_data$datetime)
#create time series
data <- xts(x = HH_data$Voltage, order.by = HH_data$datetime)
#remove NA
data_na.rm=remove_na_from_data(data, type = "mean")



# ======== aggregation ====== 

# ======== aggregation ====== 
ts_type="days"
ts_val=3
func='median'
data.agg=aggregation_data(data_na.rm, type = paste(ts_val, ts_type), func_aggregate = func, quantile_percent = .5)
ts.agg=list(data.agg=data.agg, ts_type=ts_type, ts_val=ts_val, ts_func = func)

anomalies_type = "Both"
full.model = dynamicThreshold.train(ts.agg  = ts.agg, 
                                    train.params = list(mode = 'simple',sensitivity = 'Medium'),
                                    type_th = anomalies_type)

RES = dynamicThreshold.apply(ts.agg = ts.agg, model = full.model,
                             type_th = anomalies_type, 
                             correction =list("Low" = c(coef = 0, scale = 1),
                                              "High" = c(coef = 0, scale = 1)))

plot_time_series(RES, treshhold_type = tolower(anomalies_type))

an.analysis.res = anomalies.stat(ad_results = RES, data, ts_type = ts.agg$ts_type, ts_val = ts.agg$ts_val)
anomalies.detail(an.analysis.res$ad_res, data, ts_func = ts.agg$ts_func)

coeff = dynamicThreshold.autoturn(ts.agg = ts.agg, model = full.model,type_th = anomalies_type, p_anomalies = 0.01)
RES1 = dynamicThreshold.apply(ts.agg = ts.agg, model = full.model,
                              type_th = anomalies_type, 
                              correction =list("Low" = c(coef = coeff$Low[1], scale = coeff$Low[2]),
                                               "High" = c(coef = coeff$High[1], scale = coeff$High[2])))

plot_time_series(RES1, treshhold_type = tolower(anomalies_type))

an.analysis.res = anomalies.stat(ad_results = RES1, data, ts_type = ts.agg$ts_type, ts_val = ts.agg$ts_val)
anomalies.detail(an.analysis.res$ad_res[1,], data, ts_func = ts.agg$ts_func)

# ======== aggregation ====== 
ts_type="days"
ts_val=1
func='median'
data.agg=aggregation_data(data_na.rm, type = paste(ts_val, ts_type), func_aggregate = func, quantile_percent = .5)
ts.agg=list(data.agg=data.agg, ts_type=ts_type, ts_val=ts_val, ts_func = func)

anomalies_type = "Both"
full.model = dynamicThreshold.train(ts.agg  = ts.agg, 
                                    train.params = list(mode = 'simple',sensitivity = 'Medium'),
                                    type_th = anomalies_type)

RES = dynamicThreshold.apply(ts.agg = ts.agg, model = full.model,
                             type_th = anomalies_type, 
                             correction =list("Low" = c(coef = 0, scale = 1),
                                              "High" = c(coef = 0, scale = 1)))

plot_time_series(RES, treshhold_type = tolower(anomalies_type))

an.analysis.res = anomalies.stat(ad_results = RES, data, ts_type = ts.agg$ts_type, ts_val = ts.agg$ts_val)
anomalies.detail(an.analysis.res$ad_res[1,], data, ts_func = ts.agg$ts_func)

coeff = dynamicThreshold.autoturn(ts.agg = ts.agg, model = full.model,type_th = anomalies_type, p_anomalies = 0.03)
RES1 = dynamicThreshold.apply(ts.agg = ts.agg, model = full.model,
                              type_th = anomalies_type, 
                              correction =list("Low" = c(coef = coeff$Low[1], scale = coeff$Low[2]),
                                               "High" = c(coef = coeff$High[1], scale = coeff$High[2])))

plot_time_series(RES1, treshhold_type = tolower(anomalies_type))

an.analysis.res = anomalies.stat(ad_results = RES1, data, ts_type = ts.agg$ts_type, ts_val = ts.agg$ts_val)
anomalies.detail(an.analysis.res$ad_res[1,], data, ts_func = ts.agg$ts_func)


# ======== aggregation ====== 
ts_type="hours"
ts_val=2
func='median'
data.agg=aggregation_data(data_na.rm, type = paste(ts_val, ts_type), func_aggregate = func, quantile_percent = .5)
ts.agg=list(data.agg=data.agg, ts_type=ts_type, ts_val=ts_val, ts_func = func)

anomalies_type = "Both"
full.model = dynamicThreshold.train(ts.agg  = ts.agg, 
                                    train.params = list(mode = 'simple',sensitivity = 'Medium'),
                                    type_th = anomalies_type)

RES = dynamicThreshold.apply(ts.agg = ts.agg, model = full.model,
                             type_th = anomalies_type, 
                             correction =list("Low" = c(coef = 0, scale = 1),
                                              "High" = c(coef = 0, scale = 1)))

plot_time_series(RES, treshhold_type = tolower(anomalies_type))

an.analysis.res = anomalies.stat(ad_results = RES, data, ts_type = ts.agg$ts_type, ts_val = ts.agg$ts_val)
anomalies.detail(an.analysis.res$ad_res[1,], data, ts_func = ts.agg$ts_func)

coeff = dynamicThreshold.autoturn(ts.agg = ts.agg, model = full.model,type_th = anomalies_type, p_anomalies = 0.03)
RES1 = dynamicThreshold.apply(ts.agg = ts.agg, model = full.model,
                              type_th = anomalies_type, 
                              correction =list("Low" = c(coef = coeff$Low[1], scale = coeff$Low[2]),
                                               "High" = c(coef = coeff$High[1], scale = coeff$High[2])))

plot_time_series(RES1, treshhold_type = tolower(anomalies_type))

an.analysis.res = anomalies.stat(ad_results = RES1, data, ts_type = ts.agg$ts_type, ts_val = ts.agg$ts_val)
anomalies.detail(an.analysis.res$ad_res[1,], data, ts_func = ts.agg$ts_func)


# ======== aggregation ====== 
ts_type="minutes"
ts_val=7
func='median'
data.agg=aggregation_data(data_na.rm, type = paste(ts_val, ts_type), func_aggregate = func, quantile_percent = .5)
ts.agg=list(data.agg=data.agg, ts_type=ts_type, ts_val=ts_val, ts_func = func)

anomalies_type = "Both"
full.model = dynamicThreshold.train(ts.agg  = ts.agg, 
                                    train.params = list(mode = 'simple',sensitivity = 'Medium'),
                                    type_th = anomalies_type)

RES = dynamicThreshold.apply(ts.agg = ts.agg, model = full.model,
                             type_th = anomalies_type, 
                             correction =list("Low" = c(coef = 0, scale = 1),
                                              "High" = c(coef = 0, scale = 1)))

plot_time_series(RES, treshhold_type = tolower(anomalies_type))

an.analysis.res = anomalies.stat(ad_results = RES, data, ts_type = ts.agg$ts_type, ts_val = ts.agg$ts_val)
anomalies.detail(an.analysis.res$ad_res[1,], data, ts_func = ts.agg$ts_func)

coeff = dynamicThreshold.autoturn(ts.agg = ts.agg, model = full.model,type_th = anomalies_type, p_anomalies = 0.03)
RES1 = dynamicThreshold.apply(ts.agg = ts.agg, model = full.model,
                              type_th = anomalies_type, 
                              correction =list("Low" = c(coef = coeff$Low[1], scale = coeff$Low[2]),
                                               "High" = c(coef = coeff$High[1], scale = coeff$High[2])))

plot_time_series(RES1, treshhold_type = tolower(anomalies_type))

an.analysis.res = anomalies.stat(ad_results = RES1, data, ts_type = ts.agg$ts_type, ts_val = ts.agg$ts_val)
anomalies.detail(an.analysis.res$ad_res[1,], data, ts_func = ts.agg$ts_func)




# ======== aggregation ====== 
ts_type="days"
ts_val=3
func='median'
data.agg=aggregation_data(data_na.rm, type = paste(ts_val, ts_type), func_aggregate = func, quantile_percent = .5)
data.agg.train = data.agg[1:(floor(length(data.agg)*0.8))]
data.agg.test = data.agg[(floor(length(data.agg)*0.8)+1):length(data.agg)]
ts.agg.train=list(data.agg=data.agg.train, ts_type=ts_type, ts_val=ts_val, ts_func = func)
ts.agg.test=list(data.agg=data.agg.test, ts_type=ts_type, ts_val=ts_val, ts_func = func)
ts.agg=list(data.agg=data.agg, ts_type=ts_type, ts_val=ts_val, ts_func = func)

anomalies_type = "Both"
full.model = dynamicThreshold.train(ts.agg  = ts.agg.train, 
                                    train.params = list(mode = 'simple',sensitivity = 'Medium'),
                                    type_th = anomalies_type)

RES = dynamicThreshold.apply(ts.agg = ts.agg.train, model = full.model,
                             type_th = anomalies_type, 
                             correction =list("Low" = c(coef = 0, scale = 1),
                                              "High" = c(coef = 0, scale = 1)))

plot_time_series(RES, treshhold_type = tolower(anomalies_type))

an.analysis.res = anomalies.stat(ad_results = RES, data, ts_type = ts.agg$ts_type, ts_val = ts.agg$ts_val)
anomalies.detail(an.analysis.res$ad_res[1,], data, ts_func = func)



coeff = dynamicThreshold.autoturn(ts.agg = ts.agg, model = full.model,type_th = anomalies_type, p_anomalies = 0.03)
RES1 = dynamicThreshold.apply(ts.agg = ts.agg, model = full.model,
                             type_th = anomalies_type, 
                             correction =list("Low" = c(coef = coeff$Low[1], scale = coeff$Low[2]),
                                              "High" = c(coef = coeff$High[1], scale = coeff$High[2])))

plot_time_series(RES1, treshhold_type = tolower(anomalies_type))

an.analysis.res = anomalies.stat(ad_results = RES1, data, ts_type = ts.agg$ts_type, ts_val = ts.agg$ts_val)
indx = which(an.analysis.res$ad_res$significance=="Low")
anomalies.detail(an.analysis.res$ad_res[indx[5],], data, ts_func = ts.agg$ts_func)



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



coeff = dynamicThreshold.autoturn(ts.agg = ts.agg.test,model = full.model,type_th = anomalies_type, p_anomalies = 0.02)
RES = dynamicThreshold.apply(ts.agg = ts.agg.test,model = full.model,
                             type_th = anomalies_type, 
                             correction =list("Low" = c(coef = coeff$Low[1], scale = coeff$Low[2]),
                                              "High" = c(coef = coeff$High[1], scale = coeff$High[2])))

plot_time_series(RES, treshhold_type = tolower(anomalies_type))

an.analysis.res = anomalies.stat(ad_results = RES, data, ts_type = ts.agg.test$ts_type, ts_val = ts.agg.test$ts_val)
indx = which(an.analysis.res$ad_res$significance=="High")
anomalies.detail(an.analysis.res$ad_res[indx[1],], data, ts_func = ts.agg.test$ts_func)

