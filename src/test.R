### Testing transforming functions


source("src/aggregation.R")

# count_non_zero

count_non_zero(c(-1, 0, 2, 0, 4, 0, 0))

# aggregation_data

harMet_15Min <- read.csv(
    file="data/HARV/FisherTower-Met/hf001-10-15min-m.csv",
    stringsAsFactors = FALSE) %>% mutate(datetime =as.POSIXct(datetime,format="%Y-%m-%dT%H:%M") ) %>%
    na.omit(datetime) %>% select(datetime, airt)

head(harMet_15Min)

data <- xts(x = harMet_15Min$airt, order.by = harMet_15Min$datetime)
head(data, 10)

aggregation_data(data, type = '3 weeks', func_aggregate = 'n_quantile', quantile_percent = .9) %>% head()
aggregation_data(data, type = '3 weeks', func_aggregate = 'median', quantile_percent = .5) %>% head()
aggregation_data(data, type = '3 weeks', func_aggregate = 'count_non_zero') %>% head(10)


source("src/denoise.R")

# denoise_data

HH_data <- readRDS("data/household_power_consumption_datatime.rds")
data <- xts(x = HH_data$Voltage, order.by = HH_data$datetime)
data <- data %>% remove_na_from_data(type = "mean") %>%
                aggregation_data(type = "6 hours", func_aggregate = 'median')
ttest <- denoise_data(data, type = 'wma')
plot(data)
plot(ttest)

ttest <- denoise_data(data, type = 'wma', window_noise = 10)
plot(data)
plot(ttest)

n  = 50
x  = seq(-pi,pi,length.out=n)
y  = sin(x) + (runif(length(x))*0.1) #NOISY DATA
ys = smth(y,window = .1,method = "gaussian") #SMOOTHING
plot(x,y,type="l",col="red")
lines(x,ys,col="black",lwd=3)
title("Example Smoothing of Noisy Data")

source("src/remove_na_values.R")

# remove_na_from_data

n = 15
full.dates = seq(Sys.Date(), by = 'day', length = n)
y = rnorm(n)
serie = zoo(y, full.dates)
data <- as.xts(serie)
data[c(2, 6:7, 10)] <- NA
data
remove_na_from_data(data, type = 'linear')


source("src/remove_outliers.R")
source("src/visualization.R")

# remove_outliers_from_data


n = 30
full.dates = seq(Sys.Date(), by = 'day', length = n)
y = c(-2,-1,0,1,2,3,4,2,3,-2,-1,0,1,2,3,4,2,3-2,-1,0,1,2,3,4,2,3,1,0,1500)
serie = zoo(y, full.dates)
data <- as.xts(serie)
number <- 1
data_diff <- remove_outliers_from_data(data, number = 1, type = 'diff')
data %>% dim()
length(data_diff)

plot_time_series(data)
plot_time_series(data_diff)


# plot_time_series

# testing 1 column

harMet_15Min <- read.csv(
    file="data/HARV/FisherTower-Met/hf001-10-15min-m.csv",
    stringsAsFactors = FALSE) %>% mutate(datetime =as.POSIXct(datetime,format="%Y-%m-%dT%H:%M") ) %>%
    na.omit(datetime) %>% select(datetime, airt)

head(harMet_15Min)

data <- xts(x = harMet_15Min$airt, order.by = harMet_15Min$datetime) %>%
    aggregation_data(type = '1 days', func_aggregate = 'median')

plot_time_series(data)


# testing 1 column with window on anomalies

data <- xts(x = harMet_15Min$airt, order.by = harMet_15Min$datetime)

data_agg <- data %>%
    aggregation_data(type = '1 days', func_aggregate = 'median')

fit <- model_prophet_train(data_agg)
res <- model_prophet_new_interval(fit, percent_up=0, percent_low=0, method='both')
plot_time_series(res, treshhold_type = 'both')

index_anomalies <-  144
window_data <- c(index(res)[index_anomalies - 1], index(res)[index_anomalies])


plot_time_series(data, window_size = 0.9)
plot_time_series(data, window_size = window_data)

# testing 1 column with train-test split

plot_time_series(data_agg, window_size = 0.9)
plot_time_series(data_agg, window_size = 0, train_test_split = 0.95)


# testing 4 column

HH_data <- readRDS("data/household_power_consumption_datatime.rds")

#create time series
data <- xts(x = HH_data$Voltage, order.by = HH_data$datetime)

#remove NA
data_na.rm=remove_na_from_data(data, type = "mean")

#aggregation
ts_type="days"
ts_val=1
data.agg=aggregation_data(data_na.rm, type = paste(ts_val, ts_type), func_aggregate = 'median', quantile_percent = .5)



data.agg.train = data.agg[1:1200]
data.agg.test = data.agg[1201:length(data.agg)]


# Time Series for model Training and Applying

ts.agg.train=list(data.agg=data.agg.train, ts_type=ts_type, ts_val=ts_val)
ts.agg.test=list(data.agg=data.agg.test, ts_type=ts_type, ts_val=ts_val)
ts.agg=list(data.agg=data.agg, ts_type=ts_type, ts_val=ts_val)


#==== Train ====================================================================================================

#++++ Simple + Medium ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

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
library(plotly)
library(Rssa)
library(parallel)


RES.h=find.anomalies(ts.agg  = ts.agg.test, 
                     ad.model = my.model.h$model, 
                     coef=0.0, scale=1)

RES.l=find.anomalies(ts.agg  = ts.agg.test, 
                     ad.model = my.model.l$model, 
                     coef=0.4, scale=1.2)


xts.vis = TSThresholdsAnomaliesVis_both(ts.agg = ts.agg.test, 
                                        thresholds = list(RES.l$thresholds, RES.h$thresholds),
                                        anomalies = c(RES.l$anomalies,RES.h$anomalies))

plot_time_series(xts.vis, treshhold_type = 'both')


#testing 3 columns with High or Low treshholds

xts.vis = TSThresholdsAnomaliesVis(ts.agg=my.model.l$raw_timeseries, 
                                   thresholds = my.model.l$ad_results$thresholds, 
                                   anomalies = my.model.l$ad_results$anomalies)
plot_time_series(xts.vis, treshhold_type = 'low')

xts.vis = TSThresholdsAnomaliesVis(ts.agg=my.model.h$raw_timeseries, 
                                   thresholds = my.model.h$ad_results$thresholds, 
                                   anomalies = my.model.h$ad_results$anomalies)
plot_time_series(xts.vis, treshhold_type = 'high')


### train_test_split

source('src/train_test_split.R')

dim(data)

fit <- train_test_split_time_series(data, split_data = .5)
fit$train %>% dim()
fit$test %>% dim()

fit$train %>% tail()
fit$test %>% head()

fit$plot
