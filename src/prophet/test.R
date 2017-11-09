# Testing prophet model

source("src/prophet/prophet_model.R")
source('src/visualization.R')
source('src/aggregation.R')

# model_prophet

harMet_15Min <- read.csv(
    file="data/HARV/FisherTower-Met/hf001-10-15min-m.csv",
    stringsAsFactors = FALSE) %>% mutate(datetime =as.POSIXct(datetime,format="%Y-%m-%dT%H:%M") ) %>%
    na.omit(datetime) %>% select(datetime, airt)

head(harMet_15Min)

data <- xts(x = harMet_15Min$airt, order.by = harMet_15Min$datetime) %>%
    aggregation_data(type = '2 days', func_aggregate = 'mean')

fit <- model_prophet(data)
fit$data %>% tail()
data %>% tail()

# model_prophet_new_interval

fit <- model_prophet(data, interval_width = 0.9)
fit$data %>% tail()

res <- model_prophet_new_interval(fit, percent_up=0, percent_low=50, method='both')
plot_time_series(res, treshhold_type = 'both')



# model_prophet_test


data <- xts(x = harMet_15Min$airt, order.by = harMet_15Min$datetime) %>%
    aggregation_data(type = '2 days', func_aggregate = 'mean')

data_train <- data[1:1500,]
data_test <- data[1501:length(data),]

tail(data_train)
head(data_test)

fit <- model_prophet(data_train)
fit_test <- model_prophet_test(fit, data_test)

res <- model_prophet_new_interval(fit_test, percent_up=0, percent_low=0, method='both')
plot_time_series(res, treshhold_type = 'both')

