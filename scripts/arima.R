#########################
### Imports and setup ###
#########################

#setwd('..')

library(tidyverse)
library(tidymodels)
library(doParallel)
library(modeltime)
library(timetk)

source('./scripts/load_data.R')
source('./scripts/analysis.R')

PARALLEL <- T

set.seed(843)

# parallel
if(PARALLEL){
  cl <- makePSOCKcluster(8)
  registerDoParallel(cl)
}

#########################
####### Load Data #######
#########################

cv_plots <- c()
res_plots <- c()

## Create 2 plots
for(i in 1:2){

## Load data
data <- load_data()
train <- data$train
test <- data$test

## Set up preprocessing
prepped_recipe <- setup_recipe(train)

## Bake recipe
bake(prepped_recipe, new_data=train)
bake(prepped_recipe, new_data=test)

#########################
## Fit Regression Model #
#########################

# P's are autogregressive (longer term)
# Q's are moving average (shorter term)
# D's model residuals vs day-to-day difference

## Define model
arima_model <- arima_reg(
  seasonal_period=365, #365
  non_seasonal_ar=5, # default max p to tune
  non_seasonal_ma=5, # default max q to tune
  seasonal_ar=2, # default max P to tune
  seasonal_ma=2, #default max Q to tune
  non_seasonal_differences=2, # default max d to tune
  seasonal_differences=2 #default max D to tune
  ) %>%
  set_engine("auto_arima")

## Create CV splits
cv_split <- time_series_split(train, assess="6 months", cumulative = TRUE)

## Visualize CV split
cv_split %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(date, sales, .interactive=FALSE)

## Define workflow
arima_wf <- workflow() %>%
  add_recipe(prepped_recipe) %>%
  add_model(arima_model) %>%
  fit(data=training(cv_split))

## Cross-validate to tune model
cv_results <- modeltime_calibrate(arima_wf, new_data = testing(cv_split))

## Visualize CV results
cv_plot <- cv_results %>%
  modeltime_forecast(
    new_data = testing(cv_split),
    actual_data = train) %>%
  plot_modeltime_forecast(.interactive=FALSE)

cv_plots[[i]] <- cv_plot

## Evaluate CV accuracy
cv_results %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(.interactive = FALSE)

## Refit to all data
model_fullfit <- cv_results %>%
  modeltime_refit(data = train)

## Forecast new data
forecast <- model_fullfit %>%
  modeltime_forecast(
    new_data = test,
    actual_data = train)
# %>%
#   rename(date=.index, sales=.value) %>%
#   select(date, sales) %>%
#   full_join(., y=test, by="date") %>%
#   select(id, sales)

## Visualize forecast
res_plot <- forecast %>%
  plot_modeltime_forecast(.interactive=FALSE)  +
  xlab(paste('Forecast',i))

res_plots[[i]] <- res_plot
}

plotly::subplot(cv_plots[[1]],cv_plots[[2]],res_plots[[1]],res_plots[[2]], nrows=2)

if(PARALLEL){
  stopCluster(cl)
}
