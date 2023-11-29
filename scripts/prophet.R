#########################
### Imports and setup ###
#########################

#setwd('..')

## NOTE: Exponential smoothing and ARIMA often work better 
# for monthly data than for daily data

library(tidyverse)
library(tidymodels)
library(modeltime)
library(timetk)
library(doParallel)

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
  
  #########################
  # Fit Time Series Model #
  #########################
  
  ## Create CV splits
  cv_split <- time_series_split(train, assess="12 months", cumulative = TRUE)
  
  ## Visualize CV split
  cv_split %>%
    tk_time_series_cv_plan() %>%
    plot_time_series_cv_plan(date, sales, .interactive=FALSE)
  
  ## Define model
  prophet_model <- prophet_reg() %>%
    set_engine('prophet') %>%
    fit(sales~date, data=training(cv_split))
  
  ## Cross-validate to tune model
  cv_results <- modeltime_calibrate(prophet_model, new_data = testing(cv_split))
  
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
      h = '12 months',
      new_data = test,
      actual_data = train)
  
  ## Visualize forecast
  res_plot <- forecast %>%
    plot_modeltime_forecast(.interactive=FALSE)  +
    xlab(paste('Forecast',i))
  
  res_plots[[i]] <- res_plot
}

plotly::subplot(cv_plots[[1]],cv_plots[[2]],res_plots[[1]],res_plots[[2]], nrows=2)
