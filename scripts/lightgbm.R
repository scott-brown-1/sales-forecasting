#########################
### Imports and setup ###
#########################

#setwd('..')

library(tidyverse)
library(tidymodels)
library(bonsai)
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

## Load data
data <- load_data()
train <- data$train
test <- data$test

## Set up preprocessing
prepped_recipe <- setup_recipe(train)

## Bake recipe
bake(prepped_recipe, new_data=train)
bake(prepped_recipe, new_data=test)

## Explore data
viz <- bake(prepped_recipe, train)
train %>%
  timetk::plot_time_series(date, sales, .interactive=F) +
  geom_line(data=viz, aes(x=date,y=sin_scaled, color='red'))

#########################
## Fit Regression Model #
#########################

## Define model
boost_model <- boost_tree(
  trees = 200,#tune(), #200
  tree_depth = 15,#tune(), #15
  learn_rate = 0.05,#tune(), #0.1,
  mtry = 5,#tune(), #3,
  min_n = 30, #tune(), #20,
  loss_reduction = 0 #tune(), #0
) %>% 
  set_engine("lightgbm") %>% 
  set_mode("regression")

## Define workflow
boost_wf <- workflow(prepped_recipe) %>%
  add_model(boost_model)

## Grid of values to tune over
tuning_grid <- grid_regular(
  #trees(),
  tree_depth(range=c(10,15)),
  #learn_rate(),
  #mtry(range=c(4,5)),#ncol(train))),
  #min_n(),
  #loss_reduction(),
  levels = 2)

## Split data for CV
folds <- vfold_cv(train, v = 2, repeats=1)

## Run the CV
cv_results <- boost_wf %>%
  tune_grid(resamples=folds,
            grid=tuning_grid,
            metrics=metric_set(smape))

## Find optimal tuning params
best_params <- cv_results %>%
  select_best('smape')

print(best_params)

## Find best tune results
best_smape <- cv_results %>%
  collect_metrics() %>%
  pull(mean) %>%
  min()

print(best_smape)

