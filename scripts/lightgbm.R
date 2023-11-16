#########################
### Imports and setup ###
#########################

library(tidyverse)
library(tidymodels)
library(bonsai)
library(doParallel)

source('./scripts/load_data.R')
source('./scripts/analysis.R')

PARALLEL <- F

set.seed(843)

# parallel
if(PARALLEL){
  cl <- makePSOCKcluster(10)
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
# train %>%
#   timetk::plot_time_series(date, sales, .interactive=F) +
#   geom_line(data=viz, aes(x=date,y=sin_scaled, color='red'))

#########################
## Fit Regression Model #
#########################

## Define model
boost_model <- boost_tree(
  trees = tune(), #200
  tree_depth = tune(), #15
  learn_rate = tune(), #0.1,
  mtry = tune(), #3,
  min_n = 20, #tune(), #20,
  loss_reduction = 0 #tune(), #0
) %>% 
  set_engine("lightgbm") %>% 
  set_mode("regression")

## Define workflow
boost_wf <- workflow(prepped_recipe) %>%
  add_model(boost_model)

## Grid of values to tune over
tuning_grid <- grid_regular(
  trees(),
  tree_depth(),
  learn_rate(),
  mtry(range=c(3,ncol(train))),
  #min_n(),
  #loss_reduction(),
  levels = 6)

## Split data for CV
folds <- vfold_cv(train, v = 6, repeats=1)

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

