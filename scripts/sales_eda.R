library(tidyverse)
library(timetk)
library(patchwork)

train <- vroom::vroom('./data/train.csv')
test <- vroom::vroom('./data/test.csv')

n_stores <- max(train$store)
n_items <- max(train$item)

patch_plot <- NULL

for(s in 1:2){#n_stores){
  for(i in 1:2){#n_items){
    print(paste(s,i))

    store_item_train <- train %>%
      filter(store==s, item==i)
    
    store_item_test <- test %>%
      filter(store==s, item==i)
    
    single_plot <- store_item_train %>%
      pull(sales) %>%
      forecast::ggAcf(.)
    
    if(is.null(patch_plot)){
      patch_plot <- single_plot
        #plot_time_series(date, sales, .interactive=FALSE)
    }else{
      patch_plot <- patch_plot + single_plot
          #plot_time_series(date, sales, .interactive=FALSE)
    }

    ## Fit storeItem models here

    ## Predict storeItem sales

    ## Save storeItem predictions
    # if(s==1 & i==1){
    #   all_preds <- preds
    # } else {
    #   all_preds <- bind_rows(all_preds, preds)
    # }
  }
}

patch_plot
