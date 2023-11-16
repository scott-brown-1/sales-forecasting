load_data <- function(store_num=NULL, item_num=NULL){
  train_full <- vroom::vroom('./data/train.csv')
  test_full <- vroom::vroom('./data/test.csv')
  
  if(is.null(store_num)){
    store_num <- sample(1:max(train_full$store), 1)
  }
  
  if(is.null(item_num)){
    item_num <- sample(1:max(train_full$item), 1)
  }
  
  print(paste('Selected store:',store_num))
  print(paste('Selected item:',item_num))
  
  train <- train_full %>%
    filter(store == store_num, item == item_num)
  
  test <- test_full %>%
    filter(store == store_num, item == item_num)
  
  return(list(
    'train' = train,
    'test' = test))
}
