setup_recipe <- function(df){
  prelim_ft_eng <- recipe(sales ~ ., data=df) %>%
    step_date(date, features='dow') %>%
    step_date(date, features='month') %>%
    step_date(date, features='year') %>%
    step_date(date, features='doy') %>%
    step_date(date, features='decimal') %>%
    step_range(date_doy, min=0, max=pi) %>%
    step_mutate(sinDOY=sin(date_doy), cosDOY=cos(date_doy))# %>%
    #step_mutate(sin_scaled = 0.5 * sin(date_doy) * 30)
  
    #step_lag(sales,lag=365) %>%
    # step_mutate(sin_scaled = 0.5 * sin(date_doy) * mean(
    #   aggregate(sales ~ date_year, max)[,2] -
    #     aggregate(sales ~ date_year, min, data=df)[,2])
    #   )
    #step_normalize(all_numeric_predictors()) # Normalize features
  
  # Set up preprocessing
  prepped_recipe <- prep(prelim_ft_eng)
  
  return(prepped_recipe)
}

## Most people getting 10s and 11s