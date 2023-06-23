

library(tidyverse)
library(tidyquant)
library(tidymodels)
library(modeltime)
library(timetk)
library(vip)

# ---------------- data ---------------------

# --- process new data ---
source("./data_processing.R")

# --- use saved data ---
#load(file = "./processed_data.RData")





# ------------ train test split --------------

splits <- 
  time_series_split(data, 
                    assess = "24 months", 
                    cumulative = TRUE)

splits %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(date, 
                           returns, 
                           .interactive=FALSE)



# --- split training set
val_set <- 
  validation_split(training(splits), 
                   prop = 0.80)





# --------------- random forest ----------------

cores <- parallel::detectCores()
cores


rf_recipe <- 
  recipe(forward_returns ~ ., data = training(splits)) %>%
  step_date(date) %>%
  step_rm(symbol) %>%
  step_rm(returns) %>% 
#  step_rm(forward_trend) %>% 
  step_log(forward_returns, offset=1, skip = TRUE)


#  ---- bakery
#df <- rf_recipe %>% prep() %>% bake(new_data = NULL)



# --- hyperparam tuning ----------------------------------

rf_tune_spec <- 
  rand_forest(mtry = tune(), min_n = tune(), trees = 1000) %>% 
  set_engine("ranger", num.threads = cores) %>% 
  set_mode("regression")



rf_tune_workflow <- 
  workflow() %>% 
  add_model(rf_tune_spec) %>% 
  add_recipe(rf_recipe)



set.seed(345)

rf_res <- 
  rf_tune_workflow %>% 
  tune_grid(val_set,
            grid = 25,
            control = control_grid(save_pred = TRUE),
            metrics = metric_set(mae))
 



rf_res %>% 
  show_best(metric = "mae")


rf_best <- 
  rf_res %>% 
  select_best(metric = "mae")

rf_best


rf_res %>% 
  collect_predictions()


# ------ final model ------

# --- final model spec
rf_final_spec <- 
  rand_forest(mtry = 52, 
              min_n = 7, 
              trees = 1000) %>% 
  set_engine("ranger", 
              num.threads = cores, 
              importance = "impurity") %>% 
  set_mode("regression")


# --- final workflow
rf_final_workflow <- 
  rf_tune_workflow %>% 
  update_model(rf_final_spec)

#  --- final model
rf_final_model <- 
  rf_final_workflow %>% 
  fit(data = training(splits))



# --- Modeltime Table ----
model_tbl <- modeltime_table(rf_final_model)

# --- Calibrate ----
calib_tbl <- 
  model_tbl %>%
  modeltime_calibrate(testing(splits))

# --- Accuracy ----
calib_tbl %>% modeltime_accuracy()


# --- Test Set Visualization ----
calib_tbl %>%
  modeltime_forecast(new_data    = testing(splits),
                     actual_data = data) %>%
  plot_modeltime_forecast(.interactive=FALSE)





 

# --- last fit ------
# --- fit on complete data set ----

rf_last_fit <- 
  rf_final_workflow %>% 
  last_fit(splits)


rf_last_fit %>% collect_metrics()



rf_last_fit %>% 
  extract_fit_parsnip() %>% 
  vip(num_features = 20)



rf_last_fit %>% 
  collect_predictions() %>% 
  roc_curve(future_trend, .pred_trend) %>% 
  autoplot()














