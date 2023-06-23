library(tidyverse)
library(tidyquant)
library(tidymodels)
library(modeltime)
library(timetk)


# ------------ param -------------

symbols <- c("AAPL", "VOO")

save_rdata <- FALSE

# --------------------------------



paste("\n\n\n",
      "---- data processing -----\n",
      "symbol:", symbols[1], "\n",
      "benchmark:", symbols[2], "\n",
      "--------------------------",
      "\n\n\n") %>% 
  cat()


# --- pull data
cat("pulling data from internet...")

data <- 
  symbols %>% 
  map(tq_get)

cat("done\n\n\n")


# tidy data
data <- 
  tibble(data) %>% 
  unnest(cols = c(data))


# ----- plot -----

# data %>% 
#     plot_time_series(
#         date, 
#         adjusted, 
#         .interactive=FALSE
#     )


# ------------ features -----------


data <- 
  data %>%
  mutate(year = year(date), 
         month = month(date))

# --- number of trading days
data <- 
  data %>% 
  group_by(symbol, month, year) %>% 
  mutate(num_trading_days = n())

# --- avg volume 
data <- 
  data %>% 
  group_by(symbol, month, year) %>% 
  mutate(avg_vol = sum(volume)/num_trading_days)








# --- mutate returns
returns <- 
  data %>% 
  group_by(symbol) %>% 
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "monthly", 
               col_rename = "returns")



# ----------- alpha beta --------------


## make benchmark return a column variable
benchmark_returns <- 
  returns %>% 
  filter(symbol == "VOO")

benchmark_returns <- 
  benchmark_returns %>% 
  pivot_wider(names_from  = symbol, 
              values_from = returns)

returns <- 
  returns %>% 
  full_join(benchmark_returns)



# --- CAPM regression func

regr_func <- 
  function(data) {
    lm(returns ~ VOO, 
       data = timetk::tk_tbl(data, silent = TRUE)) %>% 
    coef()
  }


returns <- 
  returns %>% 
  filter(symbol!="VOO")

data <- 
  data %>% 
  filter(symbol!="VOO")





##TODO param set width
## mutate rollapply
returns <- 
  returns %>%
  group_by(symbol) %>% 
  tq_mutate(mutate_fun = rollapply,
            width      = 12,
            FUN        = regr_func,
            by.column  = FALSE,
            col_rename = c("alpha", "beta"))




## --- MACD
data <- 
  data %>%  
  group_by(symbol) %>%
  tq_mutate(select     = adjusted, 
            mutate_fun = MACD,
            nFast      = 12,
            nSlow      = 26,
            nSig       = 9,
            maType     = EMA,
            percent    = TRUE)

data <- 
  data %>%
  group_by(symbol) %>%
  mutate(macd_sig = macd - signal)



# ## --- BBANDS
# data <- 
#   data %>%  
#   group_by(symbol) %>%
#   tq_mutate(select     = c(high, low, close), 
#             mutate_fun = BBands)


# --- lag stuff

general_recipe <- 
  recipe( ~ ., data = data) %>% 
  step_lag(#open, 
           #high, 
           #low, 
           #close, 
           #adjusted, 
           # volume, 
           macd_sig, 
           lag = 1:15) %>%
  step_rm(macd) %>%
  step_rm(signal) %>%
  step_rm(open) %>%
  step_rm(high) %>%
  step_rm(low) %>%
  step_rm(close) %>%
  step_rm(volume) %>%
  step_rm(adjusted) %>%
  step_naomit(all_predictors())


data <- 
  general_recipe %>% 
  prep() %>%
  bake(new_data = NULL)


# --- join with returns data  
data <- 
  data %>% 
  right_join(returns) %>% 
  na.omit()


# --- mutate forward return column
data <- 
  data %>% 
  mutate(forward_returns = lead(returns))

# --- delete last 2 rows 
data <- 
  data %>% 
  slice(1:(n()-2))

# --- mutate forward trend column
data <- 
  data %>% 
  mutate(forward_trend = ifelse(forward_returns<0, 
                                "neg", 
                                "pos"))




if (save_rdata == TRUE){
  save.image(file="processed_data.RData")
}







