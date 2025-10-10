library('forecast')

# Load the data after preprocessing
source('preprocess_data.R')

################################################################################
### GET THE ETS PREDICTION INTERVALS ###########################################
################################################################################

# Whether or not to use bootstrap intervals (set to TRUE to get the bootstrap variant)
bootstrap <- FALSE

# Train the ETS models
l_ets <- lapply(658:929, function(train_end) lapply(1:13, function(frac) ets(ts(ts_port[1:train_end, frac], frequency=5))))

# Forecast the respective test set with the trained models
l_model_to_fcast <- function(l_model, h=1, xreg=NULL, bt=FALSE) {
  lapply(1:272, function(i) lapply(1:13, function(frac) forecast(l_model[[i]][[frac]], h=h, xreg=xreg[seq(i+658,i+667), ], bootstrap=bt)))
}

# One-step-ahead forecasts with ETS using the prediction intervals based on
# the normal distribution
fcast_ets <- l_model_to_fcast(l_ets, h=1)

# Check the assumption of normally distributed residuals (at least for the last
# forecast fold)
for (i in 1:13) {
  res <- l_ets[[272]][[i]]$residuals
  checkresiduals(res)
}


################################################################################
### EVALUATE THE ETS PREDICTION INTERVALS ######################################
################################################################################

# Get the lower bounds of the 80% prediction intervals
get_low80 <- function(l_fcast) {
  sapply(1:272, function(i) sapply(1:13, function(frac) l_fcast[[i]][[frac]]$low[ , 1]))
}

l_ets_low80 <- get_low80(fcast_ets)
row.names(l_ets_low80) <- var_endog

# Get the lower bounds of the 95% prediction intervals
get_low95 <- function(l_fcast) {
  sapply(1:272, function(i) sapply(1:13, function(frac) l_fcast[[i]][[frac]]$low[ , 2]))
}

l_ets_low95 <- get_low95(fcast_ets)
row.names(l_ets_low95) <- var_endog

# Get the upper bounds of the 80% prediction intervals
get_up80 <- function(l_fcast) {
  sapply(1:272, function(i) sapply(1:13, function(frac) l_fcast[[i]][[frac]]$up[ , 1]))
}

l_ets_up80 <- get_up80(fcast_ets)
row.names(l_ets_up80) <- var_endog

# Get the upper bounds of the 95% prediction intervals
get_up95 <- function(l_fcast) {
  sapply(1:272, function(i) sapply(1:13, function(frac) l_fcast[[i]][[frac]]$up[ , 2]))
}

l_ets_up95 <- get_up95(fcast_ets)
row.names(l_ets_up95) <- var_endog

# Get the point predictions
get_mean <- function(l_fcast) {
  sapply(1:272, function(i) sapply(1:13, function(frac) l_fcast[[i]][[frac]]$mean))
}

l_ets_mean <- get_mean(fcast_ets)

# Get the RMSE values for all fractions
for (i in 1:13) {
  print(Metrics::rmse(l_ets_mean[i, ], ts_port[659:930, i]))
}

# Get coverage
is_in_interval <- function(x, lower, upper) {
  lower <= x & x <= upper
}

# Evaluate the interval coverage
eval_pred_interval <- function(low, high) {
  colMeans(is_in_interval(ts_port[659:930, ], t(low), t(high)))
}

# Coverage ETS 80% prediction interval one-step-ahead forecasts
cover_80_ets <- eval_pred_interval(l_ets_low80, l_ets_up80)

# Coverage ETS 95% prediction interval one-step-ahead forecasts
cover_95_ets <- eval_pred_interval(l_ets_low95, l_ets_up95)

# Average range of the prediction interval
len_80_ets <- rowMeans(l_ets_up80 - l_ets_low80)
len_95_ets <- rowMeans(l_ets_up95 - l_ets_low95)