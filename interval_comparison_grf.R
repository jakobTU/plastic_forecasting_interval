library('grf')
library('caret')
library('caretForecast')
library('Metrics')

################################################################################
### GET THE PREDICTION INTERVALS ###############################################
################################################################################

# Load the data after preprocessing
source('preprocess_data.R')

################################################################################
### FIT THE quantile_forest FUNCTION INTO THE CARET FRAMEWORK ##################
################################################################################

# To fit a new model into the caret framework
# https://topepo.github.io/caret/using-your-own-model-in-train.html
# is a helpful reference

# Define the parameters of the model
qfParams <- data.frame(parameter=c('num.trees', 'mtry', 'min.node.size'),
                            class=rep('numeric', 3),
                            label=c('num.trees', 'mtry', 'min.node.size'))

# Define the parameter grid of the model
qfGrid <- function(x, y, len=NULL, search='grid') {
  expand.grid(num.trees=500, mtry=c(1,4,7), min.node.size=c(1,5,10))
}

# Define how to fit the new model
qfFit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
  grf::quantile_forest(X=x,
                       Y=y,
                       quantiles=c(0.025, 0.1, 0.5, 0.9, 0.975),
                       num.trees=param$num.trees,
                       mtry=param$mtry,
                       min.node.size=param$min.node.size,
                       ...)
}

# Define how predictions are made with the new model
qfPred <- function(modelFit, newdata, preProc=NULL, submodels=NULL) {
  predict(modelFit, newdata)
}

# Put everything together
htree <- list(library='grf',
              type='Regression',
              parameters=qfParams,
              grid=qfGrid,
              fit=qfFit,
              predict=qfPred,
              prob=NULL)


################################################################################
### GET THE PREDICTION INTERVALS ###############################################
################################################################################

# Due to computational power the original training was performed splitted
# by the fractions. To mimic that, it is looped over the fractions. It is
# important to reset the seed to 123 for every split in order to get the same results.
l_qfs <- list()
for (frac in 1:13) {
  # Get the same training data as in [1]
  params_rf_arml <- expand.grid(mtry=c(1,4,7), splitrule='variance', min.node.size=c(1,5,10))
  rf_arml <- caretForecast::ARml(ts(ts_port[1:930, frac], frequency=5), max_lag=30, caret_method='ranger', tune_grid=params_rf_arml, cv_horizon=10, allow_parallel=TRUE)
  
  set.seed(123)
  l_qf <- lapply((658:929)-30, function(train_end) train(x=rf_arml$x[1:train_end, ],
                                                         y=as.numeric(rf_arml$y_modified[1:train_end]),
                                                         method=htree,
                                                         trControl=trainControl(method='timeslice', initialWindow=train_end-50, horizon=10, fixedWindow=FALSE,
                                                                                verbose=TRUE, allowParallel=TRUE, returnData=TRUE, returnResamp='final',
                                                                                savePredictions='final')))
  
  l_qfs[[frac]] <- l_qf
}

# To make the following calculations easier, focus on one fraction (to not have to
# loop over all fractions every time). To choose a different fraction, simply set
# the frac variable to a different value below.
frac <- 1

fcast_qf <- t(sapply(1:272, function(i) predict(l_quantTree[[frac]][[i]]$finalModel, newdata=rf_arml$x[i+628, , drop=F])$predictions))

# Visualize the situation
plot(rf_arml$y_modified[(831:930)-30], type='l', ylab='Proportion', main='Beverage Carton')
lines(1:100, fcast_rf[173:272, 3], col='red')
lines(1:100, fcast_rf[173:272, 1], col='green')
lines(1:100, fcast_rf[173:272, 5], col='green')
lines(1:100, fcast_rf[173:272, 2], col='blue')
lines(1:100, fcast_rf[173:272, 4], col='blue')

# Get coverage
is_in_interval <- function(x, lower, upper) {
  lower <= x & x <= upper
}

# Evaluate the interval coverage
eval_pred_interval <- function(low, high) {
  mean(is_in_interval(ts_port[659:930, frac], low, high))
}

# Calculate the RMSE of the point predictions
Metrics::rmse(fcast_rf[ , 3], rf_arml$y_modified[1:272+628])

# The interval coverages for the 80% and 95% prediction intervals
cover80 <- eval_pred_interval(fcast_rf[ , 2], fcast_rf[ , 4])
cover95 <- eval_pred_interval(fcast_rf[ , 1], fcast_rf[ , 5])

# The interval lengths for the 80% and 95% prediction intervals
len80 <- mean(fcast_rf[ , 4] - fcast_rf[ , 2])
len95 <- mean(fcast_rf[ , 5] - fcast_rf[ , 1])

# [1] Becker, J., Kempkes, P., Mielke, K., Fendel, A., Bommert, A., Pauly, M.: Forecasting Plastic Waste Fractions: A Recycling Perspective. In: Proceedings of the Conference on Production Systems and Logistics: CPSL 2025, pp. 494-508. publishing-Ing., Offenburg (2025)