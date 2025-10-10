library('caret')
library('caretForecast')
library('rfinterval')
library('Metrics')

# Load the data after preprocessing
source('preprocess_data.R')

################################################################################
### FIT THE rfinterval FUNCTION INTO THE CARET FRAMEWORK #######################
################################################################################

# Define the coverage of the prediction intervals; here 95%.
# For 80% prediction intervals type 0.2.
one_minus_cover_perc <- 0.05

# To fit a new model into the caret framework
# https://topepo.github.io/caret/using-your-own-model-in-train.html
# is a helpful reference

# Define the parameters of the model
rfParams <- data.frame(parameter=c('num.trees', 'mtry', 'min.node.size'),
                            class=rep('numeric', 3),
                            label=c('num.trees', 'mtry', 'min.node.size'))

# Define the parameter grid of the model
rfGrid <- function(x, y, len=NULL, search='grid') {
  expand.grid(num.trees=500, mtry=c(1,4,7), min.node.size=c(1,5,10))
}

# Define how to fit the new model
rfFit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
  train_dat <- data.frame(cbind(x, y=y))
  n <- nrow(train_dat)
  test_dat <- data.frame(cbind(rf_arml$x[n+1, , drop=F], y=rf_arml$y_modified[n+1]))
  
  rfinterval::rfinterval(y ~ ., train_data=train_dat, test_data=test_dat,
                         method='oob', alpha=one_minus_cover_perc,
                         params_ranger=list(num.trees=param$num.trees, mtry=param$mtry, min.node.size=param$min.node.size))
}

# Define how predictions are made with the new model
rfPred <- function(modelFit, newdata, preProc=NULL, submodels=NULL) {
  modelFit$testPred
}

# Put everything together
htree <- list(library='rfinterval',
              type='Regression',
              parameters=rfParams,
              grid=rfGrid,
              fit=rfFit,
              predict=rfPred,
              prob=NULL)


################################################################################
### GET THE PREDICTION INTERVALS ###############################################
################################################################################

# Due to computational power the original training was performed splitted
# by the fractions. To mimic that, it is looped over the fractions. It is
# important to reset the seed to 123 for every split in order to get the same results.
l_rfs <- list()
for (frac in 1:13) {
  # Get the same training data as in [1]
  params_rf_arml <- expand.grid(mtry=c(1,4,7), splitrule='variance', min.node.size=c(1,5,10))
  rf_arml <- caretForecast::ARml(ts(ts_port[1:930, frac], frequency=5), max_lag=30, caret_method='ranger', tune_grid=params_rf_arml, cv_horizon=10, allow_parallel=TRUE)
  
  set.seed(123)
  l_rf <- lapply((658:929)-30, function(train_end) train(x=rf_arml$x[1:train_end, ],
                                                              y=as.numeric(rf_arml$y_modified[1:train_end]),
                                                              method=htree,
                                                              trControl=trainControl(method='timeslice', initialWindow=train_end-20, horizon=10, fixedWindow=FALSE,
                                                                                     verbose=TRUE, allowParallel=TRUE, returnData=TRUE, returnResamp='final',
                                                                                     savePredictions='final')))
  
  l_rfs[[frac]] <- l_rf
}

# From the list of trained models across all folds and fractions get the respective
# true values, lower prediction interval bounds, upper prediction interval bounds
# and point predictions.
mat_truth <- sapply(1:13, function(frac) sapply(1:272, function(i) l_rfs[[frac]][[i]]$finalModel$test_data$y))
mat_lower <- sapply(1:13, function(frac) sapply(1:272, function(i) l_rfs[[frac]][[i]]$finalModel$oob_interval$lower))
mat_upper <- sapply(1:13, function(frac) sapply(1:272, function(i) l_rfs[[frac]][[i]]$finalModel$oob_interval$upper))
mat_pred <- sapply(1:13, function(frac) sapply(1:272, function(i) l_rfs[[frac]][[i]]$finalModel$testPred))

# To make the following calculations easier, focus on one fraction (to not have to
# loop over all fractions every time). To choose a different fraction, simply set
# the frac variable to a different value below.
frac <- 1
truth <- mat_truth[ , frac]
lower <- mat_lower[ , frac]
upper <- mat_upper[ , frac]
pred <- mat_pred[ , frac]

# Visualize the situation
plot(truth, type='l')
lines(lower, col='red')
lines(upper, col='red')
lines(pred, col='green')

# Get coverage
is_in_interval <- function(x, lower, upper) {
  lower <= x & x <= upper
}

# Evaluate the interval coverage
evapred_interval <- function(low, high) {
  mean(is_in_interval(ts_port[659:930, frac], low, high))
}

# Calculate the RMSE of the point predictions
Metrics::rmse(truth, pred)

# Get average interval coverage and length across the 272 folds
cover <- evapred_interval(lower, upper)
len <- mean(upper - lower)

# [1] Becker, J., Kempkes, P., Mielke, K., Fendel, A., Bommert, A., Pauly, M.: Forecasting Plastic Waste Fractions: A Recycling Perspective. In: Proceedings of the Conference on Production Systems and Logistics: CPSL 2025, pp. 494-508. publishing-Ing., Offenburg (2025)