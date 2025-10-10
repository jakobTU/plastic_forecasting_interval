library('caret')
library('caretForecast')
library('RFpredInterval')
library('Metrics')

# Load the data after preprocessing
source('preprocess_data.R')

################################################################################
### FIT THE pibf FUNCTION INTO THE CARET FRAMEWORK #############################
################################################################################

# Define the coverage of the prediction intervals; here 95%.
# For 80% prediction intervals type 0.2.
one_minus_cover_perc <- 0.05

# To fit a new model into the caret framework
# https://topepo.github.io/caret/using-your-own-model-in-train.html
# is a helpful reference

# Define the parameters of the model
boostTreeParams <- data.frame(parameter=c('num.trees', 'mtry', 'min.node.size'),
                            class=rep('numeric', 3),
                            label=c('num.trees', 'mtry', 'min.node.size'))

# Define the parameter grid of the model
boostTreeGrid <- function(x, y, len=NULL, search='grid') {
  expand.grid(num.trees=500, mtry=c(1,4,7), min.node.size=c(1,5,10))
}

# Define how to fit the new model
boostTreeFit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
  train_dat <- data.frame(cbind(x, y=y))
  n <- nrow(train_dat)
  test_dat <- data.frame(cbind(rf_arml$x[n+1, , drop=F], y=rf_arml$y_modified[n+1]))
  
  RFpredInterval::pibf(y ~ ., traindata=dat[seq(1,n-1), ], testdata=dat[n, , drop=F], alpha=one_minus_cover_perc,
                       params_ranger=list(num.trees=param$num.trees, mtry=param$mtry, min.node.size=param$min.node.size))
}

# Define how predictions are made with the new model
boostTreePred <- function(modelFit, newdata, preProc=NULL, submodels=NULL) {
  modelFit$test_pred
}

# Put everything together
htree <- list(library='RFpredInterval',
              type='Regression',
              parameters=boostTreeParams,
              grid=boostTreeGrid,
              fit=boostTreeFit,
              predict=boostTreePred,
              prob=NULL)


################################################################################
### GET THE PREDICTION INTERVALS ###############################################
################################################################################

# Due to computational power the original training was performed splitted
# by the fractions and four equally long ranges of the training indices.
# To mimic that, it is looped over the fractions and the four ranges of the
# training indices. It is important to reset the seed to 123 for every split
# in order to get the same results.
l_boostTrees <- list()
inds_train <- list((658:725)-30, (726:793)-30, (794:861)-30, (862:929)-30) # -30 since the data is shifted by 30 days because of taking the last 30 days into consideration
for (frac in 1:13) {
  # Get the same training data as in [1]
  params_rf_arml <- expand.grid(mtry=c(1,4,7), splitrule='variance', min.node.size=c(1,5,10))
  l_mod_rf_arml <- caretForecast::ARml(ts(ts_port[1:930, frac], frequency=5), max_lag=30, caret_method='ranger', tune_grid=params_rf_arml, cv_horizon=10, allow_parallel=TRUE)
  
  boostTrees <- c()
  for (i in 1:4) {
    set.seed(123)
    l_boostTree <- lapply(inds_train[i], function(train_end) train(x=l_mod_rf_arml$x[1:train_end, ],
                                                                   y=as.numeric(l_mod_rf_arml$y_modified[1:train_end]),
                                                                   method=htree,
                                                                   trControl=trainControl(method='timeslice', initialWindow=train_end-50, horizon=10, fixedWindow=FALSE,
                                                                                          verbose=TRUE, allowParallel=TRUE, returnData=TRUE, returnResamp='final',
                                                                                          savePredictions='final')))
    
    boostTrees[i] <- l_boostTree
  }
  boostTrees <- unlist(boostTrees, recursive=FALSE)
  l_boostTrees[[frac]] <- boostTrees
}

# From the list of trained models across all folds and fractions get the respective
# true values, lower prediction interval bounds, upper prediction interval bounds
# and point predictions.
mat_truth <- sapply(1:13, function(frac) sapply(1:272, function(i) l_boostTrees[[frac]][[i]]$finalModel$test_response))
mat_lower <- sapply(1:13, function(frac) sapply(1:272, function(i) l_boostTrees[[frac]][[i]]$finalModel$pred_interval$lower))
mat_upper <- sapply(1:13, function(frac) sapply(1:272, function(i) l_boostTrees[[frac]][[i]]$finalModel$pred_interval$upper))
mat_pred <- sapply(1:13, function(frac) sapply(1:272, function(i) l_boostTrees[[frac]][[i]]$finalModel$test_pred))

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