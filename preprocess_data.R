library('xts')

################################################################################
### DATA PREPROCESSING #########################################################
################################################################################


# Read in the endogenous variables.
df_endog <- read.csv('input_output_data.csv')
df_endog$time <- as.Date(df_endog$time, format = '%Y-%m-%d')
names(df_endog)[names(df_endog) == 'time'] <- 'date'

# Read in the exogenous variables.
df_holiday <- read.csv('holiday.csv', stringsAsFactors = TRUE)
df_holiday$holiday <- as.logical(df_holiday$holiday)
df_holiday <- df_holiday[ , c('date', 'holiday')]

# Merge the endogenous and exogenous variables to one frame.
df <- merge(df_endog, df_holiday)

# Introduce weekday variable.
df$weekday <- as.factor(rep(1:7, length.out=1375))

# Remove unnecessary data points.
df <- df[29:nrow(df), ]
df <- df[df$weekday != 6 & df$weekday != 7, ]

# Remove the holidays.
nonop <- which(df$sum_fractions == 0)
df <- df[-nonop, ]

# Store the data in time series structure.
ts_truth <- xts(df[ , -1], order.by=df$date, frequency=5)

var_endog <- c('Beverage.carton', 'Foil', 'Hollow.bodies', 'MP.hard', 'MP.soft', 'NF.metal',
               'PPC', 'PE', 'PET.bottles', 'PET.bowls', 'PP', 'PS',
               'Sorting.residues')

ts_port <- xts(df[ , var_endog] / df[ , 'sum_fractions'], order.by=df$date, frequency=5)