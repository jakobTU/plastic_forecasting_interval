### THIS FILE CREATES AN EXAMPLE DATA SET. THE NUMBERS ARE PURELY MADE UP AND
### ARE ONLY FOR EXEMPLARY PURPOSES.

# Means and standard deviations for the 13 fractions.
means <- c(10, 20, 30, 20, 15, 10, 5, 5, 10, 25, 30, 10, 1)
sds <- means / 2

# Sample Gaussian distributed data, set the minimum to 0 and save it in a data.frame.
set.seed(123)
ts_fractions <- as.data.frame(pmax(mapply(function(i) rnorm(1375, mean=means[i], sd=sds[i]), i=1:13), 0))

# The variable names
var_names <- c('Beverage carton', 'Foil', 'Hollow bodies', 'MP hard', 'MP soft', 'NF metal',
               'PPC', 'PE', 'PET bottles', 'PET bowls', 'PP', 'PS',
               'Sorting residues')
names(ts_fractions) <- var_names

# Get the sum of the fractions.
ts_fractions$sum_fractions <- rowSums(ts_fractions)

# Randomly sample the 24 holidays and bridging days on which the plant was out of production.
ts_fractions[sample(1375, 24), ] <- 0

# Add dates to the data.
ts_fractions$time <- seq(as.Date('2020/03/02'), by='day', length.out=1375)

# Save the data.frame.
write.csv(ts_fractions, 'input_output_data.csv', row.names=FALSE)