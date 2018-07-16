https://www.tutorialspoint.com/r/r_mean_median_mode.htm

# calculating mean
mean(df$col, na.rm=TRUE)

# calculating mean, given filter on this colmun
mean(df$col[df$col > 1000])

# calculate mean, given filter on another column
mean(df$col[df$col2 == TRUE])

# ==== creating data ====

# normal distribution
rnorm(howmanynumbersyouwant, mean, std)