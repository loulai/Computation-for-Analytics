# histogram
hist(df$col, main="chart name", xlab="xaxis name")

# mean line (for histogram)
abline(v=mean(df$col), col="red", lwd=4)

# combine multiple plots into one graph
https://www.statmethods.net/advgraphs/layout.html
attach(df)
par(mfrow=(2,2)) # 2 rows, 2 columns
hist(df$col)
hist(df$col)

