# histogram
hist(df$col, main="chart name", xlab="xaxis name")

# add a straight line to scatter plot
abline(intercept, slope, col="red") 

# mean line (for histogram)
abline(v=mean(df$col), col="red", lwd=4)

# simple scatter
plot(x, y)

# scatterplot details
plot(x, y, pch=20)

# PCH (plotting chatacters)
http://www.sthda.com/sthda/RDoc/figure/graphs/r-plot-pch-symbols-points-in-r.png

# combine multiple plots into one graph
https://www.statmethods.net/advgraphs/layout.html
attach(df)
par(mfrow=(2,2)) # 2 rows, 2 columns
hist(df$col)
hist(df$col)

