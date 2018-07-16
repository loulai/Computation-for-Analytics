library("dplyr")

one <- 10352 + 987653
two <- 10352 / 987653
three <- 2^6 * 99
four <- 914545 %% 33

timeOne <- system.time(myVecOne <- rnorm(1000000, mean = 10, sd = 5))
timeTwo <- system.time(myVecTwo <- rnorm(10000000, mean = 10, sd = 5))
timeThree <- system.time(myVecThree <- rnorm(1000000000, mean = 10, sd = 5))
timeThree

# create graph in ggplot
myDF %>%
  ggplot(aes(x=time, y=sampleSize)) + 
  geom_line(linetype = "dotted") +
  geom_point(size=3) +
  xlab("\nTime (seconds") +
  ylab("Normal Random Variates Sample Size (millions)\n") +
  scale_y_continous(labels=comma)