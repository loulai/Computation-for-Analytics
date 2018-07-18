titanicData <- read.csv("./titanic.csv", na='\\N')
titanicData

library(dplyr)
library(ggplot2)

# 2.2, 2.3
nrow(titanicData)
ncol(titanicData)

# 2.4
maxNA  = 0

for (i in c(1:ncol(titanicData))) {
  if(sum(is.na(titanicData[i])) > maxNA){
    maxNA = sum(is.na(titanicData[i]))
    print(colnames(titanicData[i]))
    print(sum(is.na(titanicData[i])))
  }
}

# 2.5
for (i in c(1:ncol(titanicData))) {
  print(colnames(titanicData[i]))
  print(class(titanicData[i][1,]))
}

# survived should be logical (binary), becuase there are only two options
# Pclass should be
# come back!
# survived is an integer originally

# 2.5
titanicData$survived <- as.logical(titanicData$Survived)
class(titanicData$survived)

print(sum(is.na(titanicData$Age))) # note there are 117 NAs for age, but none for Survived
print(sum(is.na(titanicData$Survived)))

avgAgeSurvivor <- mean(titanicData$Age[titanicData$Survived==TRUE], na.rm=TRUE) # mean age of survivors = 28.34
avgAgeNonsurvivor <- mean(titanicData$Age[titanicData$Survived==FALSE], na.rm=TRUE) # mean age of survivors = 30.63

View(titanicData$Age[titanicData$Survived==TRUE])

sum(titanicData$survived==TRUE) # 342
sum(titanicData$survived==FALSE) # 549
sum(is.na(titanicData$Survived)) # 0

# plotting histogram
hist(titanicData$Age[titanicData$survived == TRUE])

attach(titanicData)
par(mfrow=c(2,1)) # 2 rows 1 col
hist(titanicData$Age[titanicData$survived == TRUE], main="Age of Survivors", xlab="age")
abline(v=avgAgeSurvivor, lwd=4, col="red")

hist(titanicData$Age[titanicData$survived == FALSE], main ="Age of Non-survivors", xlab="age")
abline(v=avgAgeNonsurvivor, lwd=4, col="red")

# first ten cabin
View(titanicData$Cabin[1:10])

# could just load the data again, subbing spare "" with NA
# data <- read.csv("data.csv", na.strings="")
# but we won't do that becuase we don't want to reload file
titanicData$Cabin <- as.character(titanicData$Cabin)
class(titanicData$Cabin)
titanicData[titanicData==""] <-  # adds 687 NAs to Cabin

# count how many NAs
for (i in c(1:ncol(titanicData))) {
    print(colnames(titanicData[i]))
    print(sum(is.na(titanicData[i])))
}

# Question 3
distA <- rnorm(100, -1, 1)
distB <- rnorm(1000, -1, 1)
distC <- rnorm(10000, -1, 1)
distD <- rnorm(100000, -1, 1)
distE <- rnorm(1000000, -1, 1)

distribution <- list(distA, distB, distC, distD, distE)

sampleSize <- c(100, 1000, 10000, 100000, 1000000)

theoreticalMean <-c(-1, -1, -1)
sampleMean <- sapply(distribution, function(x) mean(x))
deltaMean <- abs(theoreticalMean - sampleMean)

theoreticalVariance <- c(1, 1, 1)
sampleVariance <- sapply(distribution, function(x) var(x))
deltaVariance <- abs(theoreticalVariance - sampleVariance)

unifDataFrame <- data.frame(sampleSize, theoreticalMean, sampleMean, deltaMean, theoreticalVariance, sampleVariance, deltaVariance)
attach(unifDataFrame)
plot(sampleSize, deltaMean)
lines(lowess(deltaMean ~ sampleSize), col="black")

plot(sampleSize, deltaVariance)
lines(lowess(deltaVariance ~ sampleSize), col="black")

# 3.3
runIfVector <- rnorm(10000000, 0, 1)
samples <- sample(runIfVector, 100000)
# need to generate variances

# Question 4
set.seed(100)
x_1 <- runif(100000, -100, 100)
y_1 <- rexp(100000,  rate = 0.5)

x_hat <- mean(x_1)
y_hat <- mean(y_1)

rFunction <- lm(y_1 ~ x_1, data.frame(x_1, y_1))
summary(rFunction)
# SSE = sum of squares due to error
# SSR = sum of squares due to regression
# SSTO = Total sum of squares