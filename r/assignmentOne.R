# Question 1
courseNum <- c("593", "501", "504", "502")
courseName <- c("Exploratory Data Analysis", "Computation for Analytics", "Review of Probability and Statistics", "Linear Algebra")
courseProf <- c("Paul Intrevado", "Terrence Parr", "Jeff Hamrick", "Xuemei Chen") 
enrolled <- as.logical(c(1, 1, 1, 0))
anticipatedGrade <- c("A", "A", "B", NA)
anticipatedHours <- c(15, 10, 15, NA)

# make Matrix
names <- c("courseNum", "courseName", "courseProf", "enrolled", "anticipatedGrade", "anticipatedHours")
types <- c(typeof(courseNum), typeof(courseName), typeof(courseProf), typeof(enrolled), typeof(anticipatedGrade), typeof(anticipatedHours))
class <- c(class(courseNum), class(courseName), class(courseProf), class(enrolled), class(anticipatedGrade), class(anticipatedHours))

myMatrix <- matrix(c(names, types, class), nrow=6, ncol=3, byrow=FALSE)
colnames(myMatrix) <- c("items", "type", "class")
View(myMatrix)

# make DF
bootcampDataFrame <- data.frame(
  courseNum,
  courseName,
  courseProf,
  enrolled,
  anticipatedGrade,
  anticipatedHours
)

View(bootcampDataFrame)

types_2 <-  c()
class_2 <-  c()

for(i in 1:length(names)){
  types_2[i] <- typeof(bootcampDataFrame[,i])
  class_2[i] <- class(bootcampDataFrame[,i])
}

myMatrix_2 <- matrix(c(names, types_2, class_2), nrow=6, ncol=3, byrow=FALSE)
colnames(myMatrix_2) <- c("items", "type", "class")

# no, character in list turns into type integer and type factor in list

bootcampList <- list(
  courseNum,
  courseName,
  courseProf,
  enrolled,
  anticipatedGrade,
  anticipatedHours
)

names(bootcampList) <- names
types_3 <- c()
class_3 <- c()

for(i in 1:length(names)){
  types_3[i] <- typeof(bootcampList[[i]])
  class_3[i] <- class(bootcampList[[i]])
}

# they do retain their types from the initial matrix, unlike when we did the dataframe

# number of hours anticpate spending on coursework, per week
sum(bootcampDataFrame$anticipatedHours, na.rm=TRUE) #40

# number of hours anticpate spending on coursework, over all bootcamp
sum(bootcampDataFrame$anticipatedHours, na.rm=TRUE) * 5 #200

# data frame with onlt the third row and first two columns of `bootcampDataFrame`
bootcampDataFrame[3,1:2]

####

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
# calculate b1 (slope)
get_b1 <- function(x_list, y_list){
  x_hat <- mean(x_list)
  y_hat <- mean(y_list)
  numerator = 0.0
  denominator = 0.0
  for (i in 1:length(x_list)){
    numerator = numerator + ( (x_list[i] - x_hat) * (y_list[i] - y_hat) ) # 8064.517
    denominator = denominator + ( (x_list[i]-x_hat)**2 ) # 333564449
  }
  b1 = numerator/denominator # 2.417679e-05
  return(b1)
}

print(get_b1(x_1, y_1)) # works

# calculate b0 (intercept)
get_b0 <- function(x_list, y_list, b1_given){
  b0 = (sum(y_list) - b1_given*sum(x_list))/length(x_list) # 1.995036
  return(b0)
}

print(get_b0(x_1, y_1)) # works

# Proof through R's package
rFunction <- lm(y_1 ~ x_1, data.frame(x_1, y_1)) # intercept = 1.995e+00, 2.418e-05
summary(rFunction)
plot(resid(rFunction) ~ fitted(rFunction))
plot(x_1, y_1)

# Q4.2

# generate predicted Y values from x values, using regression formula (i.e. coefficients found above)
get_predicted_y_values <- function(x_list, b0, b1){ # might break due to variable overlap b0 b1
  predictedY = c()
  for (i in 1:length(x_list)){
    predictedY[i] = b0 + b1*x_list[i]
  }
  return(predictedY)
}

get_SSE <- function(x_list, y_list, predictedY){
  SSE = 0.0
  for (i in 1:length(x_list)){
    SSE = SSE + (y_list[i]- predictedY[i])**2 # y_1 = y actual 
  }
  SSE # 396,806.1
}

print(get_SSE(x_1, y_1, predictedY)) # works

get_SSTo <- function(x_list, y_list){
  SSTo = 0.0
  y_hat = mean(y_list)
  for (i in 1:length(x_list)){
    SSTo = SSTo + (y_list[i]-y_hat)**2
  } 
  return(SSTo) # 396,806.3
}
# works

get_SSR <- function(SSTo, SSE){
  return(SSTo-SSE) # 0.19497
}

print(get_SSR(get_SSTo(x_1, y_1), get_SSE(x_1, y_1, predictedY))) # works

# calucluate R-squared
get_rSquared <- function(SSR, SSTo){
  rSquared = SSR/SSTo # 4.913584e-07
  return(rSquared)
}

print(get_rSquared(get_SSR(get_SSTo(x_1, y_1), get_SSE(x_1, y_1, predictedY)), get_SSTo(x_1, y_1))) # works

# Q4.3
plot(x_1, y_1, main="A Beautiful Scatterplot", xlab="x values", ylab="y values", pch=20, cex=0.2)
abline(b0, b1, col="red")

# Q4.4
# residuals
get_residuals <- function(x_list, y_list, predictedY){
  residuals = c()
  for(i in 1:length(x_list)){
    residuals[i] = y_list[i] - predictedY[i]
  }
  return(residuals)
}


# Q4.5
# plot residuals
plot(x_1, residuals, main="Residuals", xlab="x values", ylab="residuals (ei)", pch=20, cex=0.2)
abline(0,0, col="red")

# Q4.5
#### model 1
set.seed(100)
x_1 <- runif(100000, -100, 100)
y_1 <- rexp(100000,  rate = 0.5)

b1_1 <- get_b1(x_1, y_1) # -3.205058e-05
b0_1 <- get_b0(x_1, y_1, b1_1) # 1.999032
predictedY_1 <- get_predicted_y_values(x_1, b0_1, b1_1)
SSE_1 <- get_SSE(x_1, y_1, predictedY_1) # 398,628.3
SSTo_1 <- get_SSTo(x_1, y_1) # 398,629.4
SSR_1 <- get_SSR(SSTo_1, SSE_1) # 1.0323
rSquared_1 <- get_rSquared(SSR_1, SSTo_1) # 2.589701e-06
residuals_1 <- get_residuals(x_1, y_1, predictedY_1) # list

plot(x_1, y_1, main="A Beautiful Scatterplot", xlab="x values", ylab="y values", pch=20, cex=0.2)
abline(b0_1, b1_1, col="red")

plot(x_1, residuals_1, main="Residuals", xlab="x values", ylab="residuals (ei)", pch=20, cex=0.2)
abline(0,0, col="red")
#######

set.seed(999)
x_2 <- rnorm(100000, -100, 100)
y_2 <- rexp(100000, rate = 0.5)

b1_2 <- get_b1(x_2, y_2) # -3.205058e-05
b0_2 <- get_b0(x_2, y_2, b1_2) # 1.999032
predictedY_2 <- get_predicted_y_values(x_2, b0_2, b1_2)
SSE_2 <- get_SSE(x_2, y_2, predictedY_2) # 398,628.3
SSTo_2 <- get_SSTo(x_2, y_2) # 398,629.4
SSR_2 <- get_SSR(SSTo_2, SSE_2) # 1.0323
rSquared_2 <- get_rSquared(SSR_2, SSTo_2) # 2.589701e-06
residuals_2 <- get_residuals(x_2, y_2, predictedY_2) # list

plot(x_2, y_2, main="The Second Beautiful Scatterplot", xlab="x values", ylab="y values", pch=20, cex=0.2)
abline(b0_2, b1_2, col="red")

plot(x_2, residuals_2, main="Residuals", xlab="x values", ylab="residuals (ei)", pch=20, cex=0.2)
abline(0,0, col="red")

#### 
set.seed(543)
x_3 <- rnorm(100000, -100, 100)
y_3 <- rnorm(100000, -100, 100)

b1_3 <- get_b1(x_3, y_3) # -3.205058e-05
b0_3 <- get_b0(x_3, y_3, b1_3) # 1.999032
predictedY_3 <- get_predicted_y_values(x_3, b0_3, b1_3)
SSE_3 <- get_SSE(x_3, y_3, predictedY_3) # 398,628.3
SSTo_3 <- get_SSTo(x_3, y_3) # 398,629.4
SSR_3 <- get_SSR(SSTo_3, SSE_3) # 1.0323
rSquared_3 <- get_rSquared(SSR_3, SSTo_3) # 2.589701e-06
residuals_3 <- get_residuals(x_3, y_3, predictedY_3) # list

plot(x_3, y_3, main="The Third Beautiful Scatterplot", xlab="x values", ylab="y values", pch=20, cex=0.2)
abline(b0_3, b1_3, col="red")

plot(x_3, residuals_3, main="Residuals", xlab="x values", ylab="residuals (ei)", pch=20, cex=0.2)
abline(0,0, col="red")


summaryTable <- data.frame(
  model <- c("Model 1", "Model 2", "Model 3"),
  b0 <- c(b0_1, b0_2, b0_3),
  b1 <- c(b1_1, b1_2, b1_3),
  SSE <- c(SSE_1, SSE_2, SSE_3),
  SSR <- c(SSR_1, SSR_2, SSR_3),
  SSTo <- c(SSTo_1, SSTo_2, SSTo_3),
  stringsAsFactors = FALSE
)

colnames(summaryTable) <- c("model", "b0", "b1", "SSE", "SSR", "SSTo", "RSquared")

View(summaryTable)


# Proof through R's package
rFunction_1 <- lm(y_2 ~ x_2, data.frame(x_2, y_2)) # intercept = 1.999e+00, slope = -3.205e-05
summary(rFunction_1)
plot(resid(rFunction) ~ fitted(rFunction))
plot(x_2, y_2)


# just a storage of functions
get_b1 <- function(x_list, y_list)
  get_b0 <- function(x_list, y_list)
    get_predicted_y_values <- function(x_list, b0, b1) # might break due to variable overlap b0 b1
      get_SSE <- function(x_list, y_list, predictedY)
        get_SSTo <- function(x_list, y_list)
          get_SSR <- function(SSTo, SSE)
            get_rSquared <- function(SSR, SSTo)
              get_residuals <- function(x_list, y_list, predictedY)
                
  