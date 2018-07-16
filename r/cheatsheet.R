# A bunch of collected shortcuts that make my life easier

# == R Keystroke Shortcuts (Linux) ==

# select by word: 
ctrl + arrow

# select entire line: 
alt + shift + arrow

# assignment operator (<-): 
alt + -
  
# restart R session: 
cntrl + shift + F10



# == When starting up ==  

# remove all objects from current environment
rm(list=ls()) 

# install packages
install.packages("myPackageName")

# library
library("dplyr")

# load package
library(PackageName)

# access function from package directly
dplyr::summarise(myVariables)

# load CSV
df <- read.csv("path.csv", na='\\N')

# load CSV to replace blanks with NA
df <- read.csv("path.csv", na.strings=c("", "anything else you want NA'ed")) 

# == numbers ==

# generate random number
sample(1:1000000, 1000000, replace=T)

# time 
system.time(function)


# == summing ===

# count the number of rows given criteria
sum(titanicData$Sex == 'male')



# ===== R Markdown ======

# ''': 
cntrol + alt + I


# ====== Terminal ======

# finding a file
$ find ./Extensions -name eventpage.js
