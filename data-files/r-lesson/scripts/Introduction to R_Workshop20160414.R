# Introduction to R; Software Carpentry Workshop
# 14Apr16
detach()

#-------------------------
# LOADING DATA
# Set working directory
setwd("~/Desktop/software-carpentry-2016/data-files/r-lesson/data")
list.files()

# Print out one of the csv files in the "data" folder
read.csv("inflammation-01.csv", header = FALSE)

# Assigning variables
weight_kg <- 55
# To convert weight in kg to pounds:
weight_kg * 2.2
# Change an object's value
weight_kg <- 57.5

# Convert the weight to pounds and save to a variable
weight_lb <- weight_kg * 2.2
weight_lb

# If we then decide to change weight_kg again...
weight_kg <- 100.0
# ... and weight in pounds is still the old value
weight_lb

# Import all of our data into a variable, "dat"
dat <- read.csv(file = "inflammation-01.csv", header = FALSE)
dat
head(dat)

#-----------------------
# MANIPULATING DATA
# What type of data do we have
class(dat)
# What are the dimensions of that data frame
dim(dat)

# first value in dat (top left)
dat[1,1]
# middle value in dat
dat [30, 20]
# select a whole section
dat[1:4, 1:10]
dat[5:10, 1:10]
# we can use function "c" to combine and select non-contiguous values
dat[c(3,8,37,56), c(10,14,29)]
# to get all columns from the 5th row
dat[5,]
# or to get all the rows from the 16th column
dat[,16]

# let's look at the data from the first raptor, which is the first row and all columns
raptor_1 <- dat[1,]
# to find the maximum (max) or minimum (min) inflammation for raptor 1
max(raptor_1)
min(raptor_1)

# or, instead of sotring the row as a variable, we can just select the row we want to find the max in
max(dat[2,])
# mean inflammation on day 7
mean(dat[,7])
# median inflammation on day 7
median(dat[,7])
# standard deviation of inflammation on day 7
sd(dat[,7])

# get help on how to use the apply function
?apply
# obtain the average inflammation of each raptor; calculate the mean of all the rows (MARGIN = 1) of the data frame
avg_raptor_inflammation <- apply(dat, 1, mean)
avg_raptor_inflammation
# obtain the average inflammation of each day we will need to calculate the mean of all the columns (MARGIN = 2) of the data frame
avg_day_inflammation <- apply(dat, 2, mean)
avg_day_inflammation

# Challenge - slicing (subsetting) data
animal <- c("m", "o", "n", "k", "e", "y")
animal
animal[1:3]
animal[4:6]
animal[1:4]
# to get reverse order
animal[4:1]
# to omit 1
animal[-1]
animal [-4]
# to omit 1 through 4
animal[-1:-4]
# to select specific values
animal [c(5,2,3)]

# Challenge - Subsetting data 2
# to determine the max inflamation for raptor 5 across days three to seven
max(dat[5,3:7])

#---------------------
# PLOTTING
# to plot average inflammation over time
plot(avg_day_inflammation)
# look at the daily maximum inflammation
max_day_inflammation <- apply(dat,2,max)
max_day_inflammation
plot(max_day_inflammation)
# look at the daily minimum inflammation
min_day_inflammation <- apply(dat,2,min)
min_day_inflammation
plot(min_day_inflammation)

# Challenge - Plotting data
# Plot the standard deviation of the inflammation data for each day across all raptors
sd_day_inflammation <- apply(dat,2,sd)
sd_day_inflammation
plot(sd_day_inflammation)


#-------------------------
# CREATING FUNCTIONS
getwd()
list.files()
# gives structure 
str(read.csv)

temp <- 67
# define a function for converting from Fahrenheit to Kelvin
fahr_to_kelvin <- function(temp){
  kelvin <- ((temp - 32) * (5/9)) +273.15
  return(kelvin)
}
fahr_to_kelvin(67)
fahr_to_kelvin(temp)
# freezing point of water
fahr_to_kelvin(32)

# Composing Functions
# define a function for converting Kelvin into Celsius
kelvin_to_celsius <- function(temp) {
  celsius <- temp - 273.15
  return(celsius)
}
# absolute zero in Celsius
kelvin_to_celsius(0)


# Create a new function by composing the two functions we have already created
# To convert Fahrenheit to Celcius
fahr_to_celsius <- function(temp) {
  temp_k <- fahr_to_kelvin(temp)
  result <- kelvin_to_celsius(temp_k)
  return(result)
}
# freezing point of water in Celsius
fahr_to_celsius(32.0)

# Challenge - Create a function
best_practice <- c("Write", "programs", "for", "people", "not", "computers")
asterisk <- "***" 
# R interprets a variable with a single value as a vector with one element
# Create a fence function
fence <- function(original, wrapper){
  return(c(wrapper, original, wrapper))
}
fence(best_practice, asterisk)
