##
# session2_Reality.R
# Purpose: Demonstration code for session two of CSP-708 Spring 2016. Note, there is a corresponding
# file named "session2.R" which contains what I planned to cover in the class, this file represents
# changes in the script that occurred during class, and ends where we ended
#
# Contents: Median, mode, mean, variance, central limit theorem, sampling, normal distribution,
# density, histogram

library(gtools)
library(plotly)

# Randomly select (sample) an 'age'from a uniform distribution
age <- sample(x = 1:120, size=1, replace=TRUE)


evenAges <- age[even(age) == TRUE]

### Median ####

# Sample a collection of ages (DISCRETE distribution)
set.seed(1)
ages <- sample(1:120, size=200000, replace=TRUE)
myMedian <- median(ages)
print(myMedian)

# Produce a histogram of the sample
hist(ages)

summary(ages)

# Sample 200 values from a uniform CONTINUOUS distribution
set.seed(1)
x <- runif(n=200, min=-1, max=1)
summary(x)

# Calculate the mean
xbar <- mean(x)
print(xbar)

# Produce a histogram of the sample
hist(x)

# Produce a cool, interactive histogram
plot_ly(x=x, type = "histogram")

## Repeatedly sample from the normal distribution and save the mean of each sample

  # Declare variable to store our samples
  xbars <- numeric() # declare xbars variable

  # Loop through X times calculating the mean of the sample each time
  for (i in 1:1000){

    #xbars[i] <- mean(rnorm(n=20, mean=0, sd=1))
    mySample <- runif(n=2000, min=13, max=99)
    xbars[i] <- mean(mySample) # Calculate the mean of the sample
  }

# What is the mean of the means?
mean(xbars)

# What is the distribution of the means
hist(xbars)
d <- density(xbars, kernel="gaussian")
plot(d)

