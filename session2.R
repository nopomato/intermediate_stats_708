##
# session2.R
# Purpose: Demonstration code for session two of CSP-708 Spring 2016
#
# Contents: Median, mode, mean, variance, central limit theorem, sampling, normal distribution,
# density, histogram

# Randomly select (sample) an 'age'from a uniform distribution
age <- sample(x = 1:120, size=1)


### Median ####

# Sample a collection of ages (DISCRETE distribution)
ages <- sample(1:120, 20, replace=TRUE)
myMedian <- median(ages)
print(myMedian)

# Produce a histogram of the sample
hist(ages)

# Sample 200 values from a uniform CONTINUOUS distribution
x <- runif(n=200, min=-1, max=1)
print(x)

# Calculate the mean
xbar <- mean(x)
print(xbar)

# Produce a histogram of the sample
hist(x)

# Produce a cool, interactive histogram
library(plotly)
plot_ly(x=x, type = "histogram")

## Repeatedly sample from the normal distribution and save the mean of each sample

  # Declare variable to store our samples
  xbars <- numeric() # declare xbars variable

  # Loop through X times calculating the mean of the sample each time
  for (i in 1:100){

    #xbars[i] <- mean(rnorm(n=20, mean=0, sd=1))
    xbars[i] <- mean(runif(n=200, min=-1, max=1)) # Calculate the mean of the sample
  }

# What is the mean of the means?
mean(xbars)

# What is the distribution of the means
hist(xbars)
d <- density(xbars, kernel="gaussian")
plot(d)


# Use ggplot to make a histogram
library(ggplot2)

ggplot(data=data.frame(xbars=xbars), aes(x=xbars)) + 
  geom_histogram(stat="bin", binwidth=.1) +
# scale_x_continuous(breaks=seq(from=-1, to=1, by=.1))


ggplot(data=data.frame(xbars=xbars), aes(x=xbars)) + 
  geom_histogram(aes(y=..density..), binwidth=.1, colour="black", fill="white") +
  geom_density(alpha = .2, fill="#FF6666")

### Variance ####

x <- rnorm(n=200, mean=0, sd=1) # Sample from the normal distribution

# Calculate the mean
mean(x)

# Produce a histogram
hist(x)

# Calculate the variance of the sample
var(x)

### Standard Deviation ###

sd(x) # Calculate the standard deviation of the sample

# Show normal curve with selected SDs
hist(x)
quantile(x=x, probs=c(0, .25, .5, .75, 1.0)) # Get the quartile values
range(x) # Inspect the range
median(x) # Inspect the median

# Outliers
pnorm(q=0) # Probability of less than or equal to zero
pnorm(q=-1) # Probability of less than or equal to -1
1-pnorm(q=-1) # Probability of being greater than -1
pnorm(q=c(-1, 1)) # Probability of being less than -1 and less than 1

probs <- pnorm(q=c(-1, 1)) # Save the prob of less than -1 and 1
probs[2] - probs[1] # Probability of being between -1 and 1

# More on range and distribution - leading up to boxplots
x <- rnorm(n=20, mean=0, sd=1)
range(x)

summary(x)

pnorm(min(x))

1- pnorm(min(x))

mySummary <- summary(x)

names(mySummary)

mySummary["1st Qu."]

interQuartileRange <- (mySummary["3rd Qu."] - mySummary["1st Qu."])

minFence =  mySummary["1st Qu."] - (1.5 * interQuartileRange)

maxFence = mySummary["3rd Qu."] + (1.5 * interQuartileRange)

sort(x)

# Box plots
myBoxPlot <- boxplot(x)
plot_ly(x=x, type = "box")
