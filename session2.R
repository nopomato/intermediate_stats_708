
### Median ####

# Randomly select (sample) an 'age'
age <- sample(x = 1:120, size=1)

# Sample a collection of ages
ages <- sample(1:120, 20, replace=TRUE)
median(ages)

hist(ages)

# Sample 200 values from a uniform distribution
x <- runif(n=200, min=-1, max=1)

# Calculate the mean
xbar <- mean(x)
hist(x)
library(plotly)
plot_ly(x=x, type = "histogram")

# Repeatedly sample from the normal distribution and save the mean of each sample
xbars <- numeric() # declare xbars variable

# Loop through X times calculating the mean of the sample each time
for (i in 1:1000){

#  xbars[i] <- mean(rnorm(n=20, mean=0, sd=1))
  xbars[i] <- mean(runif(n=20, min=-1, max=1))
}

# What is the mean of the means?
mean(xbars)

# What is the distribution of the means
hist(xbars)
d <- density(xbars)
plot(d)


library(ggplot2)

ggplot(data=data.frame(xbars=xbars), aes(x=xbars)) + 
  geom_histogram(stat="bin", binwidth=.1) +

  
  
# scale_x_continuous(breaks=seq(from=-1, to=1, by=.1))


ggplot(data=data.frame(xbars=xbars), aes(x=xbars)) + 
  geom_histogram(aes(y=..density..), binwidth=.1, colour="black", fill="white") +
  geom_density(alpha = .2, fill="#FF6666")

### Variance ####
x <- rnorm(n=200, mean=0, sd=1)

mean(x)

hist(x)

var(x)

### Standard Deviation ###
sd(x)

# Show normal curve with selected SDs
hist(x)
quantile(x=x, probs=c(0, .25, .5, .75, 1.0))
range(x)
median(x)

# Outliers
pnorm(q=0) # Probability of less than or equal to zero
pnorm(q=-1) # Probability of less than or equal to -1
1-pnorm(q=-1)
pnorm(q=c(-1, 1))
probs <- pnorm(q=c(-1, 1))
probs[2] - probs[1]

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
