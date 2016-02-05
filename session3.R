# Blah blah changing things

##
# session3.R
# Purpose: Demonstration code for session two of CSP-708 Spring 2016
#
# Contents:

  # So it is reproducable   
  set.seed(seed=1)

#### Cumulative Frequency Table

  # Get a sample of "PHQ9" Scores
  dummy9 <- runif(n=100, min=0, max=27)
  
  # What is the distribution of scores
  hist(dummy9)
  
  # What about a table?
  table(dummy9) # Hmmm, not so helpful
  
  # Let's round to integer values
  dummy9 <- round(dummy9, 0)
 
  # Now what's the table look like?
  table(dummy9) # That's better
  
  # How about frequencies?
  prop.table(table(dummy9))
  
  # Cumulative counts?
  cumsum(table(dummy9))
  
  # Cumulative proportions
  cumsum(prop.table(table(dummy9)))  

  # Put it all together
  dummy9_tbl = table(dummy9)
  
  
  # Save it into a dataframe
  phq9FreqTbl_df <- as.data.frame(cbind( Freq=dummy9_tbl, relative=prop.table(dummy9_tbl), Cumul=cumsum(dummy9_tbl), CumulPer = cumsum(prop.table(dummy9_tbl))))
  
  phq9FreqTbl_df
  
  # How do we get the phq9 scores into the data frame as the first column?
  
  # What is the probability that a score is less <= 20?
  
  # What is the probability that a score is between 10 & 15
  
  # How can I get "CumulPer" in percentage form rather than raw proportion
### More Distribution functions
  
# Let's get a sample of means 
  
  # So it is reproducable   
  set.seed(seed=1)
  
  # Declare variable to store our samples
  xbars <- numeric() # declare xbars variable

  # Loop through X times calculating the mean of the sample each time
  for (i in 1:100){

    #xbars[i] <- mean(rnorm(n=20, mean=0, sd=1))
    xbars[i] <- mean(runif(n=200, min=-1, max=1)) # Calculate the mean of the sample
  }

# What is the distribution of the means
hist(xbars)
d <- density(xbars, kernel="gaussian")
plot(d)

# Create an emprical cumulative distribution function
myECDF <- ecdf(xbars)

# Get quantiles and range for xbars
summary(xbars)

# What is probability of the median?
myECDF(median(xbars))

# What is probability of the mean
myECDF(mean(xbars))


# What is probability of the first quartile?
myECDF(???)

# What is probability that a value lies between the 1st quartile and the median?
???

## Goodness of Fit

# Let's assume an urn with 30 red balls, 30 green balls and 30 yellow balls
# What's the probability of drawing a red ball? green? yellow?
XX

# Let's sample from our urn
draws <- XX

draws

# What is the likelihood that our sample came from a distribution of equal probabilities?
chisq.test(x = table(x), p  = rep(1/length(table(x)), length(table(x))))

## Test of Independence

# What is the frequency of men in this room
# What is frequeny of men in the general population
# What is the frequency of women in the general population
# What is the frquency of women in this room
# What is the expected frequency of men/women

chisq.test(x=matrix(c(50,50,30, 70), nrow=2))
chisq.test(x=matrix(c(50,50,49, 51), nrow=2))
chisq.test(x=matrix(c(5,6,2, 9), nrow=2))


# Effect size?
# Cramer's V
x=matrix(c(50,50,30, 70), nrow=2)
chiSquare <- chisq.test(x)$statistic
k <- min(dim(x)) # Could just set  to two for a 2x2 contingency table able
cramersV <- sqrt(chiSquare/(sum(x)*(k-1)))

# What to do with small samples? Use fisher.test
fisher.test(x)

## Assumptions
## Groups must be independent. When would this be violated?
## If we were doing a pre-post survey (like before and after watching an advertisement)


# What if your observations aren't independent?

# Let's create some data
sampleSize <- 40
mySample <- data.frame(subjID = seq(1:sampleSize), before=factor(sample(c(0,1), size=sampleSize, prob=c(.3, .7), replace=TRUE), labels=c("NO", "YES")),
           after=factor(sample(c(0,1), size=sampleSize, prob=c(.5, .5), replace=TRUE), labels=c("NO", "YES")))

summary(mySample) # Does a table on before and after for us

beforeAfter_tb <- table(before=mySample$before, after=mySample$after)
beforeAfter_tb

# MacNemar's test
mcnemar.test(beforeAfter_tb)


# Let's visualize some data
creativityDat <- read.csv("creativity_survey_dat.csv")

# Plot Gender
gender_tb <- table(creativityDat$gender)
barplot(gender_tb, main="Gender Distribution", xlab="Gender")

# What is workflow for determining if males and females are appropriately represented?

# How to plot race?

# How about proportion/percentage of each race?
