#################################################################################################
#### R CHALLENGE 5  ###################################################
library(tidyverse)
library(emmeans)
library(car)
library(agridat)
library(glmmTMB)
library(easystats)
library(DHARMa)


## Load in and read about the beall.webworms dataset. The variables of interest are the y-count of webworms, 
## spray- spray treatment, and lead-lead treatment. Don't worry about the block or other variables for now.

data("beall.webworms")
d1 <- beall.webworms
?beall.webworms  ## info about the beall.webworms dataset
head(d1) ## view data set

## examine plot of data
ggplot(d1, aes(x=spray, y=y, fill=lead)) + geom_violin(scale="width", adjust=2) + 
  geom_point(position = position_jitterdodge(jitter.width=.5, jitter.height=.1, dodge.width = 1), alpha=.1) 


## Q1. Do webworm counts follow a normal distribution? Does it seem close enough?

## Q2. Construct a linear model with y as the response and Spray*Lead as the predictors.
##     How are the residuals? (use check_model() and/or simulateResisuals() from DHARMa)
##     Optional: also look at raw/traditional residuals (hist, qqplot, plot residuals vs fitted, etc.)
##     How do the emmeans differ between the different spray and lead treatments?



## Q3. Try a log-transformation of y 
##     hint: first check a histogram of log(y+1) to see if it looks more normal then run model.
##     How are the residuals? How do the emmeans differ between the linear and log-linear model?



## Q4. Try a generalized linear model using a Poisson distribution. How are the residuals? 
##      How do the emmeans differ between the linear and log-linear models?
##      How do the SE differ between the linear and log-linear models?
##      Check overdispersion using 'check_overdispersion' from performance package



## Q5. Fit a model with a negative binomial distribution using glmmTMB() with family='nbinom2'
##     How are the residuals?
##     How do the emmeans and SE differ between the four models? Discuss briefly which is most reasonable and why.


