##################################################################################
#### R CHALLENGE 6: BINOMIAL GENERALIZED MODEL ###################################

library(emmeans)
library(car)
library(agridat)
library(DHARMa)
library(easystats)
library(glmmTMB)
library(tidyverse)

## load in jansen carrot data and read about the experiment. We will focus only on 'trt' as predictor variables.
## response variables will include 'n' as the number of carrots examined per plot and 'y' as the number infested with fly maggots.
data("jansen.carrot")
?jansen.carrot

d1 <- jansen.carrot
head(d1)

ggplot(d1, aes(x=trt, y=(y/n))) + 
  geom_boxplot(outlier.shape=NA)+ 
  geom_jitter(width=.15, height=0) +
  xlab("Pest treatment") + ylab("Prop infested w/ fly larvae\n(y/n)") +
  theme_bw(base_size = 16)

hist(d1$y/d1$n) ## histogram of distribution

#1. Calculate the average proportion of carrots infested in the two pest treatments. Use summarize() to caluculate the raw proportion. 


#2.construct a generalized linear model with #infested/#non-infested as the response variable. Specify the appropriate family.
### HINT: you're response variable should be 'cbind(y, n-y)' (don't include the '')


#3. Check the residuals and overdispersion. Does the model seem like a good fit to the data?

#3b. Correct for overdispersion, if necessary.
     #HINT: To correct for overdisperion with binomial, you have two options 
     #1) add an observation-level random effect (will return to this later)
     #2) use a family=betabinomial (USE THIS OPTION)


#4. Based on the model from Q2, is the effect of pest treatment significant?
    # compare both models to see how the p-values and SE change with the overdispersion correction.




#5. Examine the predicted means or probabilities. How do they compare to the raw proportions from Q1?
## How Do the p-values compare between the overdispersed and corrected model?
## How much does the pesticide treatment (T2) compare to the control (T1)?
