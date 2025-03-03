#### R CHALLENGE 7 ####
library(tidyverse)
library(car)
library(emmeans)
library(glmmTMB)

## load in the ChickWeight dataset. It contains weight (g) of small chickens grown on four different diets.
## Chickens were weighed every few days for 21 days. Don't worry about the 'Chick' column for this challenge
data("ChickWeight")
ChickWeight$Diet <- as.factor(ChickWeight$Diet)
?ChickWeight

## plot out data
ggplot(ChickWeight, aes(x=Time,y=weight))+geom_point()+facet_wrap(~Diet)+geom_smooth(method="lm")

## Q1: Conduct a regular one-way ANOVA to see if weight differs among the four diets (ie. ignore time). Which ones differ?



## Q2: Do the weights differ among the diets when including the time covariate? 


## Q2a: At what time point are the emmeans being calculated? (hint: the default is the average time point)


## Q3: Do the chicks on different diets have different growth rates? Which is fastest/slowest? (ie. compare slopes) 


## Bonus: Make a nice plot (either scatter plot or a dotplot with error bars for the emmeans)



######## PART 2  
library(lme4) # install if needed, can use instead of glmmTMB

data("ChickWeight")
ChickWeight$Diet <- as.factor(ChickWeight$Diet)

## Q4. The ChickWeight dataset contains measurements on 50 chicks fed four different diets over a 21 day period.
##    Last time we did an ANCOVA to compare the weights at the end of the experiment (Time=20).
        ## Hint: add 'at=list(Time=20)'  to the emmeans statement.
##    The results seem overly strong (ie. the p-values seem artificially low). Are all observations independent?
##    What might we not have considered that was part of the experimental design but was not accounted for in our analysis?
##    Construct a more appropriate model (use random effects if justified) and compare the diets.
##    Which diets differ significantly? How do the results differ from your model last week?


## Q5. How much variation is there among chicks? (ie. what is the variance component?)
##     How might this help to explain the difference in emmeans between your old and new model?

## Q6. Check residuals. How do they look? (we will return to this in a couple weeks to fix any potential issues)


## Q7. Bonus- extract the BLUPs and plot a histogram

