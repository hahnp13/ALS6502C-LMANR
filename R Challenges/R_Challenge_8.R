#### R CHALLENGE 8 ####
library(tidyverse)
library(car)
library(lme4)
library(lmerTest)
library(emmeans)
library(patchwork)
library(MuMIn)
library(glmmTMB)
library(agridat)
library(viridis)

wat <- harris.wateruse %>% filter(age=='A1')
head(wat)
str(wat)

?harris.wateruse

ggplot(data=wat, aes(x=day, y=water, color=species))+
  geom_point()+
  scale_color_viridis(discrete = T, end=.8) +
  theme_bw(base_size = 16)

#########################################################################################################
#### QUESTION FOR R CHALLENGE 8 #########################################################################
#### 1. Fit several models, one that accounts for tree ID as a regular random effect and one that accounts 
####     for temporal autocorrelation. Additionally, it seems like there is some non-linearity so try a quadratic term. 
####     Choose the right fixed effects to answer the question: do the species differ in water use and does this change through time?
####     At this point, just fit the models (~4 models) and then proceed to Question 2.

#### 2. Examine the residuals and check for autocorrelation for each of your models.
####    Do they seem ok? If your model has temporal autocorrelation, how strong is it?
####    Use AIC to compare your models. Which is best?


#### 3. Does water use differ between the species at these three time points: 175 days, 225 days, and 275 days?
