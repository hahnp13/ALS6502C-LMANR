# LOAD AND PROCESS DATA ####
library(tidyverse)   ### load tidyverse
library(car)         ### load car package, which is helpful for analyzing linear models
library(emmeans)     ### load emmeans package, which is helpful for getting means from linear models
library(glmmTMB)
library(broom.mixed) ### INSTALL and load 

## MAY NEED TO REMOVE AND REINSTALL LME4 PACKAGE ####
# remove.packages("lme4")
# install.packages("lme4", type="source")


# ANCOVA ####
#### assemble data with covariate (don't worry about the code...it just makes up some data)##
set.seed(17)
data("InsectSprays")
d <- InsectSprays %>% filter(spray=='A'|spray=='B'|spray=='C'|spray=='F') %>% droplevels()
d$count[13:24] <- d$count[13:24]+5
d$weeds <- abs(round(rnorm(48,2*d$count,10),1))
d$weeds[25:36] <- c(55.3,46.8,30.2,62.3,24.2,33.2,18.2,12.6,39.7,41.0,46.9,42.8)
######################################################################## --

## plot raw data ####
ggplot(d, aes(x=spray,y=count)) + geom_boxplot(outlier.shape = NA) + geom_jitter(height=0,width=.1) 

anova_means <- emmeans(lm(count~spray, data=d), pairwise~spray) 
anova_means

## plot out data with weed cover ####
ggplot(d, aes(x=weeds,y=count)) + geom_point() + facet_wrap(~spray) + geom_smooth(method='lm')


# ANCOVA: multiple intercept model ####

lm1i <- glmmTMB(count ~ spray + weeds, data=d)

Anova(lm1i, type=2) ## Anova table with Type II sums of squares

summary(lm1i) ## model coefficients

## calculate estimated marginal mean for each group (ie. groups means after accounting for the effect of weeds)
ancova_means <- emmeans(lm1i, pairwise~spray)  
ancova_means

## extract the emmean means (ie. group means after accounting for the effect of weeds)
## this code is just for adding features of the model to the graph
lm1i_coef <- as.data.frame(emmeans(lm1i, ~spray))
## extract intercepts and add slopes into new dataframe
lm1i_coef2 <- as.data.frame(emmeans(lm1i, ~spray, at=list(weeds=0)))
lm1i_coef2$slope <- tidy(lm1i)$estimate[5] 


## plot the data with the fitted model
ggplot(data=d, aes(x=weeds,y=count)) + 
  geom_point() + 
  facet_wrap(~spray) + 
  geom_abline(data=lm1i_coef2, aes(intercept=emmean, slope=slope))+
  geom_point(data=lm1i_coef2, aes(x=0,y=emmean),color="red")+
  geom_point(data=lm1i_coef, aes(x=mean(d$weeds),y=emmean),color="blue", size=2)



# ANCOVA: multiple intercept AND slope model ####

lm1is <- glmmTMB(count ~ spray + weeds + spray:weeds, data=d)
## same as above: glmmTMB(count ~ spray * weeds, data=d)

Anova(lm1is, type=2)

summary(lm1is)


## calculate estimated marginal mean for each group (ie. groups means after accounting for the effect of weeds)
ancova_is_means <- emmeans(lm1is, pairwise~spray)  
ancova_is_means

## calculate the slope for each group
ancova_is_slopes <- emtrends(lm1is, pairwise~spray, var="weeds", infer=T)  
ancova_is_slopes

## extract the emmean means (ie. group means after accounting for the effect of weeds)
lm1is_coef <- as.data.frame(emmeans(lm1is, ~spray))

## extract intercepts and add slopes into new dataframe
lm1is_coef2a <- as.data.frame(emmeans(lm1is, ~spray, at=list(weeds=0)))
lm1is_coef2b <- as.data.frame(emtrends(lm1is, var="weeds"))
lm1is_coef2 <- full_join(lm1is_coef2a,lm1is_coef2b,by="spray")

## plot the data with the fitted model
ggplot(data=d, aes(x=weeds,y=count)) + geom_point() + facet_wrap(~spray) + 
  geom_abline(data=lm1is_coef2, aes(intercept=emmean, slope=weeds.trend), lty=2)+
  geom_point(data=lm1is_coef2, aes(x=0,y=emmean),color="orange")+
  geom_point(data=lm1is_coef, aes(x=mean(d$weeds),y=emmean),color="purple", size=2)


# nice plot out data with weed cover ####
ggplot(d, aes(x=weeds,y=count)) + geom_point() + facet_wrap(~spray) + 
  geom_smooth(method='lm', color='black')+theme_bw(base_size = 16)

