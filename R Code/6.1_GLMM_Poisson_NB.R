# GLMMs  ##########################################
library(tidyverse)
library(emmeans)
library(car)
library(agridat)
library(glmmTMB)
library(DHARMa)
library(performance)
library(MuMIn)
library(bbmle)

## Insect sprays example ####
data("InsectSprays")
InsectSprays$block <- as.factor(rep(c(1,2,3,4,5,6,7,8,9,10,11,12), 6)) # add blocks

d <- InsectSprays %>% filter(spray=='A'|spray=='B'|spray=='C'|spray=='F')

glmm1 <- glmmTMB(count ~ spray + (1|block), data=d, family="poisson")
summary(glmm1)
Anova(glmm1)

# Beall webworm data example ####
## Load in and read about the beall.webworms dataset. #### 
## The variables of interest are the y-count of webworms, 
## spray- spray treatment, and lead-lead treatment. Don't worry about the block or other variables for now.

data("beall.webworms")
d1 <- beall.webworms
?beall.webworms  ## info about the beall.webworms dataset
head(d1) ## view data set

## 0. examine plot of data ####
ggplot(d1, aes(x=spray, y=y, fill=lead)) + geom_violin(scale="width", adjust=2) + 
  geom_point(position = position_jitterdodge(jitter.width=.5, jitter.height=.1, dodge.width = 1), alpha=.1)+
  facet_wrap(~block)

## 1. construct poisson and negative binomial models ####
#### Models r1 and r2 were used previously in module 3.3

r1 <- glmmTMB(y ~ spray * lead, data=d1, family="poisson")
r2 <- glmmTMB(y ~ spray * lead, data=d1, family="nbinom2")

## 2. Examine residuals and test for overdispersion
plot(simulateResiduals(r1)) ## DHARMa package simulated residuals
hist(simulateResiduals(r1)) ## histogram should be flat
check_overdispersion(r1) # overdispersion ratio calculator from performance

plot(simulateResiduals(r2)) ## DHARMa package simulated residuals
hist(simulateResiduals(r2)) ## histogram should be flat
check_overdispersion(r2) # overdispersion ratio calculator from performance


# QUESTIONS: What's next? Any aspects of the experimental design missing from the model? ####
####            Construct a model that includes any missing factors.

## 1. construct model (again!!) ####

r3 <- glmmTMB(y ~ spray * lead + (1|block), data=d1, family="nbinom2")   ### nb w/ block

#### Two models (r2 and r3) above differ only random effects. Fixed effects are the same.
#### Can use AIC for selecting the most appropriate random effects.
#### for model selection, use AIC or likihood ratio test
#### Note that I've included the models with the block. Because 'block' was part of the expeirment, it should be in the model no matter what.

## 2. examine residuals ####
plot(simulateResiduals(r3)) ## DHARMa package simulated residuals
hist(simulateResiduals(r3)) ## histogram should be flat
check_overdispersion(r3) # overdispersion ratio calculator from performance

## 3. Check random effects
summary(r3)

library(broom.mixed)
glance(r3)
tidy(r3, effects="ran_pars") ## same as stdev in summary()
r3_blups <- tidy(r3, effects="ran_vals") ## same as stdev in summary()
hist(r3_blups$estimate, breaks=seq(-.8,.8,l=6), main="Hist of RE blups")


### 4. can compare models with and without random effects ####
## If the random effect was part of your experimental design you don't need to do this (just keep block in!)
model.sel(r2,r3) ## from MuMIn
AICtab(r1,r2,r3,base=T,logLik=T,weights=T)    ## from bbmle

## 4. Check significance ####
Anova(r3)

Anova(r2) ## compare with no-block model, similar in this case


## 5. Calculate emmeans ####
  # 6. constrasts
emmeans(r3, pairwise ~ spray:lead, type="response")

## 7. Calculate R2m and R2c
r2(r3)
r.squaredGLMM(r3) ## from MuMIn, lognormal is same as performance

r.squaredGLMM(r2) ## compare to no-block model


#### EXTRA - Look at variance components and block blups
library(broom.mixed)
glance(r4)
r3blups <- tidy(r3, effects="ran_vals")

ggplot(r3blups %>% group_by(level) %>% summarize(blup=mean(estimate),std.error=mean(std.error)) %>% 
         arrange(blup) %>% mutate(Block=factor(level, levels=level)), 
       aes(x=Block, y=blup, ymin=blup-std.error, ymax=blup+std.error))+
  geom_pointrange()+
  geom_hline(aes(yintercept=0), lty="dashed")+
  theme_bw(base_size = 16)+
  ggtitle("block blups")
