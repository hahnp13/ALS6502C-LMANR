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


## Load in and read about the beall.webworms dataset. #### 
## The variables of interest are the y-count of webworms, 
## spray- spray treatment, and lead-lead treatment. Don't worry about the block or other variables for now.

data("beall.webworms")
d1 <- beall.webworms
?beall.webworms  ## info about the beall.webworms dataset
head(d1) ## view data set

## examine plot of data
ggplot(d1, aes(x=spray, y=y, fill=lead)) + geom_violin(scale="width", adjust=2) + 
  geom_point(position = position_jitterdodge(jitter.width=.5, jitter.height=.1, dodge.width = 1), alpha=.1)+
  facet_wrap(~block)

# implement poisson and negative binomial models ####
#### Models r1 and r2 were used previously in module 3.3

r1 <- glmmTMB(y ~ spray * lead, data=d1, family="poisson")
summary(r1)
Anova(r1)
emmeans(r1, ~spray:lead, type='response') 

### test for overdispersion
check_overdispersion(r1) # overdispersion ratio calculator from performance

#### model using negative binomial
r2 <- glmmTMB(y ~ spray * lead, data=d1, family="nbinom2")
Anova(r2)
emmeans(r2, ~spray:lead, type='response') 

#### simulate residuals for poisson and negative binomial models

plot(simulateResiduals(r1))
hist(simulateResiduals(r1)) ## histogram should be flat

plot(simulateResiduals(r2))
hist(simulateResiduals(r2)) ## histogram should be flat



# QUESTIONS: What's next? Any aspects of the experimental design missing from the model? ####
####            Construct a model that includes any missing factors.


## make individual-level random effect to use for overdispersed poisson ####
d1$obs <- 1:length(d1$y) ## makes a unique number for each row in dataset

r3 <- glmmTMB(y ~ spray * lead + (1|obs), data=d1, family="poisson")     ### overdispersed poisson glm
r4 <- glmmTMB(y ~ spray * lead + (1|block), data=d1, family="nbinom2")   ### nb w/ block
r5 <- glmmTMB(y ~ spray * lead + (1|obs) + (1|block), data=d1, family="poisson")   ### OD poisson w/ block

#### Six models above differ only distribution and random effects. Fixed effects are the same.
#### Can use AIC for selecting the most appropriate distribution and random effects.
#### for model selection, use AIC or likihood ratio test
#### Note that I've included the models with the block. Because 'block' was part of the expeirment, it should be in the model no matter what.
r0 <- glmmTMB(y ~ spray * lead, data=d1) # normal model just for comparison

model.sel(r0,r1,r2,r3,r4,r5) ## from MuMIn
AICtab(r0,r1,r2,r3,r4,r5,base=T,logLik=T,weights=T)    ## from bbmle

#### now you can go on to examine residuals, fixed effects using Anova(), emmeans, etc.
plot(simulateResiduals(r4)) ## says levene's test is significant but the boxes look pretty good
hist(simulateResiduals(r4)) ## histogram should be flat

plot(simulateResiduals(r5)) ## resids look great
hist(simulateResiduals(r5)) ## histogram should be flat

#### Look at variance components and block blups
library(broom.mixed)
glance(r4)
r4blups <- tidy(r4, effects="ran_vals")

ggplot(r4blups, aes(x=level, y=estimate, ymin=estimate-std.error, ymax=estimate+std.error))+
  geom_pointrange()+
  geom_hline(aes(yintercept=0), lty="dashed")+
  theme_bw(base_size = 16)+
  ggtitle("block blups")
