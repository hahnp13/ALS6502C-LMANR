## LOAD AND PROCESSES DATA
library(tidyverse)
library(car)
library(glmmTMB)
data("InsectSprays")
InsectSprays$block <- as.factor(rep(c(1,2,3,4,5,6,7,8,9,10,11,12), 6)) # add blocks

d <- InsectSprays %>% filter(spray=='A'|spray=='B'|spray=='C'|spray=='F')

##############################################################
## GRAPH AND ANALYZE DATA USING COMPLETELY RANDOMIZED DESIGN

#plot data by treatment group
ggplot(d, aes(x=spray,y=count)) + geom_boxplot(outlier.shape = NA) + geom_jitter(height=0,width=.1)

#plot data by treatment and block -- note one observation per block
ggplot(d, aes(x=spray,y=count)) + geom_boxplot(outlier.shape = NA) + geom_jitter(height=0,width=.1) + facet_wrap(~block)  #12 blocks


### construct and compare linear models with and without blocking
## no block model
lm1 <- lm(count~spray, data=d) # using lm() so that we can see df; glmmTMB doesn't calculate df's
Anova(lm1)

## block as fixed effect
lm2 <- lm(count~spray+block, data=d)
Anova(lm2)

#####################################################################################
## ANALYZE DATA USING RANDOMIZED COMPLETE BLOCK DESIGN WITH BLOCK AS A RANDOM EFFECT

### NOTE: Can use lme4::lmer instead of glmmTMB. glmmTMB is new and flexible, but can be buggier. 
###       lme4 is more stable and less likely to get errors/warnings, although may still have issues with the version of Matrix or TMB.
###       if you get an error from lm3, try this line of code:
# install.packages('TMB', type = 'source')
# install.packages('lme4', type = 'source')

## block as random effect
lm3 <- glmmTMB(count~spray+(1|block), data=d)
Anova(lm3)

### alterantive is to use lme4::lmer and lmerTest::anova to see ddf's
library(lme4)
library(lmerTest)
lm3a <- lmer(count ~ spray+(1|block), data=d)
anova(lm3a)

#### compare summary() for fixed vs. random blocking effect
summary(lm2)
summary(lm3)

coef(lm3)    ## prints model coefficients

print(VarCorr(lm3), comp=c("Variance"))  ## print variance components from model

plot(resid(lm1) ~ fitted(lm1)) ### check residuals of no block and block models
plot(resid(lm3) ~ fitted(lm3))

hist(resid(lm1))
hist(resid(lm3))

#### compare estimated marginal means
library(emmeans)
emmeans(lm2, pairwise~spray) ## fixed effect block
emmeans(lm3, pairwise~spray) ## random effect block
