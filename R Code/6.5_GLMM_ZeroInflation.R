library(tidyverse)
library(emmeans)
library(car)
library(agridat)
library(DHARMa)
library(glmmTMB)
library(performance)

# wheatley carrot infection by carrot fly larvae ####
data("ridout.appleshoots")
?ridout.appleshoots

dat1 <- ridout.appleshoots %>% mutate(photo=as.factor(photo))

hist(dat1$roots)

## poisson model
mod1 <- glmmTMB(roots ~ photo * bap , data=dat1, family='poisson')
plot(simulateResiduals(mod1))

## negative binomial model
mod2 <- glmmTMB(roots ~ photo * bap , data=dat1, family='nbinom2')
plot(simulateResiduals(mod2))

testZeroInflation(simulateResiduals(mod2))
check_zeroinflation(mod2) ## returns the number of zeros expected vs observed

## negative binomial w/ a general intercept for zero inflation (ie. ZI equal for all observations)
mod3 <- glmmTMB(roots ~ photo * bap , data=dat1, family='nbinom2', zi=~1)
plot(simulateResiduals(mod3))
summary(mod3) ## back-transform ZI intercept (exp(p)/(1+exp(p)))


####  what do we expect to cause the zero inflation?
?ridout.appleshoots
ggplot(dat1, aes(x=roots)) + geom_histogram() + facet_grid(bap~photo) + theme_bw(base_size=16)


## negative binomial w/ zero inflation specified by treatment
mod4 <- glmmTMB(roots ~ photo * bap , data=dat1, family='nbinom2', zi=~photo)
summary(mod4)

plot(simulateResiduals(mod4))

## Probability of extra zero?
plogis(-4.4000)        # plogis does inverise logit transformation, so this is the probability of an extra zero when photo=0
plogis(-4.4000+4.2831)

## hurdle model w/ negative binomial
### Note: this model is almost identical to the ZI model above.
mod5 <- glmmTMB(roots ~ photo * bap , data=dat1, family='truncated_nbinom2', zi=~photo)
summary(mod5)

plot(simulateResiduals(mod5))

### check with AIC
AIC(mod1,mod2,mod3,mod4,mod5) # can compare all with AIC because the response is the same


# Troubleshooting -- complete separation ####

## Salamander dataset from glmmTMB package 
ggplot(Salamanders, aes(x=spp, y=count)) + 
  geom_boxplot(outlier.shape=NA) + geom_jitter(width=.15, height=0) + 
  theme_bw(base_size=16)

## some sites have no salamander at all
ggplot(Salamanders, aes(x=spp, y=count)) + 
  geom_boxplot(outlier.shape=NA) + geom_jitter(width=.15, height=0) + 
  facet_wrap(~site) +
  theme_bw(base_size=16)

## fit a zi model
zinbm0 <- glmmTMB(count~spp + (1|site), zi=~spp, Salamanders, family=nbinom2)
simulateResiduals(zinbm0, plot=T)
summary(zinbm0)

diagnose(zinbm0)

## potential solution, remove zi part 
zinbm1 <- glmmTMB(count~spp + (1|site),zi=~1, Salamanders, family=nbinom2)
simulateResiduals(zinbm1, plot=T)
summary(zinbm1)

## potential solution, fit zi part as a random effect
zinbm2 <- glmmTMB(count~spp + (1|site),zi=~(1|spp), Salamanders, family=nbinom2)
simulateResiduals(zinbm2, plot=T)
summary(zinbm2)
