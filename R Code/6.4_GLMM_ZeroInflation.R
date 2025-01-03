library(tidyverse)
library(emmeans)
library(car)
library(agridat)
library(DHARMa)
library(glmmTMB)

##### wheatley carrot infection by carrot fly larvae
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
boot::inv.logit(-4.4000)
boot::inv.logit(-4.4000+4.2831)


### check with AIC
AIC(mod1,mod2,mod3,mod4)
