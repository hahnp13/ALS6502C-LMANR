library(tidyverse)
library(emmeans)
library(car)
library(agridat)
library(DHARMa)
library(glmmTMB)
library(MuMIn)
library(performance)
library(easystats)

# wheatley carrot infection by carrot fly larvae ####
data("jansen.apple")
?jansen.apple


### load and process data
dat1 <- jansen.apple #%>% filter(insecticide!='nil')
dat1$prop_canker <- dat1$y/dat1$n
dat1$inoculum <- as.factor(dat1$inoculum)
head(dat1) 
hist(dat1$prop_canker)

### plot data
ggplot(dat1, aes(x=gen, y=(y/n), fill=inoculum)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_point(pch=21, size=3, position = position_jitterdodge(jitter.width = .2, jitter.height = 0))+
  theme_bw(base_size = 20)


### construct model
mod1 <- glmmTMB(cbind(y,n-y) ~ inoculum * gen + (1|block), data=dat1, family='binomial')
summary(mod1) # note the block effect is singular -- probably could remove block but we'll keep it in for practice

### examine residuals
simulateResiduals(mod1, plot=T)
check_model(mod1)   # this now uses the package 'easystats'
check_overdispersion(mod1)

## beta-binomial model has "build-in" overdispersion parameter
mod2 <- glmmTMB(cbind(y,n-y) ~ inoculum * gen  + (1|block), data=dat1, family='betabinomial')
summary(mod2) 

## which is better?
AIC(mod1, mod2)

### examine residuals
plot(simulateResiduals(mod2))
check_model(mod2)
check_overdispersion(mod2) ## check_overdispersion can be misleading bc the model already has an OD parameter


### examine summary
summary(mod2)

### Anova 
Anova(mod2)

### examine means
emmeans(mod1, pairwise ~ inoculum, type="response")

### r2
r.squaredGLMM(mod2)
r2(mod2)

