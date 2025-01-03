library(tidyverse)
library(emmeans)
library(car)
library(agridat)
library(DHARMa)
library(glmmTMB)

# Gamma example 1 ####
set.seed(15)
var1 <- rgamma(100, shape = 2, scale = .5)
hist(var1, main='mean=1, scale=0.5')

var1[c(2,4,5,19,33,43,56,66,72,77,83,99)] <- 0

# mean = a (shape) * b (rate)
# mean = 2 * .5 = 1
g1 <- glmmTMB(var1 ~ 1, family=Gamma, zi = ~1)

g1 <- glmmTMB(var1 ~ 1, family=ziGamma, zi = ~1)
g1
## mean should be 1 with inverse link function
1/1.018

g1a <- glmmTMB(var1 ~ 1, family=Gamma(link="log"))
g1a
## mean should be 1 with log link function
exp(0.01345)
simulateResiduals(g1a, plot=T)

g1b <- glmmTMB(var1 ~ 1, family=lognormal)
g1b
exp(0.07399)
simulateResiduals(g1b, plot=T)

# Gamma example 2 ####
set.seed(15)
var1 <- rgamma(1000, shape = 4, scale = .25)
hist(var1, main='mean=1, scale=0.25')

# mean = a (shape) * b (rate)
# mean = 4 * .25 = 1.0

g1 <- glmmTMB(var1 ~ 1, family=Gamma(link="inverse"))
g1
## mean should be 1 with inverse link function
1/0.9711

g1a <- glmmTMB(var1 ~ 1, family=Gamma(link="log"))
g1a
## mean should be 1 with log link function
exp(0.02937)



# Gamma example 3 ####
set.seed(15)
var1 <- rgamma(1000, shape = 1, scale = .5)
hist(var1, main='mean=0.5, scale=0.5')

# mean = a (shape) * b (rate)
# mean = 1 * .5 = 0.5

g1 <- glmmTMB(var1 ~ 1, family=Gamma(link="inverse"))
g1
## mean should be 1 with inverse link function
1/2.06

g1a <- glmmTMB(var1 ~ 1, family=Gamma(link="log"))
g1a
## mean should be 1 with log link function
exp(-0.7228)


#  Gamma example 4 ####
set.seed(15)
var1 <- rgamma(1000, shape = .5, scale = 1)
hist(var1, main='mean=0.25, scale=1')

# mean = a (shape) * b (rate)
# mean = .5 * 1 = 0.5

g1 <- glmmTMB(var1 ~ 1, family=Gamma(link="inverse"))
g1
## mean should be 1 with inverse link function
1/2.028

g1a <- glmmTMB(var1 ~ 1, family=Gamma(link="log"))
g1a
## mean should be 1 with log link function
exp(-0.7071)



## Zero inflated Gamma
# Gamma example 1 ####
set.seed(15)
var1 <- rgamma(100, shape = 2, scale = .5)
hist(var1, main='mean=1, scale=0.5')
var1[c(2,4,5,19,33,43,56,66,72,77,83,99)] <- 0 # add some zeros

# mean = a (shape) * b (rate)
# mean = 2 * .5 = 1
g1 <- glmmTMB(var1 ~ 1, family=Gamma, zi = ~1)

g1 <- glmmTMB(var1 ~ 1, family=ziGamma, zi = ~1)
g1
## mean should be 1 with inverse link function
exp(-1.992)

# Running Gamma GLMs ################################

## simulate data for two groups ##############################
set.seed(25)
v1 <- rgamma(100, shape = 3, scale = .5) %>% as.data.frame()
colnames(v1) <- "var"
v1$group <- "one"
head(v1)
v2 <- rgamma(100, shape = 1, scale = .2) %>% as.data.frame()
colnames(v2) <- "var"
v2$group <- "two"
head(v2)

## bind and view dataset
dat1 <- rbind(v1,v2) #mean group 1 = 1.5, group 2 = 0.5
dat1 %>% mutate(obs=rep(1:100,2)) %>% group_by(obs) %>% pivot_wider(names_from = group,values_from = var) %>% 
  ungroup() %>% select(one,two)


#### plot histogram
hist(dat1$var)
ggplot(dat1, aes(x=var)) + geom_histogram(bins=8, fill="grey", color="black") + 
  facet_wrap(~group, scales="free") + theme_bw(base_size = 16)

#### construct model w/ normal distribution
mod0 <- glmmTMB(var ~ group, data=dat1)
plot(simulateResiduals(mod0))
summary(mod0)
emmeans(mod0, ~group, type="response")

#### construct model w/ log-normal distribution
mod0a <- glmmTMB(log(var) ~ group, data=dat1)
plot(simulateResiduals(mod0a))
summary(mod0a)
emmeans(mod0a, ~group, type="response")

#### construct model w/ Gamma distribution and inverse link
mod1 <- glmmTMB(var ~ group, data=dat1, family=Gamma(link = "inverse"))
plot(simulateResiduals(mod1))
summary(mod1)
emmeans(mod1, ~group, type="response")

#### construct model w/ Gamma distribution and log link
mod2 <- glmmTMB(var ~ group, data=dat1, family=Gamma(link = "log"))
plot(simulateResiduals(mod2))
summary(mod2)
emmeans(mod2, ~group, type="response")

AIC(mod0,mod0a, mod0b, mod1,mod2)
# Note: AIC for the log-model is not comparable because the response variable was transformed (and so on a different scale)
